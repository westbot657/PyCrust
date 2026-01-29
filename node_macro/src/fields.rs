use std::collections::VecDeque;
use proc_macro2::{Ident, Span, TokenStream};
use quote::{quote, ToTokens, TokenStreamExt};
use syn::{parenthesized, Attribute, Expr, Field, Fields, GenericArgument, LitStr, PathSegment, Token, Type};
use syn::parse::{Parse, ParseStream};
use crate::data::PatWithGuard;
use crate::PARSE_NAME;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum FieldType {
    VecNode,
    BoxNode,
    Node,
    OptionNode,

    VecToken,
    OptionToken,
    Token,

    OptionUnit,
    Unit,
    Bool,
}

#[derive(Debug)]
pub enum TokenCheck {
    Token(PatWithGuard),
    PassIf(PatWithGuard),
    FailIf(PatWithGuard),
}

#[derive(Debug, Default)]
pub struct FieldAttrs {
    pub skip: bool,
    pub token: Option<TokenCheck>,
    pub sep: Option<SepAttr>,
    pub one_or_more: bool,
    pub commit: bool,
    pub prefix: Vec<TokenCheck>,
    pub postfix: Vec<TokenCheck>,
    pub pattern: Vec<TokenCheck>,
    pub errors: Vec<ErrorArgs>,
}


#[derive(Debug)]
pub enum FieldName {
    Named(Ident),
    Indexed(usize),
}

#[derive(Debug)]
pub struct FieldMetadata {
    name: FieldName,
    concrete_ty: Type,
    ty: FieldType,
    attrs: FieldAttrs,
}

#[derive(Debug)]
pub struct SepAttr {
    pub token: PatWithGuard,
    pub trailing: bool,
}

#[derive(Debug, Clone)]
pub struct ErrorArgs {
    pub format_string: LitStr,
    pub args: Vec<Expr>,
}

impl Parse for ErrorArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let format_string: LitStr = input.parse()?;

        let mut args = Vec::new();

        if input.is_empty() {
            return Ok(Self { format_string, args });
        }

        input.parse::<Token![,]>()?;

        while !input.is_empty() {
            args.push(input.parse::<Expr>()?);

            if input.is_empty() {
                break;
            }

            input.parse::<Token![,]>()?;
        }

        Ok(Self { format_string, args })
    }
}

impl ToTokens for ErrorArgs {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.format_string.to_tokens(tokens);

        for arg in &self.args {
            tokens.extend(quote::quote! { , });
            arg.to_tokens(tokens);
        }
    }
}

pub struct ErrorArgsList {
    pub items: Vec<ErrorArgs>,
}

impl Parse for ErrorArgsList {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut items = Vec::new();

        while !input.is_empty() {
            let ident: Ident = input.parse()?;
            if ident != "err" {
                return Err(syn::Error::new_spanned(
                    ident,
                    "expected `err(...)`",
                ));
            }

            let content;
            parenthesized!(content in input);
            items.push(content.parse::<ErrorArgs>()?);

            if input.is_empty() {
                break;
            }
            input.parse::<Token![,]>()?;
        }

        Ok(Self { items })
    }
}


fn generic_arg_as_type(arg: &GenericArgument) -> Option<&Type> {
    match arg {
        GenericArgument::Type(ty) => Some(ty),
        _ => None,
    }
}
fn is_of_type(ty: &Type, name: &str) -> bool {
    matches!(
        ty,
        Type::Path(tp)
            if tp.qself.is_none()
            && tp.path.segments.last().is_some_and(|s| s.ident == name)
    )
}

impl FieldMetadata {

    pub fn destructure_all<T, A>(
        fields: &mut Fields,
        args: A,
        named: fn(Vec<Self>, A) -> T,
        unnamed: fn(Vec<Self>, A) -> T,
    ) -> syn::Result<T> {
        let mut had_commit = false;
        match fields {
            Fields::Named(fields) => {
                let (destructured, filtered) = fields.named
                    .iter().enumerate()
                    .map(|(i, field)| {
                        FieldMetadata::destructure(i, field, &mut had_commit)
                    })
                    .collect::<syn::Result<(Vec<_>, Vec<_>)>>()?;
                fields.named = filtered
                    .into_iter()
                    .filter_map(|f| f)
                    .collect();

                Ok(named(destructured, args))
            }
            Fields::Unnamed(fields) => {
                let (destructured, filtered) = fields.unnamed
                    .iter().enumerate()
                    .map(|(i, field)| {
                        FieldMetadata::destructure(i, field, &mut had_commit)
                    })
                    .collect::<syn::Result<(Vec<_>, Vec<_>)>>()?;
                fields.unnamed = filtered
                    .into_iter()
                    .filter_map(|f| f)
                    .collect();

                Ok(unnamed(destructured, args))
            }
            Fields::Unit => Err(syn::Error::new(Span::call_site(), "Unit structs cannot be nodes"))
        }
    }

    fn is_token_type(seg: &PathSegment) -> syn::Result<bool> {
        if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
            let inner_ty = args.args.first().ok_or_else(|| syn::Error::new_spanned(seg, "Unsupported type while parsing generic fro Token"))?;
            if is_of_type(generic_arg_as_type(inner_ty).ok_or_else(|| syn::Error::new_spanned(seg, "Unsupported type while checking for Token as generic"))?, "Token") {
                Ok(true)
            } else {
                Ok(false)
            }
        } else {
            Err(syn::Error::new_spanned(seg, "Unsupported type while checking for Token type"))
        }
    }

    fn is_unit_type(seg: &PathSegment) -> syn::Result<bool> {
        if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
            let inner_ty = args.args.first().ok_or_else(|| syn::Error::new_spanned(seg, "Unsupported type while parsing generic for unit"))?;
            Ok(matches!(inner_ty, GenericArgument::Type(Type::Tuple(t)) if t.elems.is_empty()))
        } else {
            Err(syn::Error::new_spanned(seg, "Unsupported type while checking for unit type"))
        }
    }

    fn sub_type(cty: &mut Type, seg: &PathSegment) {
        if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
            let inner_ty = args.args.first().unwrap();
            *cty = generic_arg_as_type(inner_ty).unwrap().clone();
        }
    }

    pub fn destructure(i: usize, field: &Field, had_commit: &mut bool) -> syn::Result<(Self, Option<Field>)> {
        let mut field = field.clone();
        let name = match &field.ident {
            Some(i) => FieldName::Named(i.clone()),
            None => FieldName::Indexed(i),
        };

        let mut cty = field.ty.clone();

        let ty = match &field.ty {
            Type::Path(p) => {
                let seg = p.path.segments.last().ok_or_else(|| syn::Error::new_spanned(p, "Unsupported type while parsing Path"))?;

                match seg.ident.to_string().as_str() {
                    "Vec" => {
                        if Self::is_token_type(seg)? {
                            FieldType::VecToken
                        } else {
                            Self::sub_type(&mut cty, seg);
                            FieldType::VecNode
                        }
                    },
                    "Option" => {
                        if Self::is_token_type(seg)? {
                            FieldType::OptionToken
                        } else if Self::is_unit_type(seg)? {
                            FieldType::OptionUnit
                        } else {
                            Self::sub_type(&mut cty, seg);
                            FieldType::OptionNode
                        }
                    },
                    "Box" => {
                        if Self::is_token_type(seg)? {
                            return Err(syn::Error::new_spanned(seg, "Box<Token> is not allowed"))
                        } else {
                            Self::sub_type(&mut cty, seg);
                            FieldType::BoxNode
                        }
                    },
                    "Token" => FieldType::Token,
                    "bool" => FieldType::Bool,
                    _ => FieldType::Node,
                }
            }
            Type::Tuple(t) if t.elems.is_empty() => FieldType::Unit,
            _ => return Err(syn::Error::new_spanned(field.ty, "Unsupported type for field"))
        };


        let attrs = FieldAttrs::destructure(&mut field, ty, had_commit)?;

        let skip = attrs.skip;
        Ok((
            Self {
                name,
                concrete_ty: cty,
                ty,
                attrs,
            },
            if skip {
                None
            } else {
                Some(field)
            }
        ))

    }
}

impl FieldAttrs {

    fn destructure(field: &mut Field, ty: FieldType, had_commit: &mut bool) -> syn::Result<Self> {
        let mut field_attrs = Self::default();

        let mut error_count = 0;

        if matches!(ty, FieldType::Unit | FieldType::OptionUnit) {
            field_attrs.skip = true;
        }

        field_attrs.commit = *had_commit;

        let attrs = &mut field.attrs;
        
        for attr in attrs.iter() {
            let p = attr.path();
            if p.is_ident("skip") {
                if field_attrs.skip {
                    return Err(syn::Error::new_spanned(p, if matches!(ty, FieldType::Unit | FieldType::OptionUnit) {
                        "Unit types do not need to be explicitly marked as skip"
                    } else {
                        "field is already skipped"
                    }))
                }
                field_attrs.skip = true
            } else if p.is_ident("one_or_more") {
                if matches!(ty, FieldType::VecNode | FieldType::VecToken) {
                    if field_attrs.one_or_more {
                        return Err(syn::Error::new_spanned(p, "Field is already one_or_more"))
                    }
                    field_attrs.one_or_more = true;
                } else {
                    return Err(syn::Error::new_spanned(p, "one_or_more can only be applied to Vec types"))
                }
            } else if p.is_ident("commit") {
                if field_attrs.commit {
                    return Err(syn::Error::new_spanned(p, "commit has already been defined for this field or a previous field"))
                }
                field_attrs.commit = true;
                *had_commit = true;
            } else if p.is_ident("token") {
                if matches!(ty,
                    FieldType::Token
                    | FieldType::VecToken
                    | FieldType::OptionToken
                    | FieldType::Bool
                    | FieldType::Unit
                    | FieldType::OptionUnit
                ) {
                    if field_attrs.token.is_some() {
                        return Err(syn::Error::new_spanned(attr, "token/pass_if/fail_if has already been defined"))
                    } else if !field_attrs.pattern.is_empty() {
                        return Err(syn::Error::new_spanned(attr, "pattern is mutually exclusive with token/pass_if/fail_if"))
                    }
                    field_attrs.token = Some(TokenCheck::Token(attr.parse_args()?));
                } else {
                    return Err(syn::Error::new_spanned(attr, "token is only valid on Token types, bool, and unit"))
                }
            } else if p.is_ident("pass_if") {
                if matches!(ty, FieldType::Unit) {
                    if field_attrs.token.is_some() {
                        return Err(syn::Error::new_spanned(attr, "token/pass_if/fail_if has already been defined"))
                    } else if !field_attrs.pattern.is_empty() {
                        return Err(syn::Error::new_spanned(attr, "pattern is mutually exclusive with token/pass_if/fail_if"))
                    }
                    field_attrs.token = Some(TokenCheck::PassIf(attr.parse_args()?));
                } else {
                    return Err(syn::Error::new_spanned(attr, "pass_if is only valid on Token types, bool, and unit"))
                }
            } else if p.is_ident("fail_if") {
                if matches!(ty, FieldType::Unit) {
                    if field_attrs.token.is_some() {
                        return Err(syn::Error::new_spanned(attr, "token/pass_if/fail_if has already been defined"))
                    } else if !field_attrs.pattern.is_empty() {
                        return Err(syn::Error::new_spanned(attr, "pattern is mutually exclusive with token/pass_if/fail_if"))
                    }
                    field_attrs.token = Some(TokenCheck::FailIf(attr.parse_args()?));
                } else {
                    return Err(syn::Error::new_spanned(attr, "fail_if is only valid on Token types, bool, and unit"))
                }
            } else if p.is_ident("sep") {
                if matches!(ty, FieldType::VecToken | FieldType::VecNode) {
                    if field_attrs.sep.is_some() {
                        return Err(syn::Error::new_spanned(attr, "sep has already been defined"))
                    }
                    field_attrs.sep = Some(attr.parse_args()?)
                } else {
                    return Err(syn::Error::new_spanned(attr, "only Vec types may have sep attr"))
                }
            } else if p.is_ident("prefix") {
                if !field_attrs.postfix.is_empty() {
                    return Err(syn::Error::new_spanned(attr, "prefix has already been defined"))
                }
                let ec;
                (field_attrs.prefix, ec) = TokenCheck::parse_list(attr)?
            } else if p.is_ident("postfix") {
                if !field_attrs.postfix.is_empty() {
                    return Err(syn::Error::new_spanned(attr, "postfix has already been defined"))
                }
                let ec;
                (field_attrs.prefix, ec) = TokenCheck::parse_list(attr)?

            } else if p.is_ident("pattern") {
                if matches!(ty, FieldType::Unit | FieldType::OptionUnit | FieldType::Bool) {
                    if !field_attrs.pattern.is_empty() {
                        return Err(syn::Error::new_spanned(attr, "pattern has already been defined"))
                    } else if field_attrs.token.is_some() {
                        return Err(syn::Error::new_spanned(attr, "pattern is mutually exclusive to token/pass_if/fail_if"))
                    }
                    let ec;
                    (field_attrs.pattern, ec) = TokenCheck::parse_list(attr)?;
                } else {
                    return Err(syn::Error::new_spanned(attr, "pattern is only valid on Vec<Token>, (), Option<()>, and bool"))
                }
            } else if p.is_ident("errors") {
                if !field_attrs.errors.is_empty() {
                    return Err(syn::Error::new_spanned(attr, "errors has already been defined"))
                }
                let errors: ErrorArgsList = attr.parse_args()?;
                field_attrs.errors = errors.items;

            }
        }

        attrs.retain(|attr| {
            !(attr.path().is_ident("skip")
                || attr.path().is_ident("token")
                || attr.path().is_ident("sep")
                || attr.path().is_ident("one_or_more")
                || attr.path().is_ident("commit")
                || attr.path().is_ident("pass_if")
                || attr.path().is_ident("fail_if")
                || attr.path().is_ident("prefix")
                || attr.path().is_ident("postfix")
                || attr.path().is_ident("pattern")
                || attr.path().is_ident("errors")
            )
        });

        if field_attrs.commit {
            for check in &field_attrs.prefix {
                error_count += match check {
                    TokenCheck::Token(_) | TokenCheck::PassIf(_) => 2,
                    _ => 1
                }
            }
            for check in &field_attrs.postfix {
                error_count += match check {
                    TokenCheck::Token(_) | TokenCheck::PassIf(_) => 2,
                    _ => 1
                }
            }
        }

        match ty {
            FieldType::VecNode => {
                if (field_attrs.sep.is_some() || field_attrs.one_or_more) && field_attrs.commit {
                    error_count += 1;
                }
            }
            FieldType::BoxNode => {
                if field_attrs.commit {
                    error_count += 1;
                }
            }
            FieldType::Node => {
                if field_attrs.commit {
                    error_count += 1;
                }
            }
            FieldType::VecToken => {
                if field_attrs.token.is_none() && field_attrs.pattern.is_empty() {
                    return Err(syn::Error::new(Span::call_site(), "Vec<Token> requires #[token(...)] or #[pattern(...)] attribute"))
                }
                if (field_attrs.sep.is_some() || field_attrs.one_or_more) && field_attrs.commit {
                    error_count += 2;
                }
            }
            FieldType::OptionToken => {
                if field_attrs.token.is_none() {
                    return Err(syn::Error::new(Span::call_site(), "Option<Token> requires #[token(...)] attribute"))
                }
            }
            FieldType::Token => {
                if field_attrs.token.is_none() {
                    return Err(syn::Error::new(Span::call_site(), "Token requires #[token(...)] attribute"))
                }
                if field_attrs.commit {
                    error_count += 2;
                }
            }
            FieldType::OptionUnit => {
                if field_attrs.token.is_none() && field_attrs.pattern.is_empty() {
                    return Err(syn::Error::new(Span::call_site(), "Option<()> requires #[token(...)] or #[pattern(...)] attribute"))
                }
            }
            FieldType::Unit => {
                if field_attrs.token.is_none() && field_attrs.pattern.is_empty() {
                    return Err(syn::Error::new(Span::call_site(), "() requires #[token(...)], #[pass_if(...)], #[fail_if(...)], or #[pattern(...)] attribute"))
                }

                if field_attrs.commit {
                    if let Some(check) = &field_attrs.token {
                        error_count += match check {
                            TokenCheck::Token(_) | TokenCheck::PassIf(_) => 2,
                            _ => 1
                        }
                    } else {
                        for p in &field_attrs.pattern {
                            error_count += match p {
                                TokenCheck::Token(_) | TokenCheck::PassIf(_) => 2,
                                _ => 1
                            }
                        }
                    }
                }
            }
            FieldType::Bool => {
                if field_attrs.token.is_none() && field_attrs.pattern.is_empty() {
                    return Err(syn::Error::new(Span::call_site(), "bool requires #[token(...)] or #[pattern(...)] attribute"))
                }
            }
            _ => {}
        }

        if error_count != field_attrs.errors.len() {
            return Err(syn::Error::new_spanned(field, format!("Field requires {error_count} error messages due to existing attributes")))
        }

        Ok(field_attrs)
    }
}


impl Parse for SepAttr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let token: PatWithGuard = input.parse()?;

        let trailing = if input.peek(Token![,]) {
            input.parse::<Token![,]>()?;
            if input.peek(syn::Ident) {
                let ident: Ident = input.parse()?;
                if ident == "trailing" {
                    true
                } else {
                    return Err(syn::Error::new_spanned(ident, "Expected 'trailing'"))
                }
            } else {
                false
            }
        } else {
            false
        };

        Ok(Self { token, trailing })

    }
}

impl TokenCheck {

    fn parse_list(attr: &Attribute) -> syn::Result<(Vec<Self>, usize)> {
        attr.parse_args_with(|input: ParseStream| {
            let mut err_count = 0;
            let mut checks = Vec::new();

            loop {
                if input.is_empty() {
                    break;
                }

                if input.peek(syn::Ident) {
                    let ident: Ident = input.parse()?;

                    if ident == "token" {
                        if !input.peek(syn::token::Paren) {
                            return Err(syn::Error::new_spanned(
                                ident,
                                "expected 'token(...)' with parentheses",
                            ));
                        }
                        let content;
                        parenthesized!(content in input);
                        let expr: PatWithGuard = content.parse()?;

                        checks.push(Self::Token(expr));
                        err_count += 2;
                    } else if ident == "pass_if" {
                        if !input.peek(syn::token::Paren) {
                            return Err(syn::Error::new_spanned(
                                ident,
                                "expected 'pass_if(...)' with parentheses",
                            ));
                        }
                        let content;
                        parenthesized!(content in input);
                        let expr: PatWithGuard = content.parse()?;
                        checks.push(Self::PassIf(expr));
                        err_count += 2;
                    } else if ident == "fail_if" {
                        if !input.peek(syn::token::Paren) {
                            return Err(syn::Error::new_spanned(
                                ident,
                                "expected 'fail_if(...)' with parentheses",
                            ));
                        }
                        let content;
                        parenthesized!(content in input);
                        let expr: PatWithGuard = content.parse()?;
                        checks.push(Self::FailIf(expr));
                        err_count += 1;
                    } else {
                        let msg = format!(
                            "unknown check type '{}', expected one of: token, pass_if, fail_if",
                            ident
                        );
                        return Err(syn::Error::new_spanned(ident, msg));
                    }
                } else {
                    return Err(syn::Error::new(
                        input.span(),
                        "expected 'token(...)', 'pass_if(...)', or 'fail_if(...)'",
                    ));
                }

                if input.peek(Token![,]) {
                    input.parse::<Token![,]>()?;
                } else {
                    break;
                }
            }

            Ok((checks, err_count))
        })
    }

}


impl FieldMetadata {
    pub fn generate_parser(&self, unpack: &mut TokenStream, init: &mut TokenStream, commit: &mut bool) {

        let name = match &self.name {
            FieldName::Named(i) => i.clone(),
            FieldName::Indexed(i) => Ident::new(&format!("tuple_arg_{i}"), Span::call_site())
        };

        if !self.attrs.skip {
            init.append_all(quote! { #name, });
        }

        if self.attrs.commit {
            *commit = true;
        }

        let mut body = TokenStream::new();

        let mut errors: VecDeque<ErrorArgs> = self.attrs.errors.clone().into();

        for rule in &self.attrs.prefix {
            rule.generate_parser(&mut body, *commit, &mut errors);
            body.append_all(quote!(?;));
        }

        let cty = &self.concrete_ty;
        let parse = Ident::new(PARSE_NAME, Span::call_site());

        body.append_all(match self.ty {
            FieldType::VecNode => {
                // one_or_more?, sep?
                if let Some(sep) = &self.attrs.sep {
                    let tk = &sep.token;

                    let node_parse = if sep.trailing {
                        quote! {
                            match #cty::#parse(tokens, invalid_pass) {
                                Result::Ok(Some(n)) => nodes.push(n),
                                Result::Ok(None) => break,
                                Result::Err(e) => {
                                    tokens.restore();
                                    tokens.restore();
                                    return Err(e)
                                }
                            }
                        }
                    } else {
                        quote! {
                            match #cty::#parse(tokens, invalid_pass) {
                                Result::Ok(Some(n)) => nodes.push(n),
                                Result::Ok(None) => {
                                    tokens.restore();
                                    break;
                                },
                                Result::Err(e) => {
                                    tokens.restore();
                                    tokens.restore();
                                    return Err(e)
                                }
                            }
                        }
                    };

                    let else_br = if *commit {
                        let err0 = errors.pop_front().unwrap();
                        quote!(return Err(anyhow!(#err0)))

                    } else {
                        quote!(return Ok(None))
                    };

                    quote! {
                        let mut nodes = Vec::new();
                        tokens.snapshot();
                        match #cty::#parse(tokens, invalid_pass) {
                            Result::Ok(Some(n)) => nodes.push(n),
                            Result::Ok(None) => {
                                tokens.restore();
                                #else_br
                            },
                            Result::Err(e) => {
                                tokens.restore();
                                return Err(e)
                            }
                        }
                        loop {
                            tokens.snapshot();
                            if let Some(tk) = tokens.get(0) {
                                if matches!(&tk.value, #tk) {
                                    tokens.consume_next();
                                } else {
                                    tokens.discard_snapshot();
                                    break
                                }
                            } else {
                                tokens.discard_snapshot();
                                break
                            }

                            #node_parse
                            tokens.discard_snapshot();
                        }
                        tokens.discard_snapshot();
                        Ok(Some(nodes))
                    }
                } else if self.attrs.one_or_more {

                    let else_behavior = if *commit {
                        let err0 = errors.pop_front().unwrap();
                        quote! {
                            return Err(anyhow!(#err0))
                        }
                    } else {
                        quote! {
                            return Ok(None)
                        }
                    };

                    quote! {
                        let mut nodes = Vec::new();
                        tokens.snapshot();
                        match #cty::#parse(tokens, invalid_pass) {
                            Result::Ok(Some(n)) => nodes.push(n),
                            Result::Ok(None) => {
                                tokens.restore();
                                #else_behavior
                            },
                            Result::Err(e) => {
                                tokens.restore();
                                return Err(e)
                            }
                        }
                        loop {
                            match #cty::#parse(tokens, invalid_pass) {
                                Result::Ok(Some(n)) => nodes.push(n),
                                Result::Ok(None) => break,
                                Result::Err(e) => {
                                    tokens.restore();
                                    return Err(e)
                                }
                            }
                        }
                        tokens.discard_snapshot();
                        Ok(Some(nodes))
                    }
                } else {

                    quote! {
                        let mut nodes = Vec::new();
                        tokens.snapshot();
                        loop {
                            match #cty::#parse(tokens, invalid_pass) {
                                Result::Ok(Some(n)) => nodes.push(n),
                                Result::Ok(None) => break,
                                Result::Err(e) => {
                                    tokens.restore();
                                    return Err(e)
                                }
                            }
                        }
                        tokens.discard_snapshot();
                        Ok(Some(nodes))
                    }
                }
            }
            FieldType::BoxNode => {
                if *commit {
                    let err0 = errors.pop_front().unwrap();
                    quote! {
                        match #cty::#parse(tokens, invalid_pass)? {
                            Some(n) => Ok(Some(Box::new(n))),
                            None => Err(anyhow!(#err0))
                        }
                    }
                } else {
                    quote! {
                        match #cty::#parse(tokens, invalid_pass)? {
                            Some(n) => Ok(Some(Box::new(n))),
                            None => Ok(None)
                        }
                    }
                }

            }
            FieldType::Node => {
                if *commit {
                    let err0 = errors.pop_front().unwrap();
                    quote! {
                        match #cty::#parse(tokens, invalid_pass)? {
                            Some(n) => Ok(Some(n)),
                            None => Err(anyhow!(#err0))
                        }
                    }
                } else {
                    quote! {
                        #cty::#parse(tokens, invalid_pass)
                    }
                }
            }
            FieldType::OptionNode => {
                quote! {
                    Ok(Some(#cty::#parse(tokens, invalid_pass)?))
                }
            }
            FieldType::VecToken => {
                // one_or_more?, sep?, token

                let token = match &self.attrs.token {
                    Some(TokenCheck::Token(t)) => t,
                    _ => unreachable!()
                };

                if let Some(sep) = &self.attrs.sep {
                    let tk = &sep.token;

                    let (else_behavior, eof) = if *commit {
                        let err0 = errors.pop_front().unwrap();
                        let err1 = errors.pop_front().unwrap();
                        (
                            quote! {
                                return Err(anyhow!(#err0))
                            },
                            quote! {
                                return Err(anyhow!(#err1))
                            }
                        )
                    } else {
                        (quote!(return Ok(None)), quote!(return Ok(None)))
                    };

                    let trail = if sep.trailing {
                        quote!(tokens.discard_snapshot();)
                    } else {
                        quote!(tokens.restore();)
                    };

                    quote! {
                        let mut toks = Vec::new();
                        if let Some(tk) = tokens.get(0) {
                            if matches!(&tk.value, #token) {
                                toks.push(tokens.consume_next().unwrap().clone())
                            } else {
                                #else_behavior
                            }
                        } else {
                            #eof
                        }
                        loop {
                            tokens.snapshot();
                            if let Some(tk) = tokens.get(0) {
                                if matches!(&tk.value, #tk) {
                                    tokens.consume_next();
                                } else {
                                    tokens.discard_snapshot();
                                    break;
                                }
                            } else {
                                tokens.discard_snapshot();
                                break;
                            }

                            if let Some(tk) = tokens.get(0) {
                                if matches!(&tk.value, #token) {
                                    toks.push(tokens.consume_next().unwrap().clone())
                                } else {
                                    #trail
                                    break
                                }
                            } else {
                                #trail
                                break
                            }
                            tokens.discard_snapshot();
                        }
                        Ok(Some(toks))
                    }
                } else if self.attrs.one_or_more {

                    let (else_behavior, eof) = if *commit {
                        let err0 = errors.pop_front().unwrap();
                        let err1 = errors.pop_front().unwrap();
                        (
                            quote! {
                                return Err(anyhow!(#err0))
                            },
                            quote! {
                                return Err(anyhow!(#err1))
                            }
                        )
                    } else {
                        (quote!(return Ok(None)), quote!(return Ok(None)))
                    };

                    quote! {
                        let mut toks = Vec::new();
                        if let Some(tk) = tokens.get(0) {
                            if matches!(&tk.value, #token) {
                                toks.push(tokens.consume_next().unwrap().clone())
                            } else {
                                #else_behavior
                            }
                        } else {
                            #eof
                        }
                        loop {
                            if let Some(tk) = tokens.get(0) {
                                if matches!(&tk.value, #token) {
                                    toks.push(tokens.consume_next().unwrap().clone())
                                } else {
                                    break;
                                }
                            } else {
                                break;
                            }
                        }
                        Ok(Some(toks))
                    }
                } else {

                    quote! {
                        let mut toks = Vec::new();
                        loop {
                            if let Some(tk) = tokens.get(0) {
                                if matches!(&tk.value, #token) {
                                    toks.push(tokens.consume_next().unwrap().clone())
                                } else {
                                    break;
                                }
                            } else {
                                break;
                            }
                        }
                        Ok(Some(toks))
                    }
                }
            }
            FieldType::OptionToken => {
                // token
                match self.attrs.token.as_ref().unwrap() {
                    TokenCheck::Token(t) => {
                        quote! {
                            if let Some(tk) = tokens.get(0) {
                                if matches!(&tk.value, #t) {
                                    Ok(Some(Some(tokens.consume_next().unwrap().clone())))
                                } else {
                                    Ok(Some(None))
                                }
                            } else {
                                Ok(Some(None))
                            }
                        }
                    }
                    _ => unreachable!()
                }
            }
            FieldType::Token => {
                // token
                match self.attrs.token.as_ref().unwrap() {
                    TokenCheck::Token(t) => {
                        let (a, b) = if *commit {
                            let err0 = errors.pop_front().unwrap();
                            let err1 = errors.pop_front().unwrap();
                            (
                                quote! {Err(anyhow!(#err0))},
                                quote! {Err(anyhow!(#err1))},
                            )
                        } else {
                            (
                                quote! {Ok(None)},
                                quote! {Ok(None)},
                            )
                        };

                        quote! {
                            if let Some(tk) = tokens.get(0) {
                                if matches!(&tk.value, #t) {
                                    Ok(Some(tokens.consume_next().unwrap().clone()))
                                } else {
                                    #a
                                }
                            } else {
                                #b
                            }
                        }
                    }
                    _ => unreachable!()
                }

            }
            FieldType::OptionUnit => {
                // token|pattern
                if let Some(TokenCheck::Token(t)) = &self.attrs.token {

                    quote! {
                        if let Some(tk) = tokens.get(0) {
                            if matches!(&tk.value, #t) {
                                tokens.consume_next();
                            }
                        }
                        Ok(Some(()))
                    }
                } else {

                    let mut checks = TokenStream::new();
                    for check in &self.attrs.pattern {
                        check.generate_parser(&mut checks, false, &mut errors);
                    }

                    quote! {
                        (|| {
                            tokens.snapshot()
                            #checks
                            tokens.discard_snapshot();
                        })();
                    }
                }
            }
            FieldType::Unit => {
                // token|fail_if|pass_if|pattern
                if let Some(check) = &self.attrs.token {

                    let mut out = TokenStream::new();
                    check.generate_parser(&mut out, *commit, &mut errors);
                    out

                } else {
                    let mut checks = TokenStream::new();
                    for check in &self.attrs.pattern {
                        check.generate_parser(&mut checks, false, &mut errors);
                        checks.append_all(quote!(?;));
                    }

                    quote! {
                        (|| {
                            tokens.snapshot();
                            #checks
                            tokens.discard_snapshot();
                            Ok(Some(()))
                        })()
                    }
                }
            }
            FieldType::Bool => {
                // token|pattern
                if let Some(TokenCheck::Token(t)) = &self.attrs.token {

                    quote! {
                        if let Some(tk) = tokens.get(0) {
                            if matches!(&tk.value, #t) {
                                tokens.consume_next();
                                Ok(Some(true))
                            } else {
                                Ok(Some(false))
                            }
                        } else {
                            Ok(Some(false))
                        }
                    }
                } else {

                    let mut checks = TokenStream::new();
                    for check in &self.attrs.pattern {
                        check.generate_parser(&mut checks, false, &mut errors);
                    }

                    quote! {
                        match (|| {
                            tokens.snapshot()
                            #checks
                            tokens.discard_snapshot();
                        })() {
                            Result::Ok(Some(_)) => Ok(true),
                            Result::Ok(None) => Ok(false),
                            Result::Err(e) => Err(e)
                        }
                    }
                }
            }
        });

        for rule in &self.attrs.postfix {
            rule.generate_parser(&mut body, *commit, &mut errors);
            body.append_all(quote!(?;));
        }

        if self.attrs.skip {
            unpack.append_all(quote! {
                match (|| { #body })() {
                    Result::Ok(Some(_)) => {

                    }
                    Result::Ok(None) => {
                        tokens.restore();
                        return Ok(None)
                    }
                    Result::Err(e) => {
                        tokens.restore();
                        return Err(e)
                    }
                }
            })
        } else {
            unpack.append_all(quote! {
                let #name = match (|| { #body })() {
                    Result::Ok(Some(s)) => {
                        s
                    }
                    Result::Ok(None) => {
                        tokens.restore();
                        return Ok(None)
                    }
                    Result::Err(e) => {
                        tokens.restore();
                        return Err(e)
                    }
                };
            })
        }

    }
}

impl TokenCheck {
    fn generate_parser(&self, tokens: &mut TokenStream, commit: bool, errors: &mut VecDeque<ErrorArgs>) {
        tokens.append_all(match self {
            Self::Token(t) => {
                if commit {
                    let err0 = errors.pop_front().unwrap();
                    let err1 = errors.pop_front().unwrap();
                    quote! {
                        if let Some(tk) = tokens.get(0) {
                            if matches!(&tk.value, #t) {
                                Ok(Some(tokens.consume_next().unwrap().clone()))
                            } else {
                                return Err(anyhow!(#err0))
                            }
                        } else {
                            tokens.restore();
                            return Err(anyhow!(#err1))
                        }
                    }
                } else {

                    quote! {
                        if let Some(tk) = tokens.get(0) {
                            if matches!(&tk.value, #t) {
                                Ok(Some(tokens.consume_next().unwrap().clone()))
                            } else {
                                return Ok(None)
                            }
                        } else {
                            tokens.restore();
                            return Ok(None)
                        }
                    }
                }
            }
            Self::PassIf(p) => {
                if commit {

                    let err0 = errors.pop_front().unwrap();
                    let err1 = errors.pop_front().unwrap();
                    quote! {
                        if let Some(tk) = tokens.get(0) {
                            if matches!(&tk.value, #p) {
                                Ok(Some(true))
                            } else {
                                return Err(anyhow!(#err0))
                            }
                        } else {
                            return Err(anyhow!(#err1))
                        }
                    }
                } else {

                    quote! {
                        if let Some(tk) = tokens.get(0) {
                            if matches!(&tk.value, #p) {
                                Ok(Some(()))
                            } else {
                                return Ok(None)
                            }
                        } else {
                            return Ok(None)
                        }
                    }
                }
            }
            Self::FailIf(f) => {
                if commit {
                    let err = errors.pop_front().unwrap();
                    quote! {
                        if let Some(tk) = tokens.get(0) {
                            if matches!(&tk.value, #f) {
                                return Err(anyhow!(#err))
                            } else {
                                Ok(Some(()))
                            }
                        } else {
                            Ok(Some(()))
                        }
                    }
                } else {

                    quote! {
                        if let Some(tk) = tokens.get(0) {
                            if matches!(&tk.value, #f) {
                                return Ok(None)
                            } else {
                                Ok(Some(()))
                            }
                        } else {
                            Ok(Some(()))
                        }
                    }
                }
            }
        })
    }
}

