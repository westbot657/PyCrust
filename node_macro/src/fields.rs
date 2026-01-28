use proc_macro2::{Ident, Span, TokenStream};
use quote::{quote, ToTokens, TokenStreamExt};
use syn::{parenthesized, Attribute, Field, Fields, GenericArgument, PathSegment, Token, Type};
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
        match fields {
            Fields::Named(fields) => {
                let (destructured, filtered) = fields.named
                    .iter().enumerate()
                    .map(|(i, field)| {
                        FieldMetadata::destructure(i, field)
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
                        FieldMetadata::destructure(i, field)
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

    pub fn destructure(i: usize, field: &Field) -> syn::Result<(Self, Option<Field>)> {
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


        let attrs = FieldAttrs::destructure(&mut field.attrs, ty)?;

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

    fn destructure(attrs: &mut Vec<Attribute>, ty: FieldType) -> syn::Result<Self> {
        let mut field_attrs = Self::default();

        if matches!(ty, FieldType::Unit | FieldType::OptionUnit) {
            field_attrs.skip = true;
        }

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
                field_attrs.commit = true;
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
                field_attrs.prefix = TokenCheck::parse_list(attr)?
            } else if p.is_ident("postfix") {
                if !field_attrs.postfix.is_empty() {
                    return Err(syn::Error::new_spanned(attr, "postfix has already been defined"))
                }
                field_attrs.prefix = TokenCheck::parse_list(attr)?

            } else if p.is_ident("pattern") {
                if matches!(ty, FieldType::VecToken | FieldType::Unit | FieldType::OptionUnit | FieldType::Bool) {
                    if !field_attrs.pattern.is_empty() {
                        return Err(syn::Error::new_spanned(attr, "pattern has already been defined"))
                    } else if field_attrs.token.is_some() {
                        return Err(syn::Error::new_spanned(attr, "pattern is mutually exclusive to token/pass_if/fail_if"))
                    }
                    field_attrs.pattern = TokenCheck::parse_list(attr)?;
                } else {
                    return Err(syn::Error::new_spanned(attr, "pattern is only valid on Vec<Token>, (), Option<()>, and bool"))
                }
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
            )
        });

        match ty {
            FieldType::VecToken => {
                if field_attrs.token.is_none() && field_attrs.pattern.is_empty() {
                    return Err(syn::Error::new(Span::call_site(), "Vec<Token> requires #[token(...)] or #[pattern(...)] attribute"))
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
            }
            FieldType::Bool => {
                if field_attrs.token.is_none() && field_attrs.pattern.is_empty() {
                    return Err(syn::Error::new(Span::call_site(), "bool requires #[token(...)] or #[pattern(...)] attribute"))
                }
            }
            _ => {}
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
                let ident: syn::Ident = input.parse()?;
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

    fn parse_list(attr: &Attribute) -> syn::Result<Vec<Self>> {
        attr.parse_args_with(|input: ParseStream| {
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

            Ok(checks)
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

        for rule in &self.attrs.prefix {
            rule.generate_parser(&mut body, *commit)
        }

        let cty = &self.concrete_ty;
        let parse = Ident::new(PARSE_NAME, Span::call_site());

        body.append_all(match self.ty {
            FieldType::VecNode => {
                // one_or_more?, sep?
                quote! { todo!("VecNode") }
            }
            FieldType::BoxNode => {
                if *commit {
                    quote! {
                        match #cty::#parse(tokens, invalid_pass)? {
                            Some(n) => Ok(Some(Box::new(n))),
                            None => Err(anyhow!("Failed to parse {}", stringify!(#cty)))
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
                    quote! {
                        match #cty::#parse(tokens, invalid_pass)? {
                            Some(n) => Ok(Some(n)),
                            None => Err(anyhow!("Failed to parse {}", stringify!(#cty)))
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
                // one_or_more?, sep?, token|pattern
                quote! { todo!("VecToken") }
            }
            FieldType::OptionToken => {
                // token
                match self.attrs.token.as_ref().unwrap() {
                    TokenCheck::Token(t) => {
                        quote! {
                            if let Some(tk) = tokens.get(0) {
                                if matches!(tk.value, #t) {
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
                            (
                                quote! {Err(anyhow!("Failed to parse Token"))},
                                quote! {Err(anyhow!("Unexpected EOF while parsing Token"))},
                            )
                        } else {
                            (
                                quote! {Ok(None)},
                                quote! {Ok(None)},
                            )
                        };

                        quote! {
                            if let Some(tk) = tokens.get(0) {
                                if matches!(tk.value, #t) {
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
                            if matches!(tk.value, #t) {
                                tokens.consume_next();
                            }
                        }
                    }
                } else {

                    quote! {

                    }
                }
            }
            FieldType::Unit => {
                // token|fail_if|pass_if|pattern
                quote! { todo!("Unit") }
            }
            FieldType::Bool => {
                // token|pattern
                quote! { todo!("Bool") }
            }
        });

        for rule in &self.attrs.postfix {
            rule.generate_parser(&mut body, *commit)
        }

        if self.attrs.skip {
            unpack.append_all(quote! {
                match (|| -> Result<Option<_>> { #body })() {
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
                let #name = match (|| -> Result<Option<_>> { #body })() {
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
    fn generate_parser(&self, tokens: &mut TokenStream, commit: bool) {
        todo!()
    }
}

