use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, ToTokens, TokenStreamExt};
use syn::{parse_macro_input, Data, DeriveInput, Fields, Token, parenthesized, Type};
use syn::parse::{Parse, ParseStream};

#[derive(Debug)]
struct NodeMetadata {
    name: syn::Ident,
    kind: NodeKind,
}

#[derive(Debug)]
enum NodeKind {
    Struct(Vec<FieldMetadata>),
    TupleStruct(Vec<FieldMetadata>),
    Enum(Vec<VariantMetadata>),
}

#[derive(Debug)]
struct FieldMetadata {
    name: FieldName,
    ty: syn::Type,
    attrs: FieldAttrs,
}

#[derive(Debug)]
enum FieldName {
    Named(syn::Ident),
    Indexed(usize),
}

#[derive(Debug)]
enum VariantMetadata {
    Tuple {
        name: syn::Ident,
        fields: Vec<FieldMetadata>,
    },
    Struct {
        name: syn::Ident,
        fields: Vec<FieldMetadata>,
    },
}

#[derive(Debug, Default)]
struct FieldAttrs {
    skip: bool,
    token: Option<syn::Expr>,
    sep: Option<SepAttr>,
    one_or_more: bool,
    commit: bool,
    prefix: Vec<TokenCheck>,
    postfix: Vec<TokenCheck>,
    pattern: Vec<TokenCheck>,
}

#[derive(Debug, Clone)]
enum TokenCheck {
    Token(syn::Expr),
    PassIf(syn::Expr),
    FailIf(syn::Expr),
}

#[derive(Debug)]
struct SepAttr {
    token: syn::Expr,
    trailing: bool,
}

fn generic_arg_as_type(arg: &syn::GenericArgument) -> Option<&syn::Type> {
    match arg {
        syn::GenericArgument::Type(ty) => Some(ty),
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

fn type_to_string(ty: &Type) -> String {
    quote!(#ty).to_string()
}

impl TokenCheck {
    fn generate_impl(&self, hard_fail: bool) -> proc_macro2::TokenStream {
        match self {
            TokenCheck::Token(pattern) => {
                if hard_fail {
                    quote! {
                        let Some(tok) = tokens.get(0) else {
                            return Err(anyhow!("Unexpected EOF"));
                        };
                        if matches!(&tok.value, #pattern) {
                            tokens.consume_next();
                        } else {
                            return Err(anyhow!("Expected token matching {}, but got {:?}", stringify!(#pattern), tok.value));
                        }
                    }
                } else {
                    quote! {
                        let Some(tok) = tokens.get(0) else {
                            return Ok(None);
                        };
                        if matches!(&tok.value, #pattern) {
                            tokens.consume_next();
                        } else {
                            return Ok(None);
                        }
                    }
                }
            }
            TokenCheck::PassIf(pattern) => {
                quote! {
                    let Some(tok) = tokens.get(0) else {
                        return Ok(None);
                    };
                    if !matches!(&tok.value, #pattern) {
                        return Ok(None);
                    }
                }
            }
            TokenCheck::FailIf(pattern) => {
                quote! {
                    if let Some(tok) = tokens.get(0) {
                        if matches!(&tok.value, #pattern) {
                            return Ok(None);
                        }
                    }
                }
            }
        }
    }
}

impl FieldMetadata {
    fn is_storable(&self) -> bool {
        if matches!(self.ty, Type::Tuple(_)) {
            return false;
        }

        if let Type::Path(type_path) = &self.ty {
            if let Some(seg) = type_path.path.segments.last() {
                if seg.ident == "Option" {
                    if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
                        if let Some(inner_arg) = args.args.first() {
                            if let Some(inner_type) = generic_arg_as_type(inner_arg) {
                                if matches!(inner_type, Type::Tuple(t) if t.elems.is_empty()) {
                                    return false;
                                }
                            }
                        }
                    }
                }
            }
        }

        true
    }

    fn generate_impl(&self) -> proc_macro2::TokenStream {
        let ty = &self.ty;
        let commit = self.attrs.commit;

        let mut prefix_code = proc_macro2::TokenStream::new();
        for check in &self.attrs.prefix {
            prefix_code.append_all(check.generate_impl(commit));
        }

        let mut postfix_code = proc_macro2::TokenStream::new();
        for check in &self.attrs.postfix {
            postfix_code.append_all(check.generate_impl(commit));
        }

        let mut pattern_code = proc_macro2::TokenStream::new();
        for check in &self.attrs.pattern {
            pattern_code.append_all(check.generate_impl(commit));
        }

        if let Type::Path(type_path) = ty {
            let seg = type_path.path.segments.last().unwrap();

            match seg.ident.to_string().as_str() {
                "Vec" => {
                    if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
                        let inner_ty = args.args.first().unwrap();
                        let Some(inner_type) = generic_arg_as_type(inner_ty) else {
                            return quote!(compile_error!("Vec must have a type argument"));
                        };

                        if is_of_type(inner_type, "Token") {
                            self.generate_vec_token()
                        } else {
                            self.generate_vec_node(inner_type)
                        }
                    } else {
                        quote! { compile_error!("Vec must have type arguments"); }
                    }
                }
                "Option" => {
                    if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
                        let inner_ty = args.args.first().unwrap();
                        let Some(inner_type) = generic_arg_as_type(inner_ty) else {
                            return quote!(compile_error!("Option must have a type argument"));
                        };

                        if is_of_type(inner_type, "Token") {
                            self.generate_option_token()
                        } else if matches!(inner_type, Type::Tuple(t) if t.elems.is_empty()) {
                            self.generate_option_unit()
                        } else {
                            quote! {
                                #prefix_code
                                let result = #inner_type::parse(tokens, invalid_pass)?;
                                #postfix_code
                                Ok(Some(result))
                            }
                        }
                    } else {
                        quote! { compile_error!("Option must have type arguments"); }
                    }
                }
                "Box" => {
                    if let syn::PathArguments::AngleBracketed(ref args) = seg.arguments {
                        let inner_ty = args.args.first().unwrap();
                        let Some(inner_type) = generic_arg_as_type(inner_ty) else {
                            return quote!(compile_error!("Box must have a type argument"));
                        };

                        let fail_code = if commit {
                            let msg = format!("Failed to parse {}", type_to_string(inner_type));
                            quote! { Err(anyhow!(#msg)) }
                        } else {
                            quote! { Ok(None) }
                        };

                        quote! {
                            #prefix_code
                            let result = if let Some(node) = #inner_type::parse(tokens, invalid_pass)? {
                                Ok(Some(Box::new(node)))
                            } else {
                                #fail_code
                            };
                            #postfix_code
                            result
                        }
                    } else {
                        quote! { compile_error!("Box must have type arguments"); }
                    }
                }
                "Token" => {
                    self.generate_token()
                }
                "bool" => {
                    self.generate_bool()
                }
                _ => {
                    quote! {
                        #prefix_code
                        let result = #seg::parse(tokens, invalid_pass);
                        #postfix_code
                        result
                    }
                }
            }
        } else if let Type::Tuple(tuple_ty) = ty {
            if tuple_ty.elems.is_empty() {
                self.generate_unit()
            } else {
                quote! { compile_error!("Only unit tuples () are supported"); }
            }
        } else {
            let s = format!("Unsupported type: {:?}", ty);
            quote! { compile_error!(#s); }
        }
    }

    fn generate_token(&self) -> proc_macro2::TokenStream {
        let commit = self.attrs.commit;

        if let Some(ref pattern) = self.attrs.token {
            let error_msg = format!("Expected token matching {}", quote!(#pattern));

            if commit {
                quote! {
                    let Some(tok) = tokens.get(0) else {
                        return Err(anyhow!("Unexpected EOF"));
                    };
                    if matches!(&tok.value, #pattern) {
                        let token = tokens.consume_next().unwrap().clone();
                        Ok(Some(token))
                    } else {
                        return Err(anyhow!(#error_msg));
                    }
                }
            } else {
                quote! {
                    let Some(tok) = tokens.get(0) else {
                        return Ok(None);
                    };
                    if matches!(&tok.value, #pattern) {
                        let token = tokens.consume_next().unwrap().clone();
                        Ok(Some(token))
                    } else {
                        return Ok(None);
                    }
                }
            }
        } else {
            quote! { compile_error!("Token field must have #[token(...)] attribute"); }
        }
    }

    fn generate_option_token(&self) -> proc_macro2::TokenStream {
        if let Some(ref pattern) = self.attrs.token {
            quote! {
                let result = if let Some(tok) = tokens.get(0) {
                    if matches!(&tok.value, #pattern) {
                        Some(tokens.consume_next().unwrap().clone())
                    } else {
                        None
                    }
                } else {
                    None
                };
                Ok(Some(result))
            }
        } else {
            quote! { compile_error!("Option<Token> field must have #[token(...)] attribute"); }
        }
    }

    fn generate_vec_token(&self) -> proc_macro2::TokenStream {
        let one_or_more = self.attrs.one_or_more;
        let commit = self.attrs.commit;

        if let Some(ref pattern) = self.attrs.token {
            let mut pattern_code = proc_macro2::TokenStream::new();
            for check in &self.attrs.pattern {
                pattern_code.append_all(check.generate_impl(commit));
            }

            if let Some(ref sep) = self.attrs.sep {
                let sep_pattern = &sep.token;
                let trailing = sep.trailing;

                let trail_behavior = if trailing {
                    quote!(break;)
                } else {
                    quote!(return Err(anyhow!("Unexpected trailing separator"));)
                };
                let oom = if one_or_more {
                    quote! {
                        if items.is_empty() {
                            return Err(anyhow!("Expected at least one element"));
                        }
                    }
                } else {
                    quote!()
                };

                quote! {
                    let mut items = Vec::new();

                    #pattern_code

                    if let Some(tok) = tokens.get(0) {
                        if matches!(&tok.value, #pattern) {
                            items.push(tokens.consume_next().unwrap().clone());
                        } else {
                            return Ok(None);
                        }
                    } else {
                        return Ok(None);
                    }

                    loop {
                        if let Some(tok) = tokens.get(0) {
                            if matches!(&tok.value, #sep_pattern) {
                                tokens.consume_next();

                                if let Some(tok) = tokens.get(0) {
                                    if matches!(&tok.value, #pattern) {
                                        items.push(tokens.consume_next().unwrap().clone());
                                        continue;
                                    }
                                }

                                #trail_behavior
                            } else {
                                break;
                            }
                        } else {
                            break;
                        }
                    }

                    #oom

                    Ok(Some(items))
                }
            } else {
                let oom = if one_or_more {
                    quote! {
                        if items.is_empty() {
                            return Err(anyhow!("Expected at least one element"));
                        }
                    }
                } else {
                    quote!()
                };
                quote! {
                    let mut items = Vec::new();

                    #pattern_code

                    loop {
                        if let Some(tok) = tokens.get(0) {
                            if matches!(&tok.value, #pattern) {
                                items.push(tokens.consume_next().unwrap().clone());
                            } else {
                                break;
                            }
                        } else {
                            break;
                        }
                    }

                    #oom

                    Ok(Some(items))
                }
            }
        } else {
            quote! { compile_error!("Vec<Token> field must have #[token(...)] attribute"); }
        }
    }

    fn generate_vec_node(&self, inner_type: &Type) -> proc_macro2::TokenStream {
        let one_or_more = self.attrs.one_or_more;
        let commit = self.attrs.commit;

        if let Some(ref sep) = self.attrs.sep {
            let sep_pattern = &sep.token;
            let trailing = sep.trailing;

            if commit {
                let trail_behavior = if trailing {
                    quote!(break;)
                } else {
                    quote!(return Err(anyhow!("Expected element after separator"));)
                };
                let oom = if one_or_more {
                    quote!(else { return Err(anyhow!("Expected at least one element")); })
                } else {
                    quote!()
                };
                quote! {
                    {
                        let mut items = Vec::new();

                        if let Some(node) = #inner_type::parse(tokens, invalid_pass)? {
                            items.push(node);

                            loop {
                                let has_separator = if let Some(tok) = tokens.get(0) {
                                    matches!(&tok.value, #sep_pattern)
                                } else {
                                    false
                                };

                                if has_separator {
                                    tokens.consume_next();

                                    if let Some(node) = #inner_type::parse(tokens, invalid_pass)? {
                                        items.push(node);
                                        continue;
                                    } else {
                                        #trail_behavior
                                    }
                                } else {
                                    break;
                                }
                            }
                        } #oom


                        Ok(Some(items))
                    }
                }
            } else {
                let trail_behavior = if trailing {
                    quote!(break;)
                } else {
                    quote!(tokens.restore(); return Ok(None);)
                };
                let oom = if one_or_more {
                    quote! {
                        else {
                            tokens.restore();
                            return Ok(None);
                        }
                    }
                } else {
                    quote!()
                };
                quote! {
                    {
                        let mut items = Vec::new();
                        tokens.snapshot();

                        if let Some(node) = #inner_type::parse(tokens, invalid_pass)? {
                            items.push(node);

                            loop {
                                let has_separator = if let Some(tok) = tokens.get(0) {
                                    matches!(&tok.value, #sep_pattern)
                                } else {
                                    false
                                };

                                if has_separator {
                                    tokens.consume_next();

                                    if let Some(node) = #inner_type::parse(tokens, invalid_pass)? {
                                        items.push(node);
                                        continue;
                                    } else {
                                        #trail_behavior
                                    }
                                } else {
                                    break;
                                }
                            }
                        } #oom

                        tokens.discard_snapshot();
                        Ok(Some(items))
                    }
                }
            }
        } else {
            if commit {
                let oom = if one_or_more {
                    quote! {
                        if items.is_empty() {
                            return Err(anyhow!("Expected at least one element"));
                        }
                    }
                } else {
                    quote!()
                };
                quote! {
                    {
                        let mut items = Vec::new();

                        loop {
                            if let Some(node) = #inner_type::parse(tokens, invalid_pass)? {
                                items.push(node);
                            } else {
                                break;
                            }
                        }

                        #oom

                        Ok(Some(items))
                    }
                }
            } else {
                let oom = if one_or_more {
                    quote! {
                        if items.is_empty() {
                            tokens.restore();
                            return Ok(None);
                        }
                    }
                } else {
                    quote!()
                };
                quote! {
                    {
                        let mut items = Vec::new();
                        tokens.snapshot();

                        loop {
                            if let Some(node) = #inner_type::parse(tokens, invalid_pass)? {
                                items.push(node);
                            } else {
                                break;
                            }
                        }

                        #oom

                        tokens.discard_snapshot();
                        Ok(Some(items))
                    }
                }
            }
        }
    }

    fn generate_bool(&self) -> proc_macro2::TokenStream {
        if let Some(ref pattern) = self.attrs.token {
            quote! {
                let result = if let Some(tok) = tokens.get(0) {
                    if matches!(&tok.value, #pattern) {
                        tokens.consume_next();
                        true
                    } else {
                        false
                    }
                } else {
                    false
                };
                Ok(Some(result))
            }
        } else if !self.attrs.pattern.is_empty() {
            let commit = self.attrs.commit;
            let mut pattern_code = proc_macro2::TokenStream::new();
            for check in &self.attrs.pattern {
                pattern_code.append_all(check.generate_impl(commit));
            }

            quote! {
                tokens.snapshot();
                let result = if { #pattern_code true }.is_ok() {
                    tokens.discard_snapshot();
                    true
                } else {
                    tokens.restore();
                    false
                };
                Ok(Some(result))
            }
        } else {
            quote! { compile_error!("bool field must have #[token(...)] or #[pattern(...)] attribute"); }
        }
    }

    fn generate_unit(&self) -> proc_macro2::TokenStream {
        let commit = self.attrs.commit;

        if !self.attrs.prefix.is_empty() {
            let mut code = proc_macro2::TokenStream::new();
            for check in &self.attrs.prefix {
                code.append_all(check.generate_impl(commit));
            }
            quote! {
                #code
                Ok(Some(()))
            }
        } else if let Some(ref pattern) = self.attrs.token {
            let check = TokenCheck::Token(pattern.clone());
            let code = check.generate_impl(commit);
            quote! {
                #code
                Ok(Some(()))
            }
        } else if !self.attrs.pattern.is_empty() {
            let mut pattern_code = proc_macro2::TokenStream::new();
            for check in &self.attrs.pattern {
                pattern_code.append_all(check.generate_impl(commit));
            }
            quote! {
                #pattern_code
                Ok(Some(()))
            }
        } else {
            quote! { compile_error!("Unit type () field must have #[token(...)], #[pattern(...)], #[prefix(...)] with pass_if/fail_if/token, or direct #[pass_if(...)]/#[fail_if(...)] attribute"); }
        }
    }

    fn generate_option_unit(&self) -> proc_macro2::TokenStream {

        if !self.attrs.prefix.is_empty() {
            let mut code = proc_macro2::TokenStream::new();
            for check in &self.attrs.prefix {
                code.append_all(check.generate_impl(false));
            }
            quote! {
                tokens.snapshot();
                let result = if { #code true }.is_ok() {
                    tokens.discard_snapshot();
                    Some(())
                } else {
                    tokens.restore();
                    None
                };
                Ok(Some(result))
            }
        } else if let Some(ref pattern) = self.attrs.token {
            quote! {
                let result = if let Some(tok) = tokens.get(0) {
                    if matches!(&tok.value, #pattern) {
                        tokens.consume_next();
                        Some(())
                    } else {
                        None
                    }
                } else {
                    None
                };
                Ok(Some(result))
            }
        } else if !self.attrs.pattern.is_empty() {
            let mut pattern_code = proc_macro2::TokenStream::new();
            for check in &self.attrs.pattern {
                pattern_code.append_all(check.generate_impl(false));
            }
            quote! {
                tokens.snapshot();
                let result = if { #pattern_code true }.is_ok() {
                    tokens.discard_snapshot();
                    Some(())
                } else {
                    tokens.restore();
                    None
                };
                Ok(Some(result))
            }
        } else {
            quote! { compile_error!("Option<()> field must have #[token(...)], #[pattern(...)], #[prefix(...)] with pass_if/fail_if/token, or direct #[pass_if(...)]/#[fail_if(...)] attribute"); }
        }
    }
}

impl NodeMetadata {
    fn struct_impl(fields: &Vec<FieldMetadata>, is_tuple: bool) -> proc_macro2::TokenStream {
        let storable_fields: Vec<_> = fields.iter().filter(|f| f.is_storable()).collect();

        let names = storable_fields
            .iter()
            .map(|f| {
                let x = match &f.name {
                    FieldName::Named(n) => n.clone(),
                    FieldName::Indexed(x) => proc_macro2::Ident::new(&format!("parsed_{x}"), Span::call_site()),
                };
                quote! { #x, }
            })
            .collect::<proc_macro2::TokenStream>();

        let mut setup = proc_macro2::TokenStream::new();

        for field in fields {
            let f_name = match &field.name {
                FieldName::Named(n) => n.clone(),
                FieldName::Indexed(x) => proc_macro2::Ident::new(&format!("parsed_{x}"), Span::call_site()),
            };
            let f_impl = field.generate_impl();

            if field.is_storable() {
                setup.append_all(quote! {
                    let Some(#f_name) = { #f_impl }? else { return Ok(None); };
                });
            } else {
                setup.append_all(quote! {
                    let Some(_) = { #f_impl }? else { return Ok(None); };
                });
            }
        }

        if is_tuple {
            quote! {
                #setup
                Ok(Some(Self(#names)))
            }
        } else {
            quote! {
                #setup
                Ok(Some(Self { #names }))
            }
        }
    }

    fn enum_impl(variants: &Vec<VariantMetadata>) -> proc_macro2::TokenStream {
        let mut body = proc_macro2::TokenStream::new();

        for (i, variant) in variants.iter().enumerate() {
            let variant_impl = match variant {
                VariantMetadata::Struct { name, fields } => {
                    let storable_fields: Vec<_> = fields.iter().filter(|f| f.is_storable()).collect();

                    let names = storable_fields
                        .iter()
                        .map(|f| {
                            let x = match &f.name {
                                FieldName::Named(n) => n.clone(),
                                _ => unreachable!(),
                            };
                            quote! { #x, }
                        })
                        .collect::<proc_macro2::TokenStream>();

                    let mut setup = proc_macro2::TokenStream::new();

                    for field in fields {
                        let FieldName::Named(f_name) = &field.name else {
                            unreachable!()
                        };
                        let f_impl = field.generate_impl();

                        if field.is_storable() {
                            setup.append_all(quote! {
                                let Some(#f_name) = { #f_impl }? else { return Ok(None); };
                            });
                        } else {
                            setup.append_all(quote! {
                                let Some(_) = { #f_impl }? else { return Ok(None); };
                            });
                        }
                    }

                    quote! {
                        #setup
                        Ok(Some(Self::#name { #names }))
                    }
                }
                VariantMetadata::Tuple { name, fields } => {
                    let storable_fields: Vec<_> = fields.iter().filter(|f| f.is_storable()).collect();

                    let names = storable_fields
                        .iter()
                        .map(|f| {
                            let x = match &f.name {
                                FieldName::Indexed(x) => proc_macro2::Ident::new(&format!("parsed_{x}"), Span::call_site()),
                                _ => unreachable!(),
                            };
                            quote! { #x, }
                        })
                        .collect::<proc_macro2::TokenStream>();

                    let mut setup = proc_macro2::TokenStream::new();

                    for field in fields {
                        let FieldName::Indexed(x) = &field.name else {
                            unreachable!()
                        };
                        let name = format!("parsed_{x}");
                        let name = proc_macro2::Ident::new(&name, Span::call_site());
                        let f_impl = field.generate_impl();

                        if field.is_storable() {
                            setup.append_all(quote! {
                                let Some(#name) = { #f_impl }? else { return Ok(None); };
                            });
                        } else {
                            setup.append_all(quote! {
                                let Some(_) = { #f_impl }? else { return Ok(None); };
                            });
                        }
                    }

                    quote! {
                        #setup
                        Ok(Some(Self::#name(#names)))
                    }
                }
            };

            body.append_all(quote! {
                if let Some(node) = { #variant_impl }? {
                    Ok(Some(node))
                }
            });

            if i < variants.len() - 1 {
                body.append_all(quote! { else });
            } else {
                body.append_all(quote! { else { Ok(None) } });
            }
        }

        body
    }

    fn get_parse_impl(&self) -> proc_macro2::TokenStream {
        match &self.kind {
            NodeKind::Struct(fields) => Self::struct_impl(fields, false),
            NodeKind::TupleStruct(fields) => Self::struct_impl(fields, true),
            NodeKind::Enum(variants) => Self::enum_impl(variants),
        }
    }
}

#[proc_macro_attribute]
pub fn node(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as DeriveInput);

    let metadata = match parse_node_metadata(&input) {
        Ok(meta) => meta,
        Err(e) => return e.to_compile_error().into(),
    };

    let mut cleaned_input = input.clone();
    strip_node_attrs(&mut cleaned_input);

    let struct_name = &metadata.name;
    let imp = metadata.get_parse_impl();
    let output = quote! {
        #[derive(Debug)]
        #cleaned_input

        impl Node for #struct_name {
            fn parse(tokens: &mut ParseTokens, invalid_pass: bool) -> Result<Option<Self>> {
                #imp
            }
        }
    };

    output.into()
}

fn parse_node_metadata(input: &DeriveInput) -> syn::Result<NodeMetadata> {
    let name = input.ident.clone();

    let kind = match &input.data {
        Data::Struct(data_struct) => match &data_struct.fields {
            Fields::Named(fields) => {
                let field_metas = fields
                    .named
                    .iter()
                    .filter_map(|f| {
                        if is_skip_field(&f.attrs) {
                            None
                        } else {
                            Some(parse_field_metadata(
                                FieldName::Named(f.ident.clone().unwrap()),
                                &f.ty,
                                &f.attrs,
                            ))
                        }
                    })
                    .collect::<syn::Result<Vec<_>>>()?;
                NodeKind::Struct(field_metas)
            }
            Fields::Unnamed(fields) => {
                let field_metas = fields
                    .unnamed
                    .iter()
                    .enumerate()
                    .filter_map(|(idx, f)| {
                        if is_skip_field(&f.attrs) {
                            None
                        } else {
                            Some(parse_field_metadata(
                                FieldName::Indexed(idx),
                                &f.ty,
                                &f.attrs,
                            ))
                        }
                    })
                    .collect::<syn::Result<Vec<_>>>()?;
                NodeKind::TupleStruct(field_metas)
            }
            Fields::Unit => {
                return Err(syn::Error::new_spanned(
                    input,
                    "Unit structs are not supported",
                ));
            }
        },
        Data::Enum(data_enum) => {
            let variants = data_enum
                .variants
                .iter()
                .map(parse_variant_metadata)
                .collect::<syn::Result<Vec<_>>>()?;
            NodeKind::Enum(variants)
        }
        Data::Union(_) => {
            return Err(syn::Error::new_spanned(input, "Unions are not supported"));
        }
    };

    Ok(NodeMetadata { name, kind })
}

fn parse_variant_metadata(variant: &syn::Variant) -> syn::Result<VariantMetadata> {
    let name = variant.ident.clone();

    match &variant.fields {
        Fields::Named(fields) => {
            let field_metas = fields
                .named
                .iter()
                .filter_map(|f| {
                    if is_skip_field(&f.attrs) {
                        None
                    } else {
                        Some(parse_field_metadata(
                            FieldName::Named(f.ident.clone().unwrap()),
                            &f.ty,
                            &f.attrs,
                        ))
                    }
                })
                .collect::<syn::Result<Vec<_>>>()?;

            Ok(VariantMetadata::Struct {
                name,
                fields: field_metas,
            })
        }
        Fields::Unnamed(fields) => {
            let field_metas = fields
                .unnamed
                .iter()
                .enumerate()
                .filter_map(|(idx, f)| {
                    if is_skip_field(&f.attrs) {
                        None
                    } else {
                        Some(parse_field_metadata(
                            FieldName::Indexed(idx),
                            &f.ty,
                            &f.attrs,
                        ))
                    }
                })
                .collect::<syn::Result<Vec<_>>>()?;

            Ok(VariantMetadata::Tuple {
                name,
                fields: field_metas,
            })
        }
        Fields::Unit => Err(syn::Error::new_spanned(
            variant,
            "Unit variants are not supported",
        )),
    }
}

fn is_skip_field(attrs: &[syn::Attribute]) -> bool {
    attrs.iter().any(|attr| attr.path().is_ident("skip"))
}

fn parse_field_metadata(
    name: FieldName,
    ty: &syn::Type,
    attrs: &[syn::Attribute],
) -> syn::Result<FieldMetadata> {
    let mut field_attrs = FieldAttrs::default();

    for attr in attrs {
        if attr.path().is_ident("skip") {
            field_attrs.skip = true;
        } else if attr.path().is_ident("one_or_more") {
            field_attrs.one_or_more = true;
        } else if attr.path().is_ident("commit") || attr.path().is_ident("eager") {
            field_attrs.commit = true;
        } else if attr.path().is_ident("token") {
            let expr: syn::Expr = attr.parse_args()?;
            field_attrs.token = Some(expr);
        } else if attr.path().is_ident("pass_if") {
            let expr: syn::Expr = attr.parse_args()?;
            field_attrs.prefix.push(TokenCheck::PassIf(expr));
        } else if attr.path().is_ident("fail_if") {
            let expr: syn::Expr = attr.parse_args()?;
            field_attrs.prefix.push(TokenCheck::FailIf(expr));
        } else if attr.path().is_ident("sep") {
            field_attrs.sep = Some(parse_sep_attr(attr)?);
        } else if attr.path().is_ident("prefix") {
            field_attrs.prefix = parse_token_check_list(attr)?;
        } else if attr.path().is_ident("postfix") {
            field_attrs.postfix = parse_token_check_list(attr)?;
        } else if attr.path().is_ident("pattern") {
            field_attrs.pattern = parse_token_check_list(attr)?;
        }
    }

    Ok(FieldMetadata {
        name,
        ty: ty.clone(),
        attrs: field_attrs,
    })
}

fn parse_sep_attr(attr: &syn::Attribute) -> syn::Result<SepAttr> {
    attr.parse_args_with(|input: ParseStream| {
        let token: syn::Expr = input.parse()?;

        let trailing = if input.peek(Token![,]) {
            input.parse::<Token![,]>()?;
            if input.peek(syn::Ident) {
                let ident: syn::Ident = input.parse()?;
                if ident == "trailing" {
                    true
                } else {
                    return Err(syn::Error::new_spanned(ident, "Expected 'trailing'"));
                }
            } else {
                false
            }
        } else {
            false
        };

        Ok(SepAttr { token, trailing })
    })
}

fn parse_token_check_list(attr: &syn::Attribute) -> syn::Result<Vec<TokenCheck>> {
    attr.parse_args_with(|input: ParseStream| {
        let mut checks = Vec::new();

        loop {
            if input.is_empty() {
                break;
            }

            if input.peek(syn::Ident) {
                let ident: syn::Ident = input.parse()?;

                if ident == "token" {
                    if !input.peek(syn::token::Paren) {
                        return Err(syn::Error::new_spanned(
                            ident,
                            "expected 'token(...)' with parentheses",
                        ));
                    }
                    let content;
                    parenthesized!(content in input);
                    let expr: syn::Expr = content.parse()?;
                    checks.push(TokenCheck::Token(expr));
                } else if ident == "pass_if" {
                    if !input.peek(syn::token::Paren) {
                        return Err(syn::Error::new_spanned(
                            ident,
                            "expected 'pass_if(...)' with parentheses",
                        ));
                    }
                    let content;
                    parenthesized!(content in input);
                    let expr: syn::Expr = content.parse()?;
                    checks.push(TokenCheck::PassIf(expr));
                } else if ident == "fail_if" {
                    if !input.peek(syn::token::Paren) {
                        return Err(syn::Error::new_spanned(
                            ident,
                            "expected 'fail_if(...)' with parentheses",
                        ));
                    }
                    let content;
                    parenthesized!(content in input);
                    let expr: syn::Expr = content.parse()?;
                    checks.push(TokenCheck::FailIf(expr));
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

fn strip_node_attrs(input: &mut DeriveInput) {
    match &mut input.data {
        Data::Struct(data_struct) => match &mut data_struct.fields {
            Fields::Named(fields) => {
                fields.named = fields
                    .named
                    .iter()
                    .filter_map(|field| {
                        if should_exclude_field(&field.ty) {
                            None
                        } else {
                            let mut field = field.clone();
                            strip_field_attrs(&mut field.attrs);
                            Some(field)
                        }
                    })
                    .collect();
            }
            Fields::Unnamed(fields) => {
                fields.unnamed = fields
                    .unnamed
                    .iter()
                    .filter_map(|field| {
                        if should_exclude_field(&field.ty) {
                            None
                        } else {
                            let mut field = field.clone();
                            strip_field_attrs(&mut field.attrs);
                            Some(field)
                        }
                    })
                    .collect();
            }
            Fields::Unit => {}
        },
        Data::Enum(data_enum) => {
            for variant in data_enum.variants.iter_mut() {
                match &mut variant.fields {
                    Fields::Named(fields) => {
                        fields.named = fields
                            .named
                            .iter()
                            .filter_map(|field| {
                                if should_exclude_field(&field.ty) {
                                    None
                                } else {
                                    let mut field = field.clone();
                                    strip_field_attrs(&mut field.attrs);
                                    Some(field)
                                }
                            })
                            .collect();
                    }
                    Fields::Unnamed(fields) => {
                        fields.unnamed = fields
                            .unnamed
                            .iter()
                            .filter_map(|field| {
                                if should_exclude_field(&field.ty) {
                                    None
                                } else {
                                    let mut field = field.clone();
                                    strip_field_attrs(&mut field.attrs);
                                    Some(field)
                                }
                            })
                            .collect();
                    }
                    Fields::Unit => {}
                }
            }
        }
        Data::Union(_) => {}
    }
}

fn should_exclude_field(ty: &Type) -> bool {
    if matches!(ty, Type::Tuple(t) if t.elems.is_empty()) {
        return true;
    }

    if let Type::Path(type_path) = ty {
        if let Some(seg) = type_path.path.segments.last() {
            if seg.ident == "Option" {
                if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
                    if let Some(inner_arg) = args.args.first() {
                        if let Some(inner_type) = generic_arg_as_type(inner_arg) {
                            if matches!(inner_type, Type::Tuple(t) if t.elems.is_empty()) {
                                return true;
                            }
                        }
                    }
                }
            }
        }
    }

    false
}

fn strip_field_attrs(attrs: &mut Vec<syn::Attribute>) {
    attrs.retain(|attr| {
        !attr.path().is_ident("skip")
            && !attr.path().is_ident("token")
            && !attr.path().is_ident("sep")
            && !attr.path().is_ident("one_or_more")
            && !attr.path().is_ident("commit")
            && !attr.path().is_ident("eager")
            && !attr.path().is_ident("pass_if")
            && !attr.path().is_ident("fail_if")
            && !attr.path().is_ident("prefix")
            && !attr.path().is_ident("postfix")
            && !attr.path().is_ident("pattern")
    });
}