use proc_macro::TokenStream;
use std::any::Any;
use proc_macro2::Span;
use quote::{quote, ToTokens, TokenStreamExt};
use syn::{parse_macro_input, Data, DeriveInput, Fields, Token, parenthesized, Type};
use syn::parse::{Parse, ParseStream};

/// Metadata for the entire node
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
        fields: Vec<FieldMetadata>, // Unnamed fields
    },
    Struct {
        name: syn::Ident,
        fields: Vec<FieldMetadata>, // Named fields, treated as inline #[Node] struct
    },
}

#[derive(Debug, Default)]
struct FieldAttrs {
    skip: bool,
    token: Option<TokenValue>,
    sep: Option<SepAttr>,
    one_or_more: bool,
    commit: bool,
    eager: bool,
    fail_if: Option<TokenValue>,
    pass_if: Option<TokenValue>,
    prefix: Vec<TokenCheck>,
    postfix: Vec<TokenCheck>,
    pattern: Vec<TokenCheck>,
}

#[derive(Debug, Clone)]
enum TokenCheck {
    Token(TokenValue),
    PassIf(TokenValue),
    FailIf(TokenValue),
}

#[derive(Debug)]
struct SepAttr {
    token: TokenValue,
    trailing: bool,
}

#[derive(Debug, Clone)]
struct TokenValue {
    variant: syn::Path,
    inner: Option<syn::Path>,
}

impl Parse for TokenValue {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let variant: syn::Path = input.parse()?;
        let inner = if input.peek(syn::token::Paren) {
            let content;
            parenthesized!(content in input);
            Some(content.parse()?)
        } else {
            None
        };

        Ok(TokenValue { variant, inner })
    }
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

impl FieldMetadata {
    fn generate_impl(&self) -> proc_macro2::TokenStream {
        let ty = &self.ty;
        if let Type::Path(type_path) = ty {
            let seg = type_path.path.segments.last().unwrap();

            match seg.ident.to_string().as_str() {
                "Vec" => {
                    if let syn::PathArguments::AngleBracketed(ref args) = seg.arguments {
                        let inner_ty = args.args.first().unwrap();
                        let Some(name) = generic_arg_as_type(inner_ty) else {
                            return quote!(compile_error("what"))
                        };
                        if is_of_type(name, "Token") {
                            quote! {
                                todo!("Vec<Token> blah"); Ok(None)
                            }
                        } else {
                            quote! {
                                todo!("Vec<{:#?}> branch", stringify!(#name)); Ok(None)
                            }
                        }
                    } else {
                        quote! { todo!("Vec with no generic?"); Ok(None) }
                    }
                }
                "Option" => {
                    if let syn::PathArguments::AngleBracketed(ref args) = seg.arguments {
                        let inner_ty = args.args.first().unwrap();
                        let Some(name) = generic_arg_as_type(inner_ty) else {
                            return quote!(compile_error("what"))
                        };
                        if is_of_type(name, "Token") {
                            quote! {
                                todo!("Option<Token> blah"); Ok(None)
                            }
                        } else {
                            quote! {
                                todo!("Option<{:#?}> branch", stringify!(#name)); Ok(None)
                            }
                        }
                    } else {
                        quote! { todo!("Option with no generic?"); Ok(None) }
                    }
                }
                "Box" => {
                    if let syn::PathArguments::AngleBracketed(ref args) = seg.arguments {
                        let inner_ty = args.args.first().unwrap();
                        let Some(name) = generic_arg_as_type(inner_ty) else {
                            return quote!(compile_error("what"))
                        };
                        quote! {
                            todo!("Box<{:#?}> branch", stringify!(#inner_ty)); Ok(None)
                        }
                    } else {
                        quote! { todo!("Box with no generic?"); Ok(None) }
                    }
                }
                "()" => {
                    quote! { todo!("() unit branch"); Ok(None) }
                }
                "Token" => {
                    quote! { todo!("Token branch"); Ok(None) }
                }
                "bool" => {
                    quote! { todo!("bool branch"); Ok(None) }
                }
                other => {
                    quote! { todo!("Node type branch: {}", #other); Ok(None) }
                }
            }
        } else {
            quote! { todo!("Other type: {:#?}", #ty); Ok(None) }
        }
    }
}

impl NodeMetadata {

    fn struct_impl(fields: &Vec<FieldMetadata>, is_tuple: bool) -> proc_macro2::TokenStream {
        let names = fields
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
            setup.append_all(quote! {
                let Some(#f_name) = { #f_impl }? else { return Ok(None) };
            })
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
                VariantMetadata::Struct {name, fields} => {

                    let names = fields
                        .iter()
                        .map(|f| {
                            let x = match &f.name {
                                FieldName::Named(n) => n.clone(),
                                _ => unreachable!()
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
                        setup.append_all(quote! {
                            let Some(#f_name) = { #f_impl }? else { return Ok(None) };
                        })
                    }

                    quote! {
                        #setup

                        Ok(Some(Self::#name { #names }))
                    }
                }
                VariantMetadata::Tuple {name, fields} => {

                    let names = fields
                        .iter()
                        .map(|f| {
                            let x = match &f.name {
                                FieldName::Indexed(x) => proc_macro2::Ident::new(&format!("parsed_{x}"), Span::call_site()),
                                _ => unreachable!()
                            };
                            quote! { #x , }
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
                        setup.append_all(quote! {
                            let Some(#name) = { #f_impl }? else { return Ok(None) };
                        })
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

            if i < variants.len()-1 {
                body.append_all(quote! { else })
            } else {
                body.append_all(quote! { else { Ok(None) } })
            }
        }

        body
    }

    fn get_parse_impl(&self) -> proc_macro2::TokenStream {

        match &self.kind {
            NodeKind::Struct(fields) => {
                Self::struct_impl(fields, false)
            }
            NodeKind::TupleStruct(fields) => {
                Self::struct_impl(fields, true)
            }
            NodeKind::Enum(variants) => {
                Self::enum_impl(variants)
            }
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
            fn parse(tokens: &mut Tokens) -> anyhow::Result<Option<Self>> {
                #imp
            }
        }
    };

    output.into()
}

fn parse_node_metadata(input: &DeriveInput) -> syn::Result<NodeMetadata> {
    let name = input.ident.clone();

    let kind = match &input.data {
        Data::Struct(data_struct) => {
            match &data_struct.fields {
                Fields::Named(fields) => {
                    let field_metas = fields.named.iter()
                        .filter_map(|f| {
                            if is_skip_field(&f.attrs) {
                                None
                            } else {
                                Some(parse_field_metadata(
                                    FieldName::Named(f.ident.clone().unwrap()),
                                    &f.ty,
                                    &f.attrs
                                ))
                            }
                        })
                        .collect::<syn::Result<Vec<_>>>()?;
                    NodeKind::Struct(field_metas)
                }
                Fields::Unnamed(fields) => {
                    let field_metas = fields.unnamed.iter()
                        .enumerate()
                        .filter_map(|(idx, f)| {
                            if is_skip_field(&f.attrs) {
                                None
                            } else {
                                Some(parse_field_metadata(
                                    FieldName::Indexed(idx),
                                    &f.ty,
                                    &f.attrs
                                ))
                            }
                        })
                        .collect::<syn::Result<Vec<_>>>()?;
                    NodeKind::TupleStruct(field_metas)
                }
                Fields::Unit => {
                    return Err(syn::Error::new_spanned(input, "Unit structs are not supported"));
                }
            }
        }
        Data::Enum(data_enum) => {
            let variants = data_enum.variants.iter()
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
            let field_metas = fields.named.iter()
                .filter_map(|f| {
                    if is_skip_field(&f.attrs) {
                        None
                    } else {
                        Some(parse_field_metadata(
                            FieldName::Named(f.ident.clone().unwrap()),
                            &f.ty,
                            &f.attrs
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
            let field_metas = fields.unnamed.iter()
                .enumerate()
                .filter_map(|(idx, f)| {
                    if is_skip_field(&f.attrs) {
                        None
                    } else {
                        Some(parse_field_metadata(
                            FieldName::Indexed(idx),
                            &f.ty,
                            &f.attrs
                        ))
                    }
                })
                .collect::<syn::Result<Vec<_>>>()?;

            Ok(VariantMetadata::Tuple {
                name,
                fields: field_metas,
            })
        }
        Fields::Unit => {
            Err(syn::Error::new_spanned(
                variant,
                "Unit variants are not supported"
            ))
        }
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
        } else if attr.path().is_ident("commit") {
            field_attrs.commit = true;
        } else if attr.path().is_ident("eager") {
            field_attrs.eager = true;
        } else if attr.path().is_ident("token") {
            let token_val: TokenValue = attr.parse_args()?;
            field_attrs.token = Some(token_val);
        } else if attr.path().is_ident("fail_if") {
            let token_val: TokenValue = attr.parse_args()?;
            field_attrs.fail_if = Some(token_val);
        } else if attr.path().is_ident("pass_if") {
            let token_val: TokenValue = attr.parse_args()?;
            field_attrs.pass_if = Some(token_val);
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
        let token: TokenValue = input.parse()?;

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
                            "expected 'token(...)' with parentheses"
                        ));
                    }
                    let content;
                    parenthesized!(content in input);
                    let token_val: TokenValue = content.parse()?;
                    checks.push(TokenCheck::Token(token_val));
                } else if ident == "pass_if" {
                    if !input.peek(syn::token::Paren) {
                        return Err(syn::Error::new_spanned(
                            ident,
                            "expected 'pass_if(...)' with parentheses"
                        ));
                    }
                    let content;
                    parenthesized!(content in input);
                    let token_val: TokenValue = content.parse()?;
                    checks.push(TokenCheck::PassIf(token_val));
                } else if ident == "fail_if" {
                    if !input.peek(syn::token::Paren) {
                        return Err(syn::Error::new_spanned(
                            ident,
                            "expected 'fail_if(...)' with parentheses"
                        ));
                    }
                    let content;
                    parenthesized!(content in input);
                    let token_val: TokenValue = content.parse()?;
                    checks.push(TokenCheck::FailIf(token_val));
                } else {
                    let msg = format!("unknown check type '{}', expected one of: token, pass_if, fail_if", ident);
                    return Err(syn::Error::new_spanned(
                        ident,
                        msg
                    ));
                }
            } else {
                return Err(syn::Error::new(
                    input.span(),
                    "expected 'token(...)', 'pass_if(...)', or 'fail_if(...)'"
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
        Data::Struct(data_struct) => {
            match &mut data_struct.fields {
                Fields::Named(fields) => {
                    for field in fields.named.iter_mut() {
                        strip_field_attrs(&mut field.attrs);
                    }
                }
                Fields::Unnamed(fields) => {
                    for field in fields.unnamed.iter_mut() {
                        strip_field_attrs(&mut field.attrs);
                    }
                }
                Fields::Unit => {}
            }
        }
        Data::Enum(data_enum) => {
            for variant in data_enum.variants.iter_mut() {
                match &mut variant.fields {
                    Fields::Named(fields) => {
                        for field in fields.named.iter_mut() {
                            strip_field_attrs(&mut field.attrs);
                        }
                    }
                    Fields::Unnamed(fields) => {
                        for field in fields.unnamed.iter_mut() {
                            strip_field_attrs(&mut field.attrs);
                        }
                    }
                    Fields::Unit => {}
                }
            }
        }
        Data::Union(_) => {}
    }
}

fn strip_field_attrs(attrs: &mut Vec<syn::Attribute>) {
    attrs.retain(|attr| {
        !attr.path().is_ident("skip")
            && !attr.path().is_ident("token")
            && !attr.path().is_ident("sep")
            && !attr.path().is_ident("one_or_more")
            && !attr.path().is_ident("commit")
            && !attr.path().is_ident("eager")
            && !attr.path().is_ident("fail_if")
            && !attr.path().is_ident("pass_if")
            && !attr.path().is_ident("prefix")
            && !attr.path().is_ident("postfix")
            && !attr.path().is_ident("pattern")
    });
}