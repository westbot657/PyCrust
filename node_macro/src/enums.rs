use proc_macro2::TokenStream;
use quote::{quote, ToTokens, TokenStreamExt};
use syn::{DataEnum, Variant};
use crate::fields::FieldMetadata;

#[derive(Debug, Clone, Copy)]
pub enum EnumVariantType {
    Struct,
    Tuple,
}

#[derive(Debug)]
pub struct EnumVariantMetadata {
    ty: EnumVariantType,
    name: syn::Ident,
    fields: Vec<FieldMetadata>
}

#[derive(Debug)]
pub struct EnumMetadata {
    name: syn::Ident,
    variants: Vec<EnumVariantMetadata>
}

impl EnumMetadata {

    pub fn destructure(name: syn::Ident, data_enum: &mut DataEnum) -> syn::Result<Self> {
        let variants = data_enum.variants
            .iter_mut()
            .map(EnumVariantMetadata::destructure)
            .collect::<syn::Result<_>>()?;

        Ok(Self {
            name,
            variants
        })
    }

}

impl EnumVariantMetadata {
    pub fn destructure(variant: &mut Variant) -> syn::Result<Self> {
        let var_name = variant.ident.clone();
        FieldMetadata::destructure_all(
            &mut variant.fields,
            var_name,
            |fields, name| {
                Self {
                    ty: EnumVariantType::Struct,
                    name,
                    fields
                }
            },
            |fields, name| {
                Self {
                    ty: EnumVariantType::Tuple,
                    name,
                    fields
                }
            }
        )
    }
}


impl ToTokens for EnumMetadata {
    fn to_tokens(&self, tokens: &mut TokenStream) {

        let name = &self.name;
        let mut variants = TokenStream::new();

        for variant in &self.variants {
            variant.to_tokens(&mut variants);
        }

        tokens.append_all(quote! {
            impl Node for #name {
                fn parse(tokens: &mut ParseTokens, invalid_pass: bool) -> Result<Option<Self>> {
                    tokens.snapshot();
                    #variants
                    tokens.restore();
                    Ok(None)
                }
            }
        })
    }
}

impl ToTokens for EnumVariantMetadata {
    fn to_tokens(&self, tokens: &mut TokenStream) {

        let mut unpack = TokenStream::new();
        let mut init = TokenStream::new();

        let mut commit = false;
        for field in &self.fields {
            field.generate_parser(&mut unpack, &mut init, &mut commit);
        }

        let init = match self.ty {
            EnumVariantType::Struct => quote! { { #init } },
            EnumVariantType::Tuple => quote! { ( #init ) }
        };

        let name = &self.name;
        tokens.append_all(quote! {
            match (|| -> Result<Option<Self>> { #unpack Ok(Some(Self::#name #init)) })() {
                Result::Ok(Some(s)) => {
                    tokens.discard_snapshot();
                    return Ok(Some(s))
                }
                Result::Ok(None) => {
                    // tokens.restore();
                    tokens.snapshot();
                }
                Result::Err(e) => {
                    // tokens.restore();
                    return Err(e)
                }
            }
        })

    }
}