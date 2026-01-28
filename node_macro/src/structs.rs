use proc_macro2::TokenStream;
use quote::{quote, ToTokens, TokenStreamExt};
use syn::DataStruct;
use crate::fields::FieldMetadata;

#[derive(Debug)]
pub enum StructType {
    Tuple,
    Struct,
}

#[derive(Debug)]
pub struct StructMetadata {
    pub ty: StructType,
    pub name: syn::Ident,
    pub fields: Vec<FieldMetadata>
}

impl StructMetadata {

    pub fn destructure(name: syn::Ident, data_struct: &mut DataStruct) -> syn::Result<Self> {
        FieldMetadata::destructure_all(
            &mut data_struct.fields,
            name,
            |fields, name| {
                Self {
                    ty: StructType::Struct,
                    name,
                    fields,
                }
            },
            |fields, name| {
                Self {
                    ty: StructType::Tuple,
                    name,
                    fields,
                }
            }
        )
        
    }
}


impl ToTokens for StructMetadata {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        
        let mut unpack = TokenStream::new();
        let mut init = TokenStream::new();
        
        let mut commit = false;
        
        for field in &self.fields {
            field.generate_parser(&mut unpack, &mut init, &mut commit);
        }
        
        let init = match self.ty {
            StructType::Struct => quote! { { #init } },
            StructType::Tuple => quote! { ( #init ) }
        };
        
        let name = &self.name;
        tokens.append_all(quote! {
            impl Node for #name {
                fn parse(tokens: &mut ParseTokens, invalid_pass: bool) -> Result<Option<Self>> {
                    tokens.snapshot();
                    #unpack
                    tokens.discard_snapshot();
                    Ok(Some(Self #init))
                }
            }
        })
    }
}
