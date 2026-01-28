use proc_macro2::TokenStream;
use quote::{quote, ToTokens, TokenStreamExt};
use syn::parse::{Parse, ParseStream};
use syn::{Data, DeriveInput, Token};
use syn::__private::Span;
use crate::enums::EnumMetadata;
use crate::structs::StructMetadata;

pub enum Node {
    Enum(EnumMetadata, DeriveInput),
    Struct(StructMetadata, DeriveInput)
}

impl Parse for Node {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut structured: DeriveInput = input.parse()?;
        match &mut structured.data {
            Data::Struct(data_struct) => {
                let name = structured.ident.clone();
                Ok(Self::Struct(StructMetadata::destructure(name, data_struct)?, structured))
            }
            Data::Enum(data_enum) => {
                let name = structured.ident.clone();
                Ok(Self::Enum(EnumMetadata::destructure(name, data_enum)?, structured))
            }
            Data::Union(_) => Err(syn::Error::new(Span::call_site(), "Unions cannot be nodes"))
        }
    }
}


#[derive(Debug, Clone)]
pub struct PatWithGuard {
    pub pat: syn::Pat,
    pub guard: Option<syn::Expr>,
}

impl Parse for PatWithGuard {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let pat = syn::Pat::parse_multi(input)?;

        let guard = if input.peek(Token![if]) {
            input.parse::<Token![if]>()?;
            Some(input.parse::<syn::Expr>()?)
        } else {
            None
        };

        Ok(PatWithGuard {
            pat,
            guard,
        })
    }
}

impl ToTokens for PatWithGuard {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.pat.to_tokens(tokens);
        if let Some(guard) = &self.guard {
            tokens.append_all(quote!(if));
            guard.to_tokens(tokens);
        }
    }
}

pub struct IterativeNodeArgs {
    pub rule: syn::Type,
    pub default: syn::Ident,
    pub cases: Vec<IterativeCase>,
}

pub struct IterativeCase {
    pub variant: syn::Ident,
    pub value: PatWithGuard,
}

impl Parse for IterativeNodeArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let rule: syn::Type = input.parse()?;
        input.parse::<Token![,]>()?;

        let default: syn::Ident = input.parse()?;
        input.parse::<Token![,]>()?;

        let mut cases = Vec::new();

        while !input.is_empty() {
            let variant: syn::Ident = input.parse()?;
            input.parse::<Token![:]>()?;
            let value: PatWithGuard = input.parse()?;

            cases.push(IterativeCase { variant, value });

            if input.peek(Token![,]) {
                input.parse::<Token![,]>()?;
            } else {
                break;
            }
        }

        Ok(Self {
            rule,
            default,
            cases,
        })
    }
}

impl ToTokens for Node {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append_all(quote! {
            #[derive(Debug)]
        });
        match self {
            Self::Enum(enum_meta, cleaned) => {
                cleaned.to_tokens(tokens);
                enum_meta.to_tokens(tokens);
            }
            Self::Struct(struct_meta, cleaned) => {
                cleaned.to_tokens(tokens);
                struct_meta.to_tokens(tokens);
            }
        }
    }
}



