extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Fields};

#[proc_macro_derive(Node)]
pub fn node_derive(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree
    let input = parse_macro_input!(input as DeriveInput);
    let struct_name = input.ident;

    // Only handle structs for now
    let fields = match input.data {
        Data::Struct(s) => s.fields,
        _ => panic!("Node derive only supports structs"),
    };

    // Collect field names and types
    let mut field_names = Vec::new();
    let mut field_types = Vec::new();

    match fields {
        Fields::Named(named) => {
            for f in named.named {
                let ident = f.ident.unwrap();
                let ty = f.ty;
                field_names.push(ident);
                field_types.push(ty);
            }
        }
        Fields::Unnamed(_) | Fields::Unit => {
            panic!("Node derive only supports named fields for now");
        }
    }

    // Example: generate a dummy `print_fields` function that prints the order and type
    let generated = quote! {
        impl #struct_name {
            pub fn print_fields() {
                #(
                    println!("Field {} has type {:?}", stringify!(#field_names), stringify!(#field_types));
                )*
            }
        }
    };

    generated.into()
}
