/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

extern crate proc_macro;

use proc_macro::TokenStream;

/// Implementation detail of the `match_ignore_ascii_case!` macro
#[allow(non_snake_case)]
#[proc_macro]
pub fn cssparser_internal__match_ignore_ascii_case__support(input: TokenStream) -> TokenStream {
    struct Input {
        max_length: usize,
    }

    impl syn::parse::Parse for Input {
        fn parse(input: syn::parse::ParseStream) -> syn::parse::Result<Self> {
            let mut max_length = 0;
            while !input.is_empty() {
                if input.peek(syn::Token![_]) {
                    input.parse::<syn::Token![_]>().unwrap();
                    continue;
                }
                let lit: syn::LitStr = input.parse()?;
                let value = lit.value();
                if value.to_ascii_lowercase() != value {
                    return Err(syn::Error::new(lit.span(), "must be ASCII-lowercase"));
                }
                max_length = max_length.max(value.len());
            }
            Ok(Input { max_length })
        }
    }

    let Input { max_length } = syn::parse_macro_input!(input);
    quote::quote!(
        pub(super) const MAX_LENGTH: usize = #max_length;
    )
    .into()
}

/// Implementation detail of the `ascii_case_insensitive_phf_map!` macro
#[allow(non_snake_case)]
#[proc_macro]
pub fn cssparser_internal__ascii_case_insensitive_phf_map__support(
    input: TokenStream,
) -> TokenStream {
    struct Input {
        value_type: syn::Type,
        max_key_length: usize,
        keys: Vec<syn::LitStr>,
        values: Vec<syn::Expr>,
    }

    impl syn::parse::Parse for Input {
        fn parse(input: syn::parse::ParseStream) -> syn::parse::Result<Self> {
            let value_type = input.parse()?;
            let mut max_key_length = 0;
            let mut keys = Vec::new();
            let mut values = Vec::new();
            while !input.is_empty() {
                let key: syn::LitStr = input.parse()?;
                let key_value = key.value();
                max_key_length = max_key_length.max(key_value.len());
                keys.push(syn::LitStr::new(
                    &key_value.to_ascii_lowercase(),
                    key.span(),
                ));
                values.push(input.parse()?);
            }
            Ok(Input {
                value_type,
                max_key_length,
                keys,
                values,
            })
        }
    }

    let Input {
        value_type,
        max_key_length,
        keys,
        values,
    } = syn::parse_macro_input!(input);
    quote::quote!(
        pub(super) const MAX_LENGTH: usize = #max_key_length;
        pub(super) static MAP: Map<&'static str, #value_type> = phf_map! {
            #(
                #keys => #values,
            )*
        };
    )
    .into()
}
