/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

extern crate proc_macro;
#[macro_use] extern crate quote;
extern crate syn;

use std::ascii::AsciiExt;

#[proc_macro_derive(cssparser__match_ignore_ascii_case__derive,
                    attributes(cssparser__match_ignore_ascii_case__data))]
pub fn expand_token_stream(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input(&input.to_string()).unwrap();

    let max_length;

    match input.attrs[0].value {
        syn::MetaItem::List(ref ident, ref nested)
        if ident == "cssparser__match_ignore_ascii_case__data" => {
            let lengths = nested.iter().map(|sub_attr| match *sub_attr {
                syn::NestedMetaItem::MetaItem(
                    syn::MetaItem::NameValue(ref ident, syn::Lit::Str(ref string, _))
                )
                if ident == "string" => {
                    assert_eq!(*string, string.to_ascii_lowercase(),
                               "the expected strings must be given in ASCII lowercase");
                    string.len()
                }
                _ => {
                    panic!("expected a `string = \"…\" parameter to the attribute, got {:?}", sub_attr)
                }
            });

            max_length = lengths.max().expect("expected at least one string")
        }
        _ => {
            panic!("expected a cssparser_match_ignore_ascii_case_data attribute")
        }
    }

    let tokens = quote! {
        const MAX_LENGTH: usize = #max_length;
    };

    tokens.as_str().parse().unwrap()
}
