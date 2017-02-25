/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

extern crate phf_codegen;
extern crate proc_macro;
#[macro_use] extern crate quote;
extern crate syn;

use std::ascii::AsciiExt;

#[proc_macro_derive(cssparser__match_ignore_ascii_case__max_len,
                    attributes(cssparser__match_ignore_ascii_case__data))]
pub fn max_len(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input(&input.to_string()).unwrap();
    let data = list_attr(&input, "cssparser__match_ignore_ascii_case__data");

    let lengths = data.iter().map(|sub_attr| {
        let string = sub_attr_value(sub_attr, "string");
        assert_eq!(*string, string.to_ascii_lowercase(),
                   "the expected strings must be given in ASCII lowercase");
        string.len()
    });
    let max_length = lengths.max().expect("expected at least one string");

    let tokens = quote! {
        const MAX_LENGTH: usize = #max_length;
    };

    tokens.as_str().parse().unwrap()
}


#[proc_macro_derive(cssparser__phf_map,
                    attributes(cssparser__phf_map__kv_pairs))]
pub fn phf_map(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input(&input.to_string()).unwrap();
    let name = &input.ident;
    let value_type = match input.body {
        syn::Body::Struct(syn::VariantData::Tuple(ref fields)) if fields.len() == 1 => {
            &fields[0].ty
        }
        _ => panic!("expected tuple struct newtype, got {:?}", input.body)
    };

    let kv_pairs = list_attr(&input, "cssparser__phf_map__kv_pairs");

    let mut map = phf_codegen::Map::new();
    for chunk in kv_pairs.chunks(2) {
        let key = sub_attr_value(&chunk[0], "key");
        let value = sub_attr_value(&chunk[1], "value");
        map.entry(key, value);
    }

    let mut initializer_bytes = Vec::<u8>::new();
    let mut initializer_tokens = quote::Tokens::new();
    map.build(&mut initializer_bytes).unwrap();
    initializer_tokens.append(::std::str::from_utf8(&initializer_bytes).unwrap());

    let tokens = quote! {
        impl #name {
            #[inline]
            fn map() -> &'static ::phf::Map<&'static str, #value_type> {
                static MAP: ::phf::Map<&'static str, #value_type> = #initializer_tokens;
                &MAP
            }
        }
    };

    tokens.as_str().parse().unwrap()
}

fn list_attr<'a>(input: &'a syn::DeriveInput, expected_name: &str) -> &'a [syn::NestedMetaItem] {
    match input.attrs[0].value {
        syn::MetaItem::List(ref name, ref nested) if name == expected_name => {
            nested
        }
        _ => {
            panic!("expected a {} attribute", expected_name)
        }
    }
}

fn sub_attr_value<'a>(sub_attr: &'a syn::NestedMetaItem, expected_name: &str) -> &'a str {
    match *sub_attr {
        syn::NestedMetaItem::MetaItem(
            syn::MetaItem::NameValue(ref name, syn::Lit::Str(ref value, _))
        )
        if name == expected_name => {
            value
        }
        _ => {
            panic!("expected a `{} = \"…\"` parameter to the attribute, got {:?}",
                   expected_name, sub_attr)
        }
    }
}
