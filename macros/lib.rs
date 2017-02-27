/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

extern crate phf_codegen;
extern crate proc_macro;
#[macro_use] extern crate quote;
extern crate syn;

use std::ascii::AsciiExt;

/// Find a `#[cssparser__assert_ascii_lowercase__data(string = "…", string = "…")]` attribute,
/// and panic if any string contains ASCII uppercase letters.
#[proc_macro_derive(cssparser__assert_ascii_lowercase,
                    attributes(cssparser__assert_ascii_lowercase__data))]
pub fn assert_ascii_lowercase(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input(&input.to_string()).unwrap();
    let data = list_attr(&input, "cssparser__assert_ascii_lowercase__data");

    for sub_attr in data {
        let string = sub_attr_value(sub_attr, "string");
        assert_eq!(*string, string.to_ascii_lowercase(),
                   "the expected strings must be given in ASCII lowercase");
    }

    "".parse().unwrap()
}

/// Find a `#[cssparser__max_len__data(string = "…", string = "…")]` attribute,
/// and emit a `MAX_LENGTH` constant with the length of the longest string.
#[proc_macro_derive(cssparser__max_len,
                    attributes(cssparser__max_len__data))]
pub fn max_len(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input(&input.to_string()).unwrap();
    let data = list_attr(&input, "cssparser__max_len__data");

    let lengths = data.iter().map(|sub_attr| sub_attr_value(sub_attr, "string").len());
    let max_length = lengths.max().expect("expected at least one string");

    let tokens = quote! {
        const MAX_LENGTH: usize = #max_length;
    };

    tokens.as_str().parse().unwrap()
}

/// ```
/// impl $Name {
///     fn map() -> &'static ::phf::Map<&'static str, $ValueType> { … }
/// }
/// ```
///
/// Map keys are ASCII-lowercased.
#[proc_macro_derive(cssparser__phf_map)]
pub fn phf_map(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input(&input.to_string()).unwrap();
    let name = &input.ident;
    let token_trees = find_smuggled_tokens(&input);
    let value_type = &token_trees[0];
    let pairs: Vec<_> = token_trees[1..].chunks(2).map(|chunk| {
        let key = match chunk[0] {
            syn::TokenTree::Token(syn::Token::Literal(syn::Lit::Str(ref string, _))) => string,
            _ => panic!("expected string literal, got {:?}", chunk[0])
        };
        let value = &chunk[1];
        (key.to_ascii_lowercase(), quote!(#value).to_string())
    }).collect();

    let mut map = phf_codegen::Map::new();
    for &(ref key, ref value) in &pairs {
        map.entry(&**key, &**value);
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

/// Return the `…` part in:
///
/// ```rust
/// enum $Name {
///     Input = (0, stringify!(…)).0
/// }
/// ```
fn find_smuggled_tokens(input: &syn::DeriveInput) -> &[syn::TokenTree] {
    let discriminant = match input.body {
        syn::Body::Enum(ref variants) if variants.len() == 1 => &variants[0].discriminant,
        _ => panic!("expected single-variant enum, got {:?}", input.body)
    };
    let tuple = match *discriminant {
        Some(syn::ConstExpr::Other(syn::Expr { node: syn::ExprKind::TupField(ref t, 0), .. })) => t,
        _ => panic!("expected a discriminant like tuple.0, got {:?}", discriminant)
    };
    let expr = match **tuple {
        syn::Expr { node: syn::ExprKind::Tup(ref values), .. } if values.len() == 2 => &values[1],
        _ => panic!("expected non-empty tuple, got {:?}", tuple)
    };
    let macro_args = match *expr {
        syn::Expr { node: syn::ExprKind::Mac(
            syn::Mac { ref tts, path: syn::Path { global: false, ref segments }}
        ),  .. }
        if segments.len() == 1
            && segments[0] == syn::PathSegment::from("stringify")
            && tts.len() == 1
        => &tts[0],
        _ => panic!("expected a stringify!(…) macro, got {:?}", expr)
    };
    match *macro_args {
        syn::TokenTree::Delimited(syn::Delimited { ref tts, delim: syn::DelimToken::Paren }) => tts,
        _ => panic!("expected (…) parentheses, got {:?}", macro_args)
    }
}

/// Panic if the first attribute isn’t `#[foo(…)]` with the given name,
/// or return the parameters.
fn list_attr<'a>(input: &'a syn::DeriveInput, expected_name: &str) -> &'a [syn::NestedMetaItem] {
    for attr in &input.attrs {
        match attr.value {
            syn::MetaItem::List(ref name, ref nested) if name == expected_name => {
                return nested
            }
            _ => {}
        }
    }
    panic!("expected a {} attribute", expected_name)
}

/// Panic if `sub_attr` is not a name-value like `foo = "…"` with the given name,
/// or return the value.
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
