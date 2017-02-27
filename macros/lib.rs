/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

extern crate phf_codegen;
extern crate proc_macro;
#[macro_use] extern crate quote;
extern crate syn;

use std::ascii::AsciiExt;

/// Panic if any string contains ASCII uppercase letters.
/// Emit a `MAX_LENGTH` constant with the length of the longest string.
#[proc_macro_derive(cssparser_internal__assert_ascii_lowercase__max_len)]
pub fn assert_ascii_lowercase_max_len(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    max_len_common(input, |token_trees| {
        let tokens = quote!( match x { #( #token_trees )* } );
        let expr = syn::parse_expr(tokens.as_str()).unwrap();
        let arms = match expr {
            syn::Expr { node: syn::ExprKind::Match(_, ref arms), .. } => arms,
            _ => panic!("expected a match expression, got {:?}", expr)
        };
        arms.iter().flat_map(|arm| &arm.pats).filter_map(|pattern| {
            let expr = match *pattern {
                syn::Pat::Lit(ref expr) => expr,
                syn::Pat::Wild |
                syn::Pat::Ident(_, _, None) => return None,
                syn::Pat::Ident(_, _, Some(ref sub_pattern)) => {
                    match **sub_pattern {
                        syn::Pat::Lit(ref expr) => expr,
                        syn::Pat::Wild => return None,
                        _ => panic!("expected string or wildcard pattern, got {:?}", pattern)
                    }
                }
                _ => panic!("expected string or wildcard pattern, got {:?}", pattern)
            };
            match **expr {
                syn::Expr { node: syn::ExprKind::Lit(syn::Lit::Str(ref string, _)), .. } => {
                    assert_eq!(*string, string.to_ascii_lowercase(),
                               "string patterns must be given in ASCII lowercase");
                    Some(string.len())
                }
                _ => panic!("expected string pattern, got {:?}", expr)
            }
        }).max()
    })
}

/// Emit a `MAX_LENGTH` constant with the length of the longest string.
#[proc_macro_derive(cssparser_internal__max_len)]
pub fn max_len(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    max_len_common(input, |token_trees| {
        token_trees.iter().map(|tt| string_literal(tt).len()).max()
    })
}

fn max_len_common<F>(input: proc_macro::TokenStream, f: F) -> proc_macro::TokenStream
where F : FnOnce(&[syn::TokenTree]) -> Option<usize> {
    common(input, |token_trees| {
        let max_length = f(token_trees).expect("expected at least one string");
        quote! {
            const MAX_LENGTH: usize = #max_length;
        }
    })
}

/// ```
/// static MAP: &'static ::phf::Map<&'static str, $ValueType> = …;
/// ```
///
/// Map keys are ASCII-lowercased.
#[proc_macro_derive(cssparser_internal__phf_map)]
pub fn phf_map(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    common(input, |token_trees| {
        let value_type = &token_trees[0];
        let pairs: Vec<_> = token_trees[1..].chunks(2).map(|chunk| {
            let key = string_literal(&chunk[0]);
            let value = &chunk[1];
            (key.to_ascii_lowercase(), quote!(#value).to_string())
        }).collect();

        let mut map = phf_codegen::Map::new();
        for &(ref key, ref value) in &pairs {
            map.entry(&**key, &**value);
        }

        let mut tokens = quote! {
            static MAP: ::phf::Map<&'static str, #value_type> =
        };
        let mut initializer_bytes = Vec::new();
        map.build(&mut initializer_bytes).unwrap();
        tokens.append(::std::str::from_utf8(&initializer_bytes).unwrap());
        tokens.append(";");
        tokens
    })
}

fn common<F>(input: proc_macro::TokenStream, f: F) -> proc_macro::TokenStream
where F: FnOnce(&[syn::TokenTree]) -> quote::Tokens {
    let input = syn::parse_macro_input(&input.to_string()).unwrap();
    let token_trees = find_smuggled_tokens(&input);
    let tokens = f(token_trees);
    tokens.as_str().parse().unwrap()
}

/// Return the `…` part in:
///
/// ```rust
/// enum Somthing {
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

fn string_literal(token: &syn::TokenTree) -> &str {
    match *token {
        syn::TokenTree::Token(syn::Token::Literal(syn::Lit::Str(ref string, _))) => string,
        _ => panic!("expected string literal, got {:?}", token)
    }
}
