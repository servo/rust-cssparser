/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

extern crate proc_macro;

use proc_macro::TokenStream;

/// Input: a `match` expression.
///
/// Output: a `MAX_LENGTH` constant with the length of the longest string pattern.
///
/// Panic if the arms contain non-string patterns,
/// or string patterns that contains ASCII uppercase letters.
#[allow(non_snake_case)]
#[proc_macro]
pub fn cssparser_internal__assert_ascii_lowercase__max_len(input: TokenStream) -> TokenStream {
    let expr: syn::ExprMatch = syn::parse_macro_input!(input);
    let strings = expr
        .arms
        .iter()
        .flat_map(|arm| match arm.pat {
            syn::Pat::Or(ref p) => p.cases.iter().collect(),
            ref p => vec![p],
        })
        .filter_map(|pattern| {
            let expr = match pattern {
                syn::Pat::Lit(expr) => expr,
                syn::Pat::Wild(_) => return None,
                _ => panic!("expected string or wildcard pattern, got {:?}", pattern),
            };
            match *expr.expr {
                syn::Expr::Lit(syn::ExprLit {
                    lit: syn::Lit::Str(ref lit),
                    ..
                }) => {
                    assert_eq!(
                        lit.value(),
                        lit.value().to_ascii_lowercase(),
                        "string patterns must be given in ASCII lowercase"
                    );
                    Some(lit)
                }
                _ => panic!("expected string pattern, got {:?}", expr),
            }
        });
    max_len(strings)
}

/// Input: string literals with no separator
///
/// Output: a `MAX_LENGTH` constant with the length of the longest string.
#[allow(non_snake_case)]
#[proc_macro]
pub fn cssparser_internal__max_len(input: TokenStream) -> TokenStream {
    struct Input(Vec<syn::LitStr>);

    impl syn::parse::Parse for Input {
        fn parse(input: syn::parse::ParseStream) -> syn::parse::Result<Self> {
            let mut strings = Vec::new();
            while !input.is_empty() {
                strings.push(input.parse()?)
            }
            Ok(Self(strings))
        }
    }

    let strings: Input = syn::parse_macro_input!(input);
    max_len(strings.0.iter())
}

fn max_len<'a, I: Iterator<Item = &'a syn::LitStr>>(strings: I) -> TokenStream {
    let max_length = strings
        .map(|s| s.value().len())
        .max()
        .expect("expected at least one string");
    quote::quote!( pub(super) const MAX_LENGTH: usize = #max_length; ).into()
}

/// Input: A type, followed by pairs of string literal keys and expression values. No separator.
///
/// Output: a rust-phf map, with keys ASCII-lowercased:
/// ```text
/// static MAP: &'static ::cssparser::phf::Map<&'static str, $ValueType> = …;
/// ```
#[allow(non_snake_case)]
#[proc_macro]
pub fn cssparser_internal__phf_map(input: TokenStream) -> TokenStream {
    struct Input {
        value_type: syn::Type,
        keys: Vec<syn::LitStr>,
        values: Vec<syn::Expr>,
    }

    impl syn::parse::Parse for Input {
        fn parse(input: syn::parse::ParseStream) -> syn::parse::Result<Self> {
            let mut keys = Vec::new();
            let mut values = Vec::new();
            let value_type = input.parse()?;
            while !input.is_empty() {
                keys.push(input.parse()?);
                values.push(input.parse()?);
            }
            Ok(Input {
                value_type,
                keys,
                values,
            })
        }
    }

    let Input {
        value_type,
        keys,
        values,
    } = syn::parse_macro_input!(input);
    let keys = keys
        .iter()
        .map(|s| syn::LitStr::new(&s.value().to_ascii_lowercase(), s.span()));

    quote::quote!(
        pub(super) static MAP: Map<&'static str, #value_type> = phf_map! {
            #(
                #keys => #values,
            )*
        };
    )
    .into()
}
