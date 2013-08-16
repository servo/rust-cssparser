/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use std::i32;
use std::ascii::StrAsciiExt;

use ast::*;


/// Parse the An+B notation, as found in the ``:nth-child()`` selector.
/// The input is typically the arguments of a function component value.
/// Return Some((A, B)), or None for a syntax error.
pub fn parse_nth(input: &[ComponentValue]) -> Option<(i32, i32)> {
    let iter = &mut input.skip_whitespace();
    match iter.next() {
        Some(&Number(ref value)) => match value.int_value {
            Some(b) => parse_end(iter, 0, b as i32),
            _ => None,
        },
        Some(&Dimension(ref value, ref unit)) => match value.int_value {
            Some(a) => {
                let unit: &str = unit.as_slice().to_ascii_lower();
                match unit {
                    "n" => parse_b(iter, a as i32),
                    "n-" => parse_signless_b(iter, a as i32, -1),
                    _ => match(parse_n_dash_digits(unit)) {
                        Some(b) => parse_end(iter, a as i32, b),
                        _ => None
                    },
                }
            },
            _ => None,
        },
        Some(&Ident(ref value)) => {
            let ident: &str = value.as_slice().to_ascii_lower();
            match ident {
                "even" => parse_end(iter, 2, 0),
                "odd" => parse_end(iter, 2, 1),
                "n" => parse_b(iter, 1),
                "-n" => parse_b(iter, -1),
                "n-" => parse_signless_b(iter, 1, -1),
                "-n-" => parse_signless_b(iter, -1, -1),
                _ if ident.starts_with("-") => match(parse_n_dash_digits(ident.slice_from(1))) {
                    Some(b) => parse_end(iter, -1, b),
                    _ => None
                },
                _ =>  match(parse_n_dash_digits(ident)) {
                    Some(b) => parse_end(iter, 1, b),
                    _ => None
                },
            }
        },
        Some(&Delim('+')) => match iter.iter_with_whitespace.next() {
            Some(&Ident(ref value)) => {
                let ident: &str = value.as_slice().to_ascii_lower();
                match ident {
                    "n" => parse_b(iter, 1),
                    "n-" => parse_signless_b(iter, 1, -1),
                    _ => match(parse_n_dash_digits(ident)) {
                        Some(b) => parse_end(iter, 1, b),
                        _ => None
                    },
                }
            },
            _ => None
        },
        _ => None
    }
}


type Nth = Option<(i32, i32)>;
type Iter<'self> = SkipWhitespaceIterator<'self>;

fn parse_b(iter: &mut Iter, a: i32) -> Nth {
    match iter.next() {
        None => Some((a, 0)),
        Some(&Delim('+')) => parse_signless_b(iter, a, 1),
        Some(&Delim('-')) => parse_signless_b(iter, a, -1),
        Some(&Number(ref value)) => match value.int_value {
            Some(b) if has_sign(value) => parse_end(iter, a, b as i32),
            _ => None,
        },
        _ => None
    }
}

fn parse_signless_b(iter: &mut Iter, a: i32, b_sign: i32) -> Nth {
    match iter.next() {
        Some(&Number(ref value)) => match value.int_value {
            Some(b) if !has_sign(value) => parse_end(iter, a, b_sign * (b as i32)),
            _ => None,
        },
        _ => None
    }
}

fn parse_end(iter: &mut Iter, a: i32, b: i32) -> Nth {
    match iter.next() {
        None => Some((a, b)),
        Some(_) => None,
    }
}

fn parse_n_dash_digits(string: &str) -> Option<i32> {
    if string.len() >= 3
    && string.starts_with("n-")
    && string.slice_from(2).iter().all(|c| match c { '0'..'9' => true, _ => false })
    {
        let result = i32::from_str(string.slice_from(1));  // Include the minus sign
        assert!(result.is_some());
        result
    }
    else { None }
}

#[inline]
fn has_sign(value: &NumericValue) -> bool {
    match value.representation[0] as char {
        '+' | '-' => true,
        _ => false
    }
}
