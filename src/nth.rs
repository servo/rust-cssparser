/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use std::ascii::AsciiExt;

use super::{Token, NumericValue, Parser};


/// Parse the *An+B* notation, as found in the `:nth-child()` selector.
/// The input is typically the arguments of a function,
/// in which case the caller needs to check if the arguments’ parser is exhausted.
/// Return `Ok((A, B))`, or `Err(())` for a syntax error.
pub fn parse_nth(input: &mut Parser) -> Result<(i32, i32), ()> {
    match try!(input.next()) {
        Token::Number(value) => Ok((0, try!(value.int_value.ok_or(())) as i32)),
        Token::Dimension(value, unit) => {
            let a = try!(value.int_value.ok_or(())) as i32;
            if unit.eq_ignore_ascii_case("n") {
                parse_b(input, a)
            } else if unit.eq_ignore_ascii_case("n-") {
                parse_signless_b(input, a, -1)
            } else {
                Ok((a, try!(parse_n_dash_digits(unit.as_slice()))))
            }
        }
        Token::Ident(value) => {
            if value.eq_ignore_ascii_case("even") {
                Ok((2, 0))
            } else if value.eq_ignore_ascii_case("odd") {
                Ok((2, 1))
            } else if value.eq_ignore_ascii_case("n") {
                parse_b(input, 1)
            } else if value.eq_ignore_ascii_case("-n") {
                parse_b(input, -1)
            } else if value.eq_ignore_ascii_case("n-") {
                parse_signless_b(input, 1, -1)
            } else if value.eq_ignore_ascii_case("-n-") {
                parse_signless_b(input, -1, -1)
            } else if value.starts_with("-") {
                Ok((-1, try!(parse_n_dash_digits(value.slice_from(1)))))
            } else {
                Ok((1, try!(parse_n_dash_digits(value.as_slice()))))
            }
        }
        Token::Delim('+') => match try!(input.next_including_whitespace()) {
            Token::Ident(value) => {
                if value.eq_ignore_ascii_case("n") {
                    parse_b(input, 1)
                } else if value.eq_ignore_ascii_case("n-") {
                    parse_signless_b(input, 1, -1)
                } else {
                    Ok((1, try!(parse_n_dash_digits(value.as_slice()))))
                }
            }
            _ => Err(())
        },
        token => input.unexpected(token)
    }
}


fn parse_b(input: &mut Parser, a: i32) -> Result<(i32, i32), ()> {
    match input.next() {
        Ok(Token::Delim('+')) => parse_signless_b(input, a, 1),
        Ok(Token::Delim('-')) => parse_signless_b(input, a, -1),
        Ok(Token::Number(ref value)) if has_sign(value) => {
            Ok((a, try!(value.int_value.ok_or(())) as i32))
        }
        token => {
            input.push_back_result(token);
            Ok((a, 0))
        }
    }
}

fn parse_signless_b(input: &mut Parser, a: i32, b_sign: i32) -> Result<(i32, i32), ()> {
    match try!(input.next()) {
        Token::Number(ref value) if !has_sign(value) => {
            Ok((a, b_sign * (try!(value.int_value.ok_or(())) as i32)))
        }
        _ => Err(())
    }
}

fn parse_n_dash_digits(string: &str) -> Result<i32, ()> {
    if string.len() >= 3
    && string.slice_to(2).eq_ignore_ascii_case("n-")
    && string.slice_from(2).chars().all(|c| matches!(c, '0'...'9'))
    {
        Ok(from_str(string.slice_from(1)).unwrap())  // Include the minus sign
    } else {
        Err(())
    }
}

#[inline]
fn has_sign(value: &NumericValue) -> bool {
    matches!(value.representation.as_bytes()[0], b'+' | b'-')
}
