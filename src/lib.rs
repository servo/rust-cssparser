/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#![crate_name = "cssparser"]
#![crate_type = "rlib"]

#![deny(missing_docs)]
#![cfg_attr(feature = "serde-serialization", feature(custom_derive))]
#![cfg_attr(feature = "serde-serialization", feature(plugin))]
#![cfg_attr(feature = "serde-serialization", plugin(serde_macros))]

/*!

Implementation of [CSS Syntax Module Level 3](http://dev.w3.org/csswg/css-syntax-3/) for Rust.

# Input

Everything is based on `Parser` objects, which borrow a `&str` input.
If you have bytes (from a file, the network, or something),
see the `decode_stylesheet_bytes` function.

# Conventions for parsing functions

* Take (at least) a `input: &mut cssparser::Parser` parameter
* Return `Result<_, ()>`
* When returning `Ok(_)`,
  the function must have consume exactly the amount of input that represents the parsed value.
* When returning `Err(())`, any amount of input may have been consumed.

As a consequence, when calling another parsing function, either:

* Any `Err(())` return value must be propagated.
  This happens by definition for tail calls,
  and can otherwise be done with the `try!` macro.
* Or the call must be wrapped in a `Parser::try` call.
  `try` takes a closure that takes a `Parser` and returns a `Result`,
  calls it once,
  and returns itself that same result.
  If the result is `Err`,
  it restores the position inside the input to the one saved before calling the closure.

Examples:

```{rust,ignore}
// 'none' | <image>
fn parse_background_image(context: &ParserContext, input: &mut Parser)
                                    -> Result<Option<Image>, ()> {
    if input.try(|input| input.expect_ident_matching("none")).is_ok() {
        Ok(None)
    } else {
        Image::parse(context, input).map(Some)  // tail call
    }
}
```

```{rust,ignore}
// [ <length> | <percentage> ] [ <length> | <percentage> ]?
fn parse_border_spacing(_context: &ParserContext, input: &mut Parser)
                          -> Result<(LengthOrPercentage, LengthOrPercentage), ()> {
    let first = try!(LengthOrPercentage::parse);
    let second = input.try(LengthOrPercentage::parse).unwrap_or(first);
    (first, second)
}
```

*/

extern crate encoding;
#[macro_use] extern crate matches;
#[cfg(test)] extern crate tempdir;
#[cfg(test)] extern crate rustc_serialize;
#[cfg(feature = "serde-serialization")] extern crate serde;

pub use tokenizer::{Token, NumericValue, PercentageValue, SourceLocation};
pub use rules_and_declarations::{parse_important};
pub use rules_and_declarations::{DeclarationParser, DeclarationListParser, parse_one_declaration};
pub use rules_and_declarations::{RuleListParser, parse_one_rule};
pub use rules_and_declarations::{AtRuleType, QualifiedRuleParser, AtRuleParser};
pub use from_bytes::decode_stylesheet_bytes;
pub use color::{RGBA, Color, parse_color_keyword};
pub use nth::parse_nth;
pub use serializer::{ToCss, CssStringWriter, serialize_identifier, serialize_string};
pub use parser::{Parser, Delimiter, Delimiters, SourcePosition};


/**

This macro is equivalent to a `match` expression on an `&str` value,
but matching is case-insensitive in the ASCII range.

Usage example:

```{rust,ignore}
match_ignore_ascii_case! { string,
    "foo" => Some(Foo),
    "bar" => Some(Bar),
    "baz" => Some(Baz)
    _ => None
}
```

The macro also takes a slice of the value,
so that a `String` or `CowString` could be passed directly instead of a `&str`.

Note that because of `macro_rules` ambiguity resolutions quirks,
each arm except the fallback and the one before it must end with a comma.

*/
#[macro_export]
macro_rules! match_ignore_ascii_case {
    ( $value: expr, $( $string: expr => $result: expr ),+ _ => $fallback: expr ) => {
        {
            use std::ascii::AsciiExt;
            match &$value[..] {
                $(
                    s if s.eq_ignore_ascii_case($string) => $result,
                )+
                _ => $fallback
            }
        }
    };
}


mod rules_and_declarations;
mod tokenizer;
mod parser;
mod from_bytes;
mod color;
mod nth;
mod serializer;

#[cfg(test)]
mod tests;
