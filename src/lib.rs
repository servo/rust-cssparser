/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#![crate_name = "cssparser"]
#![crate_type = "rlib"]

#![feature(globs, macro_rules, unsafe_destructor, phase)]

extern crate encoding;
extern crate text_writer;
#[phase(plugin)] extern crate matches;
#[cfg(test)] extern crate test;
#[cfg(test)] extern crate serialize;

pub use tokenizer::{Token, NumericValue, PercentageValue, SourceLocation};
pub use rules_and_declarations::{parse_important};
pub use rules_and_declarations::{DeclarationParser, DeclarationListParser, parse_one_declaration};
pub use rules_and_declarations::{RuleListParser, parse_one_rule};
pub use rules_and_declarations::{AtRuleType, QualifiedRuleParser, AtRuleParser};
pub use from_bytes::{decode_stylesheet_bytes, parse_stylesheet_rules_from_bytes};
pub use color::{RGBA, Color, parse_color_keyword};
pub use nth::parse_nth;
pub use serializer::{ToCss, CssStringWriter, serialize_identifier, serialize_string};
pub use parser::{Parser, Delimiter, Delimiters, SourcePosition};


/**

This macro is equivalent to a `match` expression on an `&str` value,
but matching is case-insensitive in the ASCII range.

Usage example:

```{rust,ignore}
match_ignore_ascii_case! { string:
    "foo" => Some(Foo),
    "bar" => Some(Bar),
    "baz" => Some(Baz)
    _ => None
}
```

The macro also calls `.as_slice()` on the value,
so that a `String` or `CowString` could be passed directly instead of a `&str`.

Note that because of `macro_rules` ambiguity resolutions quirks,
each arm except the fallback and the one before it must end with a comma.

*/
#[macro_export]
macro_rules! match_ignore_ascii_case {
    ( $value: expr: $( $string: expr => $result: expr ),+ _ => $fallback: expr ) => {
        {
            use std::ascii::AsciiExt;
            match $value.as_slice() {
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
