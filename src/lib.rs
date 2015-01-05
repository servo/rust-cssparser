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

pub use tokenizer::{Tokenizer, Token, NumericValue};
pub use rules_and_declarations::{Priority, parse_important};
pub use rules_and_declarations::{DeclarationParser, DeclarationListParser, parse_one_declaration};
pub use rules_and_declarations::{RuleListParser, parse_one_rule};
pub use rules_and_declarations::{AtRulePrelude, QualifiedRuleParser, AtRuleParser};
pub use from_bytes::{decode_stylesheet_bytes, parse_stylesheet_rules_from_bytes};
pub use color::{RGBA, Color, parse_color_keyword};
pub use nth::parse_nth;
pub use serializer::{ToCss, CssStringWriter, serialize_identifier, serialize_string};
pub use parser::{Parser, Delimiter, Delimiters};

mod rules_and_declarations;
mod tokenizer;
mod parser;
mod from_bytes;
mod color;
mod nth;
mod serializer;

#[cfg(test)]
mod tests;
