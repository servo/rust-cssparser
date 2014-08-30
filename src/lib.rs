/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#![crate_name = "cssparser"]
#![crate_type = "rlib"]

#![feature(globs, macro_rules)]

// Temporary work-around for https://github.com/rust-lang/rust/issues/16597
#![feature(import_shadowing)]

extern crate encoding;  // https://github.com/lifthrasiir/rust-encoding

extern crate debug;

#[cfg(test)]
extern crate test;

#[cfg(test)]
extern crate serialize;

pub use tokenizer::{tokenize, Tokenizer};
pub use parser::{parse_stylesheet_rules, StylesheetParser,
                 parse_rule_list, RuleListParser,
                 parse_declaration_list, DeclarationListParser,
                 parse_one_rule, parse_one_declaration, parse_one_component_value};
pub use from_bytes::{decode_stylesheet_bytes, parse_stylesheet_rules_from_bytes};
pub use color::{RGBA, Color, CurrentColor};
pub use nth::parse_nth;
pub use serializer::{ToCss, serialize_identifier, serialize_string};

pub mod ast;
mod tokenizer;
mod parser;
mod from_bytes;
mod color;
mod nth;
mod serializer;

#[cfg(test)]
mod tests;
