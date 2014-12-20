/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use text_writer::{mod, TextWriter};

use ast::*;
use ast::ComponentValue::*;


pub trait ToCss for Sized? {
    fn to_css<W>(&self, dest: &mut W) -> text_writer::Result where W: TextWriter;

    fn to_css_string(&self) -> String {
        let mut s = String::new();
        self.to_css(&mut s).unwrap();
        s
    }
}


impl ToCss for ComponentValue {
    fn to_css<W>(&self, dest: &mut W) -> text_writer::Result where W: TextWriter {
        match self {
            &Ident(ref value) => try!(serialize_identifier(value.as_slice(), dest)),
            &AtKeyword(ref value) => {
                try!(dest.write_char('@'));
                try!(serialize_identifier(value.as_slice(), dest));
            },
            &Hash(ref value) => {
                try!(dest.write_char('#'));
                for c in value.as_slice().chars() {
                    try!(serialize_char(c, dest, /* is_identifier_start = */ false));
                }
            },
            &IDHash(ref value) => {
                try!(dest.write_char('#'));
                try!(serialize_identifier(value.as_slice(), dest));
            }
            &QuotedString(ref value) => try!(serialize_string(value.as_slice(), dest)),
            &URL(ref value) => {
                try!(dest.write_str("url("));
                try!(serialize_string(value.as_slice(), dest));
                try!(dest.write_char(')'));
            },
            &Delim(value) => try!(dest.write_char(value)),

            &Number(ref value) => try!(dest.write_str(value.representation.as_slice())),
            &Percentage(ref value) => {
                try!(dest.write_str(value.representation.as_slice()));
                try!(dest.write_char('%'));
            },
            &Dimension(ref value, ref unit) => {
                try!(dest.write_str(value.representation.as_slice()));
                // Disambiguate with scientific notation.
                let unit = unit.as_slice();
                if unit == "e" || unit == "E" || unit.starts_with("e-") || unit.starts_with("E-") {
                    try!(dest.write_str("\\65 "));
                    for c in unit.slice_from(1).chars() {
                        try!(serialize_char(c, dest, /* is_identifier_start = */ false));
                    }
                } else {
                    try!(serialize_identifier(unit, dest));
                }
            },

            &UnicodeRange(start, end) => {
                try!(dest.write_str(format!("U+{:X}", start).as_slice()));
                if end != start {
                    try!(dest.write_str(format!("-{:X}", end).as_slice()));
                }
            }

            &WhiteSpace => try!(dest.write_char(' ')),
            &Colon => try!(dest.write_char(':')),
            &Semicolon => try!(dest.write_char(';')),
            &Comma => try!(dest.write_char(',')),
            &IncludeMatch => try!(dest.write_str("~=")),
            &DashMatch => try!(dest.write_str("|=")),
            &PrefixMatch => try!(dest.write_str("^=")),
            &SuffixMatch => try!(dest.write_str("$=")),
            &SubstringMatch => try!(dest.write_str("*=")),
            &Column => try!(dest.write_str("||")),
            &CDO => try!(dest.write_str("<!--")),
            &CDC => try!(dest.write_str("-->")),

            &Function(ref name, ref arguments) => {
                try!(serialize_identifier(name.as_slice(), dest));
                try!(dest.write_char('('));
                try!(arguments.to_css(dest));
                try!(dest.write_char(')'));
            },
            &ParenthesisBlock(ref content) => {
                try!(dest.write_char('('));
                try!(content.to_css(dest));
                try!(dest.write_char(')'));
            },
            &SquareBracketBlock(ref content) => {
                try!(dest.write_char('['));
                try!(content.to_css(dest));
                try!(dest.write_char(']'));
            },
            &CurlyBracketBlock(ref content) => {
                try!(dest.write_char('{'));
                let component_values = content.iter().map(|t| match *t { (ref c, _) => c });
                try!(component_values_to_css(component_values, dest));
                try!(dest.write_char('}'));
            },

            &BadURL => try!(dest.write_str("url(<bad url>)")),
            &BadString => try!(dest.write_str("\"<bad string>\n")),
            &CloseParenthesis => try!(dest.write_char(')')),
            &CloseSquareBracket => try!(dest.write_char(']')),
            &CloseCurlyBracket => try!(dest.write_char('}')),
        }
        Ok(())
    }
}


pub fn serialize_identifier<W>(value: &str, dest: &mut W) -> text_writer::Result
where W:TextWriter {
    // TODO: avoid decoding/re-encoding UTF-8?
    let mut iter = value.chars();
    let mut c = iter.next().unwrap();
    if c == '-' {
        c = match iter.next() {
            None => return dest.write_str("\\-"),
            Some(c) => { try!(dest.write_char('-')); c },
        }
    };
    try!(serialize_char(c, dest, /* is_identifier_start = */ true));
    for c in iter {
        try!(serialize_char(c, dest, /* is_identifier_start = */ false));
    }
    Ok(())
}


#[inline]
fn serialize_char<W>(c: char, dest: &mut W, is_identifier_start: bool) -> text_writer::Result
where W: TextWriter {
    match c {
        '0'...'9' if is_identifier_start => try!(dest.write_str(format!("\\3{} ", c).as_slice())),
        '-' if is_identifier_start => try!(dest.write_str("\\-")),
        '0'...'9' | 'A'...'Z' | 'a'...'z' | '_' | '-' => try!(dest.write_char(c)),
        _ if c > '\x7F' => try!(dest.write_char(c)),
        '\n' => try!(dest.write_str("\\A ")),
        '\r' => try!(dest.write_str("\\D ")),
        '\x0C' => try!(dest.write_str("\\C ")),
        _ => { try!(dest.write_char('\\')); try!(dest.write_char(c)) },
    };
    Ok(())
}


pub fn serialize_string<W>(value: &str, dest: &mut W) -> text_writer::Result
where W: TextWriter {
    try!(dest.write_char('"'));
    // TODO: avoid decoding/re-encoding UTF-8?
    for c in value.chars() {
        match c {
            '"' => try!(dest.write_str("\\\"")),
            '\\' => try!(dest.write_str("\\\\")),
            '\n' => try!(dest.write_str("\\A ")),
            '\r' => try!(dest.write_str("\\D ")),
            '\x0C' => try!(dest.write_str("\\C ")),
            _ => try!(dest.write_char(c)),
        };
    }
    dest.write_char('"')
}


impl<'a> ToCss for [ComponentValue] {
    fn to_css<W>(&self, dest: &mut W) -> text_writer::Result where W: TextWriter {
        component_values_to_css(self.iter(), dest)
    }
}

fn component_values_to_css<'a, I, W>(mut iter: I, dest: &mut W) -> text_writer::Result
where I: Iterator<&'a ComponentValue>, W: TextWriter {
    let mut previous = match iter.next() {
        None => return Ok(()),
        Some(first) => { try!(first.to_css(dest)); first }
    };
    macro_rules! matches(
        ($value:expr, $($pattern:pat)|+) => (
            match $value { $($pattern)|+ => true, _ => false }
        );
    )
    // This does not borrow-check: for component_value in iter {
    loop { match iter.next() { None => break, Some(component_value) => {
        let (a, b) = (previous, component_value);
        if (
            matches!(*a, Ident(..) | AtKeyword(..) | Hash(..) | IDHash(..) |
                         Dimension(..) | Delim('#') | Delim('-') | Number(..)) &&
            matches!(*b, Ident(..) | Function(..) | URL(..) | BadURL(..) |
                         Number(..) | Percentage(..) | Dimension(..) | UnicodeRange(..))
        ) || (
            matches!(*a, Ident(..)) &&
            matches!(*b, ParenthesisBlock(..))
        ) || (
            matches!(*a, Ident(..) | AtKeyword(..) | Hash(..) | IDHash(..) | Dimension(..)) &&
            matches!(*b, Delim('-') | CDC)
        ) || (
            matches!(*a, Delim('#') | Delim('-') | Number(..) | Delim('@')) &&
            matches!(*b, Ident(..) | Function(..) | URL(..) | BadURL(..))
        ) || (
            matches!(*a, Delim('@')) &&
            matches!(*b, Ident(..) | Function(..) | URL(..) | BadURL(..) |
                         UnicodeRange(..) | Delim('-'))
        ) || (
            matches!(*a, UnicodeRange(..) | Delim('.') | Delim('+')) &&
            matches!(*b, Number(..) | Percentage(..) | Dimension(..))
        ) || (
            matches!(*a, UnicodeRange(..)) &&
            matches!(*b, Ident(..) | Function(..) | Delim('?'))
        ) || (match (a, b) { (&Delim(a), &Delim(b)) => matches!((a, b),
            ('#', '-') |
            ('$', '=') |
            ('*', '=') |
            ('^', '=') |
            ('~', '=') |
            ('|', '=') |
            ('|', '|') |
            ('/', '*')
        ), _ => false }) {
            try!(dest.write_str("/**/"));
        }
        // Skip whitespace when '\n' was previously written at the previous iteration.
        if !matches!((previous, component_value), (&Delim('\\'), &WhiteSpace)) {
            try!(component_value.to_css(dest));
        }
        if component_value == &Delim('\\') {
            try!(dest.write_char('\n'));
        }
        previous = component_value;
    }}}
    Ok(())
}
