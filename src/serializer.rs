/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use std::fmt;
use std::mem;
use std::num::Float;

use text_writer::{mod, TextWriter};

use super::{Token, NumericValue, PercentageValue};


pub trait ToCss for Sized? {
    /// Serialize `self` in CSS syntax, writing to `dest`.
    fn to_css<W>(&self, dest: &mut W) -> text_writer::Result where W: TextWriter;

    /// Serialize `self` in CSS syntax and return a string.
    ///
    /// (This is a convenience wrapper for `to_css` and probably should not be overridden.)
    #[inline]
    fn to_css_string(&self) -> String {
        let mut s = String::new();
        self.to_css(&mut s).unwrap();
        s
    }

    /// Serialize `self` in CSS syntax and return a result compatible with `std::fmt::Show`.
    ///
    /// Typical usage is, for a `Foo` that implements `ToCss`:
    ///
    /// ```{rust,ignore}
    /// use std::fmt;
    /// impl fmt::Show for Foo {
    ///     #[inline] fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { self.fmt_to_css(f) }
    /// }
    /// ```
    ///
    /// (This is a convenience wrapper for `to_css` and probably should not be overridden.)
    #[inline]
    fn fmt_to_css<W>(&self, dest: &mut W) -> fmt::Result where W: TextWriter {
        self.to_css(dest).map_err(|_| fmt::Error)
    }
}


#[inline]
fn write_numeric<W>(value: NumericValue, dest: &mut W) -> text_writer::Result
where W: TextWriter {
    // `value.value >= 0` is true for negative 0.
    if value.signed && value.value.is_positive() {
        try!(dest.write_str("+"));
    }
    try!(write!(dest, "{}", value.value))
    if value.int_value.is_none() && value.value.fract() == 0. {
        try!(dest.write_str(".0"));
    }
    Ok(())
}


impl<'a> ToCss for Token<'a> {
    fn to_css<W>(&self, dest: &mut W) -> text_writer::Result where W: TextWriter {
        match *self {
            Token::Ident(ref value) => try!(serialize_identifier(value.as_slice(), dest)),
            Token::AtKeyword(ref value) => {
                try!(dest.write_char('@'));
                try!(serialize_identifier(value.as_slice(), dest));
            },
            Token::Hash(ref value) => {
                try!(dest.write_char('#'));
                for c in value.as_slice().chars() {
                    try!(serialize_char(c, dest, /* is_identifier_start = */ false));
                }
            },
            Token::IDHash(ref value) => {
                try!(dest.write_char('#'));
                try!(serialize_identifier(value.as_slice(), dest));
            }
            Token::QuotedString(ref value) => try!(serialize_string(value.as_slice(), dest)),
            Token::Url(ref value) => {
                try!(dest.write_str("url("));
                try!(serialize_string(value.as_slice(), dest));
                try!(dest.write_char(')'));
            },
            Token::Delim(value) => try!(dest.write_char(value)),

            Token::Number(value) => try!(write_numeric(value, dest)),
            Token::Percentage(PercentageValue { unit_value, int_value, signed }) => {
                let value = NumericValue {
                    value: unit_value * 100.,
                    int_value: int_value,
                    signed: signed,
                };
                try!(write_numeric(value, dest));
                try!(dest.write_char('%'));
            },
            Token::Dimension(value, ref unit) => {
                try!(write_numeric(value, dest));
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

            Token::UnicodeRange(start, end) => {
                try!(dest.write_str(format!("U+{:X}", start).as_slice()));
                if end != start {
                    try!(dest.write_str(format!("-{:X}", end).as_slice()));
                }
            }

            Token::WhiteSpace(content) => try!(dest.write_str(content)),
            Token::Comment(content) => try!(write!(dest, "/*{}*/", content)),
            Token::Colon => try!(dest.write_char(':')),
            Token::Semicolon => try!(dest.write_char(';')),
            Token::Comma => try!(dest.write_char(',')),
            Token::IncludeMatch => try!(dest.write_str("~=")),
            Token::DashMatch => try!(dest.write_str("|=")),
            Token::PrefixMatch => try!(dest.write_str("^=")),
            Token::SuffixMatch => try!(dest.write_str("$=")),
            Token::SubstringMatch => try!(dest.write_str("*=")),
            Token::Column => try!(dest.write_str("||")),
            Token::CDO => try!(dest.write_str("<!--")),
            Token::CDC => try!(dest.write_str("-->")),

            Token::Function(ref name) => {
                try!(serialize_identifier(name.as_slice(), dest));
                try!(dest.write_char('('));
            },
            Token::ParenthesisBlock => try!(dest.write_char('(')),
            Token::SquareBracketBlock => try!(dest.write_char('[')),
            Token::CurlyBracketBlock => try!(dest.write_char('{')),

            Token::BadUrl => try!(dest.write_str("url(<bad url>)")),
            Token::BadString => try!(dest.write_str("\"<bad string>\n")),
            Token::CloseParenthesis => try!(dest.write_char(')')),
            Token::CloseSquareBracket => try!(dest.write_char(']')),
            Token::CloseCurlyBracket => try!(dest.write_char('}')),
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
    try!(CssStringWriter::new(dest).write_str(value));
    try!(dest.write_char('"'));
    Ok(())
}


/// A `TextWriter` adaptor that escapes text for writing as a CSS string.
/// Quotes are not included.
///
/// Typical usage:
///
/// ```{rust,ignore}
/// fn write_foo<W>(foo: &Foo, dest: &mut W) -> text_writer::Result where W: TextWriter {
///     try!(dest.write_char('"'));
///     {
///         let mut string_dest = CssStringWriter::new(dest);
///         // Write into string_dest...
///     }
///     try!(dest.write_char('"'));
///     Ok(())
/// }
/// ```
pub struct CssStringWriter<'a, W: 'a> {
    inner: &'a mut W,
}

impl<'a, W> CssStringWriter<'a, W> where W: TextWriter {
    pub fn new(inner: &'a mut W) -> CssStringWriter<'a, W> {
        CssStringWriter { inner: inner }
    }
}

impl<'a, W> TextWriter for CssStringWriter<'a, W> where W: TextWriter {
    fn write_str(&mut self, s: &str) -> text_writer::Result {
        // TODO: avoid decoding/re-encoding UTF-8?
        for c in s.chars() {
            try!(self.write_char(c))
        }
        Ok(())
    }

    fn write_char(&mut self, c: char) -> text_writer::Result {
        match c {
            '"' => self.inner.write_str("\\\""),
            '\\' => self.inner.write_str("\\\\"),
            '\n' => self.inner.write_str("\\A "),
            '\r' => self.inner.write_str("\\D "),
            '\x0C' => self.inner.write_str("\\C "),
            _ => self.inner.write_char(c),
        }
    }
}


impl<'a> ToCss for [Token<'a>] {
    fn to_css<W>(&self, dest: &mut W) -> text_writer::Result where W: TextWriter {
        let mut writer = TokenWriter::new(dest);
        for token in self.iter() {
            try!(writer.write(token))
        }
        Ok(())
    }
}


pub struct TokenWriter<'i, 'a, W: 'a> {
    dest: &'a mut W,
    previous_token: Option<Token<'i>>,
}

impl<'i, 'a, W> TokenWriter<'i, 'a, W> where W: TextWriter {
    pub fn new<'a>(dest: &'a mut W) -> TokenWriter<'i, 'a, W> {
        TokenWriter {
            dest: dest,
            previous_token: None,
        }
    }

    pub fn write(&mut self, token: &Token<'i>) -> text_writer::Result {
        use Token::*;
        let previous = &mem::replace(&mut self.previous_token, Some((*token).clone()))
                        // A "not special" token:
                        .unwrap_or(Colon);
        let (a, b) = (previous, token);
        if (
            matches!(*a, Ident(..) | AtKeyword(..) | Hash(..) | IDHash(..) |
                         Dimension(..) | Delim('#') | Delim('-') | Number(..)) &&
            matches!(*b, Ident(..) | Function(..) | Url(..) | BadUrl(..) |
                         Number(..) | Percentage(..) | Dimension(..) | UnicodeRange(..))
        ) || (
            matches!(*a, Ident(..)) &&
            matches!(*b, ParenthesisBlock(..))
        ) || (
            matches!(*a, Ident(..) | AtKeyword(..) | Hash(..) | IDHash(..) | Dimension(..)) &&
            matches!(*b, Delim('-') | CDC)
        ) || (
            matches!(*a, Delim('#') | Delim('-') | Number(..) | Delim('@')) &&
            matches!(*b, Ident(..) | Function(..) | Url(..) | BadUrl(..))
        ) || (
            matches!(*a, Delim('@')) &&
            matches!(*b, Ident(..) | Function(..) | Url(..) | BadUrl(..) |
                         UnicodeRange(..) | Delim('-'))
        ) || (
            matches!(*a, UnicodeRange(..) | Delim('.') | Delim('+')) &&
            matches!(*b, Number(..) | Percentage(..) | Dimension(..))
        ) || (
            matches!(*a, UnicodeRange(..)) &&
            matches!(*b, Ident(..) | Function(..) | Delim('?'))
        ) || matches!((a, b), (&Delim(a), &Delim(b)) if matches!((a, b),
            ('#', '-') |
            ('$', '=') |
            ('*', '=') |
            ('^', '=') |
            ('~', '=') |
            ('|', '=') |
            ('|', '|') |
            ('/', '*')
        )) {
            try!(self.dest.write_str("/**/"));
        }
        // Skip whitespace when '\n' was previously written at the previous iteration.
        if !matches!((previous, token), (&Delim('\\'), &WhiteSpace(_))) {
            try!(token.to_css(self.dest));
        }
        if token == &Delim('\\') {
            try!(self.dest.write_char('\n'));
        }
        Ok(())
    }
}
