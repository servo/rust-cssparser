/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use ast::*;


pub fn to_css_push(component_value: &ComponentValue, css: &mut String) {
    match *component_value {
        Ident(ref value) => serialize_identifier(value.as_slice(), css),
        AtKeyword(ref value) => {
            css.push_char('@');
            serialize_identifier(value.as_slice(), css);
        },
        Hash(ref value) => {
            css.push_char('#');
            for c in value.as_slice().chars() {
                serialize_char(c, css, /* is_identifier_start = */ false);
            }
        },
        IDHash(ref value) => {
            css.push_char('#');
            serialize_identifier(value.as_slice(), css);
        }
        String(ref value) => serialize_string(value.as_slice(), css),
        URL(ref value) => {
            css.push_str("url(");
            serialize_string(value.as_slice(), css);
            css.push_char(')');
        },
        Delim(value) => css.push_char(value),

        Number(ref value) => css.push_str(value.representation.as_slice()),
        Percentage(ref value) => {
            css.push_str(value.representation.as_slice());
            css.push_char('%');
        },
        Dimension(ref value, ref unit) => {
            css.push_str(value.representation.as_slice());
            // Disambiguate with scientific notation.
            let unit = unit.as_slice();
            if unit == "e" || unit == "E" || unit.starts_with("e-") || unit.starts_with("E-") {
                css.push_str("\\65 ");
                for c in unit.slice_from(1).chars() {
                    serialize_char(c, css, /* is_identifier_start = */ false);
                }
            } else {
                serialize_identifier(unit, css)
            }
        },

        UnicodeRange(start, end) => {
            css.push_str(format!("U+{:X}", start).as_slice());
            if end != start {
                css.push_str(format!("-{:X}", end).as_slice());
            }
        }

        WhiteSpace => css.push_char(' '),
        Colon => css.push_char(':'),
        Semicolon => css.push_char(';'),
        Comma => css.push_char(','),
        IncludeMatch => css.push_str("~="),
        DashMatch => css.push_str("|="),
        PrefixMatch => css.push_str("^="),
        SuffixMatch => css.push_str("$="),
        SubstringMatch => css.push_str("*="),
        Column => css.push_str("||"),
        CDO => css.push_str("<!--"),
        CDC => css.push_str("-->"),

        Function(ref name, ref arguments) => {
            serialize_identifier(name.as_slice(), css);
            css.push_char('(');
            arguments.iter().to_css_push(css);
            css.push_char(')');
        },
        ParenthesisBlock(ref content) => {
            css.push_char('(');
            content.iter().to_css_push(css);
            css.push_char(')');
        },
        SquareBracketBlock(ref content) => {
            css.push_char('[');
            content.iter().to_css_push(css);
            css.push_char(']');
        },
        CurlyBracketBlock(ref content) => {
            css.push_char('{');
            content.iter().map(|t| match *t { (ref c, _) => c }).to_css_push(css);
            css.push_char('}');
        },

        BadURL => css.push_str("url(<bad url>)"),
        BadString => css.push_str("\"<bad string>\n"),
        CloseParenthesis => css.push_char(')'),
        CloseSquareBracket => css.push_char(']'),
        CloseCurlyBracket => css.push_char('}'),
    }
}


pub fn serialize_identifier(value: &str, css: &mut String) {
    // TODO: avoid decoding/re-encoding UTF-8?
    let mut iter = value.chars();
    let mut c = iter.next().unwrap();
    if c == '-' {
        c = match iter.next() {
            None => { css.push_str("\\-"); return },
            Some(c) => { css.push_char('-'); c },
        }
    };
    serialize_char(c, css, /* is_identifier_start = */ true);
    for c in iter {
        serialize_char(c, css, /* is_identifier_start = */ false);
    }
}


#[inline]
fn serialize_char(c: char, css: &mut String, is_identifier_start: bool) {
    match c {
        '0'..'9' if is_identifier_start => css.push_str(format!("\\3{} ", c).as_slice()),
        '-' if is_identifier_start => css.push_str("\\-"),
        '0'..'9' | 'A'..'Z' | 'a'..'z' | '_' | '-' => css.push_char(c),
        _ if c > '\x7F' => css.push_char(c),
        '\n' => css.push_str("\\A "),
        '\r' => css.push_str("\\D "),
        '\x0C' => css.push_str("\\C "),
        _ => { css.push_char('\\'); css.push_char(c) },
    }
}


pub fn serialize_string(value: &str, css: &mut String) {
    css.push_char('"');
    // TODO: avoid decoding/re-encoding UTF-8?
    for c in value.chars() {
        match c {
            '"' => css.push_str("\\\""),
            '\\' => css.push_str("\\\\"),
            '\n' => css.push_str("\\A "),
            '\r' => css.push_str("\\D "),
            '\x0C' => css.push_str("\\C "),
            _ => css.push_char(c),
        }
    }
    css.push_char('"');
}


pub trait ToCss {
    fn to_css(&mut self) -> String {
        let mut css = String::new();
        self.to_css_push(&mut css);
        css
    }

    fn to_css_push(&mut self, css: &mut String);
}


impl<'a, I: Iterator<&'a ComponentValue>> ToCss for I {
    fn to_css_push(&mut self, css: &mut String) {
        let mut previous = match self.next() {
            None => return,
            Some(first) => { first.to_css_push(css); first }
        };
        macro_rules! matches(
            ($value:expr, $($pattern:pat)|+) => (
                match $value { $($pattern)|+ => true, _ => false }
            );
        )
        // This does not borrow-check: for component_value in self {
        loop { match self.next() { None => break, Some(component_value) => {
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
                css.push_str("/**/")
            }
            // Skip whitespace when '\n' was previously written at the previous iteration.
            if !matches!((previous, component_value), (&Delim('\\'), &WhiteSpace)) {
                component_value.to_css_push(css);
            }
            if component_value == &Delim('\\') {
                css.push_char('\n');
            }
            previous = component_value;
        }}}
    }
}
