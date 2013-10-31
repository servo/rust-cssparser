/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use ast;
use ast::*;


impl ast::ComponentValue {
    pub fn to_css(&mut self) -> ~str {
        let mut css = ~"";
        self.to_css_push(&mut css);
        css
    }

    pub fn to_css_push(&self, css: &mut ~str) {
        match *self {
            Ident(ref value) => serialize_identifier(value.as_slice(), css),
            AtKeyword(ref value) => {
                css.push_char('@');
                serialize_identifier(value.as_slice(), css);
            },
            Hash(ref value) | IDHash(ref value) => {
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

            Number(ref value) => css.push_str(value.representation),
            Percentage(ref value) => {
                css.push_str(value.representation);
                css.push_char('%');
            },
            Dimension(ref value, ref unit) => {
                css.push_str(value.representation);
                serialize_identifier(unit.as_slice(), css);
            },

            UnicodeRange(start, end) => {
                css.push_str(format!("U+{:X}", start));
                if end != start {
                    css.push_str(format!("-{:X}", end));
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
}


pub fn serialize_identifier(value: &str, css: &mut ~str) {
    // TODO: avoid decoding/re-encoding UTF-8?
    let mut iter = value.iter();
    let mut c = iter.next().unwrap();
    if c == '-' {
        c = match iter.next() {
            None => { css.push_str("\\-"); return },
            Some(c) => { css.push_char('-'); c },
        }
    };
    serialize_char(c, css, /* is_start = */ true);
    for c in iter {
        serialize_char(c, css, /* is_start = */ false);
    }

    #[inline]
    fn serialize_char(c: char, css: &mut ~str, is_start: bool) {
        match c {
            '0'..'9' if is_start => css.push_str(format!("\\\\3{} ", c)),
            '-' if is_start => css.push_str("\\-"),
            '0'..'9' | 'A'..'Z' | 'a'..'z' | '_' | '-' => css.push_char(c),
            _ if c > '\x7F' => css.push_char(c),
            '\n' => css.push_str("\\A "),
            '\r' => css.push_str("\\D "),
            '\x0C' => css.push_str("\\C "),
            _ => { css.push_char('\\'); css.push_char(c) },
        }
    }
}

pub fn serialize_string(value: &str, css: &mut ~str) {
    css.push_char('"');
    // TODO: avoid decoding/re-encoding UTF-8?
    for c in value.iter() {
        match c {
            '"' => css.push_str("\""),
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
    fn to_css(&mut self) -> ~str {
        let mut css = ~"";
        self.to_css_push(&mut css);
        css
    }

    fn to_css_push(&mut self, css: &mut ~str);
}


impl<'self, I: Iterator<&'self ComponentValue>> ToCss for I {
    fn to_css_push(&mut self, css: &mut ~str) {
        let mut previous = match self.next() {
            None => return,
            Some(first) => { first.to_css_push(css); first }
        };
        macro_rules! matches(
            ($value:expr, $($pattern:pat)|+) => (
                match $value { $($pattern)|+ => true, _ => false }
            );
        )
        loop { match self.next() { None => break, Some(component_value) => {
            let (a, b) = (previous, component_value);
// FIXME: this is incorrect.
// See https://github.com/mozilla-servo/rust-cssparser/issues/24
//            if (
//                matches!(*a, Hash(*) | IDHash(*) | AtKeyword(*)) &&
//                matches!(*b, Number(*) | Percentage(*) | Ident(*) | Dimension(*) |
//                            UnicodeRange(*) | URL(*) | Function(*))
//            ) || (
//                matches!(*a, Number(*) | Ident(*) | Dimension(*)) &&
//                matches!(*b, Number(*) | Ident(*) | Dimension(*))
//            ) || (
//                matches!(*a, Number(*) | Ident(*) | Dimension(*)) &&
//                matches!(*b, Percentage(*) | UnicodeRange(*) | URL(*) | Function(*))
//            ) || (
//                matches!(*a, Ident(*)) &&
//                matches!(*b, ParenthesisBlock(*))
//            ) || (
//                matches!(*a, Delim('#') | Delim('@')) &&
//                !matches!(*b, WhiteSpace)
//            ) || (
//                matches!(*a, Delim('-') | Delim('+') | Delim('.') | Delim('<') |
//                             Delim('>') | Delim('!')) &&
//                !matches!(*b, WhiteSpace)
//            ) || (
//                !matches!(*a, WhiteSpace) &&
//                matches!(*b, Delim('-') | Delim('+') | Delim('.') | Delim('<') |
//                             Delim('>') | Delim('!'))
//            ) || (
//                matches!(*a, Delim('/')) &&
//                matches!(*b, Delim('*'))
//            ) || (
//                matches!(*a, Delim('*')) &&
//                matches!(*b, Delim('/'))
//            ) {
//                css.push_str("/**/")
//            }
            component_value.to_css_push(css);
            previous = component_value;
        }}}
    }
}
