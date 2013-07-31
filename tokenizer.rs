/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

// http://dev.w3.org/csswg/css3-syntax/#tokenization

use std::{str, u32, i32, f64, vec};

use ast::*;
use super::eq_ascii_lower;


struct Parser {
    input: ~str,
    length: uint,  // All counted in bytes, not characters
    position: uint,  // All counted in bytes, not characters
    // TODO: add these in tokens
//    priv line: uint,
//    priv column: uint,  // All counted in bytes, not characters
}


impl Parser {
    pub fn from_str(input: &str) -> Parser {
        let input = preprocess(input);
        Parser {
            length: input.len(),
            input: input,
            position: 0,
//            line: 1,
//            column: 1,
        }
    }
}


macro_rules! is_match(
    ($value:expr, $($pattern:pat)|+) => (
        match $value { $($pattern)|+ => true, _ => false }
    );
)


pub fn next_component_value(parser: &mut Parser) -> Option<ComponentValue> {
    consume_comments(parser);
    if parser.is_eof() { return None }
    let c = parser.current_char();
    Some(match c {
        '-' => {
            if parser.starts_with("-->") {
                parser.position += 3;
                CDC
            } else if next_is_namestart_or_escape(parser) {
                consume_ident(parser)
            } else if (
                parser.position + 1 < parser.length
                && is_match!(parser.char_at(1), '0'..'9')
            ) || (
                parser.position + 2 < parser.length
                && parser.char_at(1) == '.'
                && is_match!(parser.char_at(2), '0'..'9')
            ) {
                consume_numeric(parser)
            } else {
                parser.position += 1;
                Delim('-')
            }
        },
        '<' => {
            if parser.starts_with("<!--") {
                parser.position += 4;
                CDO
            } else {
                parser.position += 1;
                Delim('<')
            }
        },
        '0'..'9' => consume_numeric(parser),
        '.' => {
            if (parser.position + 1 < parser.length && is_match!(parser.char_at(1), '0'..'9')) {
                consume_numeric(parser)
            } else {
                parser.position += 1;
                Delim('.')
            }
        }
        '+' => {
            if (
                parser.position + 1 < parser.length
                && is_match!(parser.char_at(1), '0'..'9')
            ) || (
                parser.position + 2 < parser.length
                && parser.char_at(1) == '.'
                && is_match!(parser.char_at(2), '0'..'9')
            ) {
                consume_numeric(parser)
            } else {
                parser.position += 1;
                Delim('+')
            }
        },
        'u' | 'U' => consume_unicode_range(parser),
        'a'..'z' | 'A'..'Z' | '_' | '\\' => consume_ident(parser),
        '~' if parser.starts_with("~=") => { parser.position += 2; IncludeMath }
        '|' if parser.starts_with("|=") => { parser.position += 2; DashMatch }
        '^' if parser.starts_with("^=") => { parser.position += 2; PrefixMatch }
        '$' if parser.starts_with("$=") => { parser.position += 2; SuffixMatch }
        '*' if parser.starts_with("*=") => { parser.position += 2; SubstringMatch }
        '|' if parser.starts_with("||") => { parser.position += 2; Column }
        _ if c >= '\x80' => consume_ident(parser),  // Non-ASCII
        _ => {
            match parser.consume_char() {
                '\t' | '\n' | ' ' => {
                    while !parser.is_eof() {
                        match parser.current_char() {
                            '\t' | '\n' | ' '
                                => parser.position += 1,
                            _ => break,
                        }
                    }
                    WhiteSpace
                },
                '"' => consume_quoted_string(parser, false),
                '#' => consume_hash(parser),
                '\'' => consume_quoted_string(parser, true),
                '(' => ParenthesisBlock(consume_block(parser, CloseParenthesis)),
                ')' => CloseParenthesis,
                ':' => Colon,
                ';' => Semicolon,
                '@' => consume_at_keyword(parser),
                '[' => SquareBraketBlock(consume_block(parser, CloseSquareBraket)),
                ']' => CloseSquareBraket,
                '{' => CurlyBraketBlock(consume_block(parser, CloseCurlyBraket)),
                '}' => CloseCurlyBraket,
                _ => Delim(c)
            }
        }
    })
}


//  ***********  End of public API  ***********


#[inline]
fn preprocess(input: &str) -> ~str {
    // TODO: Is this faster if done in one pass?
    input.replace("\r\n", "\n").replace("\r", "\n").replace("\x0C", "\n").replace("\x00", "\uFFFD")
}


#[test]
fn test_preprocess() {
    assert!(preprocess("") == ~"");
    assert!(preprocess("Lorem\r\n\t\x00ipusm\ndoror\uFFFD\r")
            == ~"Lorem\n\t\uFFFDipusm\ndoror\uFFFD\n");
}


impl Parser {
    #[inline]
    fn is_eof(&self) -> bool { self.position >= self.length }

    // Assumes non-EOF
    #[inline]
    fn current_char(&self) -> char { self.char_at(0) }

    #[inline]
    fn char_at(&self, offset: uint) -> char {
        self.input.char_at(self.position + offset)
    }

    #[inline]
    fn consume_char(&mut self) -> char {
        let range = self.input.char_range_at(self.position);
        self.position = range.next;
        range.ch
    }

    // Return value may be smaller than n if we’re near the end of the input.
    #[inline]
    fn next_n_chars(&mut self, n: uint) -> ~[char] {
        let mut chars: ~[char] = ~[];
        let mut position = self.position;
        for n.times {
            if position >= self.length { break }
            let range = self.input.char_range_at(position);
            position = range.next;
            chars.push(range.ch);
        }
        chars
    }

    #[inline]
    fn starts_with(&self, needle: &str) -> bool {
        self.input.slice_from(self.position).starts_with(needle)
    }
}


#[inline]
fn consume_comments(parser: &mut Parser) {
    while parser.starts_with("/*") {
        parser.position += 2;  // +2 to consume "/*"
        match parser.input.slice_from(parser.position).find_str("*/") {
            // +2 to consume "*/"
            Some(offset) => parser.position += offset + 2,
            None => parser.position = parser.length  // EOF
        }
    }
}


fn consume_block(parser: &mut Parser, ending_token: ComponentValue) -> ~[ComponentValue] {
    let mut content = ~[];
    loop {
        match next_component_value(parser) {
            Some(c) => if c == ending_token { break } else { content.push(c) },
            None => break,
        }
    }
    content
}


#[inline]
fn is_invalid_escape(parser: &mut Parser) -> bool {
    parser.starts_with("\\\n")
}


#[inline]
fn is_namestart_or_escape(parser: &mut Parser) -> bool {
    match parser.current_char() {
        'a'..'z' | 'A'..'Z' | '_' => true,
        '\\' => !is_invalid_escape(parser),
        c => c >= '\x80',  // Non-ASCII
    }
}


#[inline]
fn next_is_namestart_or_escape(parser: &mut Parser) -> bool {
    parser.position += 1;
    let result = !parser.is_eof() && is_namestart_or_escape(parser);
    parser.position -= 1;
    result
}


fn consume_quoted_string(parser: &mut Parser, single_quote: bool) -> ComponentValue {
    let mut string: ~str = ~"";
    while !parser.is_eof() {
        match parser.consume_char() {
            '"' if !single_quote => break,
            '\'' if single_quote => break,
            '\n' => {
                parser.position -= 1;
                return BadString;
            },
            '\\' => {
                if !parser.is_eof() {
                    if parser.current_char() == '\n' { parser.position += 1 }  // Escaped newline
                    else { string.push_char(consume_escape(parser)) }
                }
                // else: escaped EOF, do nothing.
            }
            c => string.push_char(c),
        }
    }
    String(string)
}


fn consume_hash(parser: &mut Parser) -> ComponentValue {
    match consume_ident_string(parser) {
        Some(string) => IDHash(string),
        None => match consume_ident_string_rest(parser) {
            ~"" => Delim('#'),
            string => Hash(string),
        }
    }
}


fn consume_at_keyword(parser: &mut Parser) -> ComponentValue {
    match consume_ident_string(parser) {
        Some(string) => AtKeyword(string),
        None => Delim('@')
    }
}


fn consume_ident(parser: &mut Parser) -> ComponentValue {
    match consume_ident_string(parser) {
        Some(string) => {
            if parser.is_eof() { return Ident(string) }
            match parser.current_char() {
                '(' => {
                    parser.position += 1;
                    if eq_ascii_lower(string, "url") { consume_url(parser) }
                    else { Function(string, consume_block(parser, CloseParenthesis)) }
                },
                _ => Ident(string)
            }
        },
        None => match parser.current_char() {
            '-' => {
                parser.position += 1;
                Delim('-')
            },
            '\\' => {
                parser.position += 1;
                Delim('\\')
            },
            _ => fail!(),  // Should not have called consume_ident() here.
        }
    }
}

fn consume_ident_string(parser: &mut Parser) -> Option<~str> {
    match parser.current_char() {
        '-' => if !next_is_namestart_or_escape(parser) { None }
               else { Some(consume_ident_string_rest(parser)) },
        '\\' if is_invalid_escape(parser) => return None,
        _ if !is_namestart_or_escape(parser) => return None,
        _ => Some(consume_ident_string_rest(parser))
    }
}


fn consume_ident_string_rest(parser: &mut Parser) -> ~str {
    let mut string = ~"";
    while !parser.is_eof() {
        let c = parser.current_char();
        let next_char = match c {
            'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '-'  => {
                parser.position += 1; c },
            _ if c >= '\x80' => parser.consume_char(),  // Non-ASCII
            '\\' => {
                if is_invalid_escape(parser) { break }
                parser.position += 1;
                consume_escape(parser)
            },
            _ => break
        };
        string.push_char(next_char)
    }
    string
}


fn consume_numeric(parser: &mut Parser) -> ComponentValue {
    // Parse [+-]?\d*(\.\d+)?([eE][+-]?\d+)?
    // But this is always called so that there is at least one digit in \d*(\.\d+)?
    let mut representation = ~"";
    let mut is_integer = true;
    if is_match!(parser.current_char(), '-' | '+') {
         representation.push_char(parser.consume_char())
    }
    while !parser.is_eof() {
        match parser.current_char() {
            '0'..'9' => representation.push_char(parser.consume_char()),
            _ => break
        }
    }
    if parser.position + 1 < parser.length && parser.current_char() == '.'
            && is_match!(parser.char_at(1), '0'..'9') {
        is_integer = false;
        representation.push_char(parser.consume_char());  // '.'
        representation.push_char(parser.consume_char());  // digit
        while !parser.is_eof() {
            match parser.current_char() {
                '0'..'9' => representation.push_char(parser.consume_char()),
                _ => break
            }
        }
    }
    if (
        parser.position + 1 < parser.length
        && is_match!(parser.current_char(), 'e' | 'E')
        && is_match!(parser.char_at(1), '0'..'9')
    ) || (
        parser.position + 2 < parser.length
        && is_match!(parser.current_char(), 'e' | 'E')
        && is_match!(parser.char_at(1), '+' | '-')
        && is_match!(parser.char_at(2), '0'..'9')
    ) {
        is_integer = false;
        representation.push_char(parser.consume_char());  // 'e' or 'E'
        representation.push_char(parser.consume_char());  // sign or digit
        // If the above was a sign, the first digit it consumed below
        // and we make one extraneous is_eof() check.
        while !parser.is_eof() {
            match parser.current_char() {
                '0'..'9' => representation.push_char(parser.consume_char()),
                _ => break
            }
        }
    }
    // TODO: handle overflow
    let value = NumericValue {
        int_value: if is_integer { Some(
            // Remove any + sign as int::from_str() does not parse them.
            if representation[0] != '+' as u8 {
                i32::from_str(representation)
            } else {
                i32::from_str(representation.slice_from(1))
            }.get()
        )} else { None },
        value: f64::from_str(representation).get(),
        representation: representation,
    };
    if parser.is_eof() { return Number(value) }
    match parser.current_char() {
        '%' => { parser.position += 1; Percentage(value) },
        _ => {
            match consume_ident_string(parser) {
                Some(unit) => Dimension(value, unit),
                None => Number(value),
            }
        },
    }
}


fn consume_url(parser: &mut Parser) -> ComponentValue {
    while !parser.is_eof() {
        match parser.current_char() {
            '\t' | '\n' | ' ' => parser.position += 1,
            '"' => return consume_quoted_url(parser, false),
            '\'' => return consume_quoted_url(parser, true),
            ')' => { parser.position += 1; break },
            _ => return consume_unquoted_url(parser),
        }
    }
    URL(~"")
}


fn consume_quoted_url(parser: &mut Parser, single_quote: bool)
        -> ComponentValue {
    parser.position += 1;  // The initial quote
    match consume_quoted_string(parser, single_quote) {
        String(string) => consume_url_end(parser, string),
        BadString => consume_bad_url(parser),
        // consume_quoted_string() only returns String or BadString
        _ => fail!(),
    }
}


fn consume_url_end(parser: &mut Parser, string: ~str)
        -> ComponentValue {
    while !parser.is_eof() {
        match parser.consume_char() {
            '\t' | '\n' | ' ' => (),
            ')' => break,
            _ => return consume_bad_url(parser)
        }
    }
    URL(string)
}


fn consume_unquoted_url(parser: &mut Parser) -> ComponentValue {
    let mut string = ~"";
    while !parser.is_eof() {
        let next_char = match parser.consume_char() {
            '\t' | '\n' | ' '
                => return consume_url_end(parser, string),
            ')' => break,
            '\x00'..'\x08' | '\x0B' | '\x0E'..'\x1F' | '\x7F'  // non-printable
                | '"' | '\'' | '(' => return consume_bad_url(parser),
            '\\' => {
                if !parser.is_eof() && parser.current_char() == '\n' {
                    return consume_bad_url(parser)
                }
                consume_escape(parser)
            },
            c => c
        };
        string.push_char(next_char)
    }
    URL(string)
}


fn consume_bad_url(parser: &mut Parser) -> ComponentValue {
    // Consume up to the closing )
    while !parser.is_eof() {
        match parser.consume_char() {
            ')' => break,
            '\\' => parser.position += 1, // Skip an escaped ')' or '\'
            _ => ()
        }
    }
    BadURL
}


fn consume_unicode_range(parser: &mut Parser)
        -> ComponentValue {
    let next_3 = parser.next_n_chars(3);
    // We got here with U or u
    assert!(next_3[0] == 'u' || next_3[0] == 'U');
    // Check if this is indeed an unicode range. Fallback on ident.
    if next_3.len() == 3 && next_3[1] == '+' {
        match next_3[2] {
            '0'..'9' | 'a'..'f' | 'A'..'F' => parser.position += 2,
            _ => { return consume_ident(parser) }
        }
    } else { return consume_ident(parser) }

    let mut hex = ~[];
    while hex.len() < 6 && !parser.is_eof() {
        let c = parser.current_char();
        match c {
            '0'..'9' | 'A'..'F' | 'a'..'f' => {
                hex.push(c); parser.position += 1 },
            _ => break
        }
    }
    assert!(hex.len() > 0);
    let max_question_marks = 6u - hex.len();
    let mut question_marks = 0u;
    while question_marks < max_question_marks && !parser.is_eof()
            && parser.current_char() == '?' {
        question_marks += 1;
        parser.position += 1
    }
    let start: char;
    let end: char;
    if question_marks > 0 {
        start = char_from_hex(hex + vec::from_elem(question_marks, '0'));
        end = char_from_hex(hex + vec::from_elem(question_marks, 'F'));
    } else {
        start = char_from_hex(hex);
        hex = ~[];
        if !parser.is_eof() && parser.current_char() == '-' {
            parser.position += 1;
            while hex.len() < 6 && !parser.is_eof() {
                let c = parser.current_char();
                match c {
                    '0'..'9' | 'A'..'F' | 'a'..'f' => {
                        hex.push(c); parser.position += 1 },
                    _ => break
                }
            }
        }
        end = if hex.len() > 0 { char_from_hex(hex) } else { start }
    }
    if start > MAX_UNICODE || end < start {
        EmptyUnicodeRange
    } else {
        let end = if end <= MAX_UNICODE { end } else { MAX_UNICODE };
//        UnicodeRange {start: start, end: end}
        UnicodeRange(start, end)
    }
}


static MAX_UNICODE: char = '\U0010FFFF';


// Assumes that the U+005C REVERSE SOLIDUS (\) has already been consumed
// and that the next input character has already been verified
// to not be a newline.
fn consume_escape(parser: &mut Parser) -> char {
    if parser.is_eof() { return '\uFFFD' }  // Escaped EOF
    let c = parser.consume_char();
    match c {
        '0'..'9' | 'A'..'F' | 'a'..'f' => {
            let mut hex = ~[c];
            while hex.len() < 6 && !parser.is_eof() {
                let c = parser.current_char();
                match c {
                    '0'..'9' | 'A'..'F' | 'a'..'f' => {
                        hex.push(c); parser.position += 1 },
                    _ => break
                }
            }
            if !parser.is_eof() {
                match parser.current_char() {
                    '\t' | '\n' | ' ' => parser.position += 1,
                    _ => ()
                }
            }
            let c = char_from_hex(hex);
            if '\x00' < c && c <= MAX_UNICODE { c }
            else { '\uFFFD' }  // Replacement character
        },
        c => c
    }
}


#[inline]
fn char_from_hex(hex: &[char]) -> char {
    u32::from_str_radix(str::from_chars(hex), 16).get() as char
}
