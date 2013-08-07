/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

// http://dev.w3.org/csswg/css3-syntax/#tokenization

use std::{str, u32, i64, f64};
use std::ascii::eq_ignore_ascii_case;

use ast::*;


struct Parser {
    input: ~str,
    length: uint,  // All counted in bytes, not characters
    position: uint,  // All counted in bytes, not characters
    line: uint,
    last_line_start: uint,  // All counted in bytes, not characters
}


impl Parser {
    pub fn from_str(input: &str) -> Parser {
        let input = preprocess(input);
        Parser {
            length: input.len(),
            input: input,
            position: 0,
            line: 1,
            last_line_start: 0,
        }
    }
}


macro_rules! is_match(
    ($value:expr, $($pattern:pat)|+) => (
        match $value { $($pattern)|+ => true, _ => false }
    );
)


pub fn next_component_value(parser: &mut Parser) -> Option<(ComponentValue, SourceLocation)> {
    consume_comments(parser);
    if parser.is_eof() {
        if CFG_TEST {
            assert!(parser.line == parser.input.split_iter('\n').len_(),
                    "The tokenizer is missing a parser.new_line() call somewhere.")
        }
        return None
    }
    let start_location = SourceLocation{
        line: parser.line,
        // The start of the line is column 1:
        column: parser.position - parser.last_line_start + 1,
    };
    let c = parser.current_char();
    let component_value = match c {
        '\t' | '\n' | ' ' => {
            while !parser.is_eof() {
                match parser.current_char() {
                    ' ' | '\t' => parser.position += 1,
                    '\n' => {
                        parser.position += 1;
                        parser.new_line();
                    },
                    _ => break,
                }
            }
            WhiteSpace
        },
        '"' => consume_string(parser, false),
        '#' => {
            parser.position += 1;
            if is_ident_start(parser) { IDHash(consume_name(parser)) }
            else if !parser.is_eof() && match parser.current_char() {
                'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' => true,
                '\\' => !parser.starts_with("\\\n"),
                _ => c > '\x7F',  // Non-ASCII
            } { Hash(consume_name(parser)) }
            else { Delim(c) }
        },
        '$' => {
            if parser.starts_with("$=") { parser.position += 2; SuffixMatch }
            else { parser.position += 1; Delim(c) }
        },
        '\'' => consume_string(parser, true),
        '(' => ParenthesisBlock(consume_block(parser, CloseParenthesis)),
        ')' => { parser.position += 1; CloseParenthesis },
        '*' => {
            if parser.starts_with("*=") { parser.position += 2; SubstringMatch }
            else { parser.position += 1; Delim(c) }
        },
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
                Delim(c)
            }
        },
        ',' => { parser.position += 1; Comma },
        '-' => {
            if (
                parser.position + 1 < parser.length
                && is_match!(parser.char_at(1), '0'..'9')
            ) || (
                parser.position + 2 < parser.length
                && parser.char_at(1) == '.'
                && is_match!(parser.char_at(2), '0'..'9')
            ) {
                consume_numeric(parser)
            } else if is_ident_start(parser) {
                consume_ident_like(parser)
            } else if parser.starts_with("-->") {
                parser.position += 3;
                CDC
            } else {
                parser.position += 1;
                Delim(c)
            }
        },
        '.' => {
            if parser.position + 1 < parser.length && is_match!(parser.char_at(1), '0'..'9') {
                consume_numeric(parser)
            } else {
                parser.position += 1;
                Delim(c)
            }
        }
        '0'..'9' => consume_numeric(parser),
        ':' => { parser.position += 1; Colon },
        ';' => { parser.position += 1; Semicolon },
        '<' => {
            if parser.starts_with("<!--") {
                parser.position += 4;
                CDO
            } else {
                parser.position += 1;
                Delim(c)
            }
        },
        '@' => {
            parser.position += 1;
            if is_ident_start(parser) { AtKeyword(consume_name(parser)) }
            else { Delim(c) }
        },
        'u' | 'U' => {
            if parser.position + 2 < parser.length
               && parser.char_at(1) == '+'
               && is_match!(parser.char_at(2), '0'..'9' | 'a'..'f' | 'A'..'F' | '?')
            { consume_unicode_range(parser) }
            else { consume_ident_like(parser) }
        },
        'a'..'z' | 'A'..'Z' | '_' => consume_ident_like(parser),
        '[' => SquareBracketBlock(consume_block(parser, CloseSquareBracket)),
        '\\' => {
            if !parser.starts_with("\\\n") { consume_ident_like(parser) }
            else { parser.position += 1; Delim(c) }
        },
        ']' => { parser.position += 1; CloseSquareBracket },
        '^' => {
            if parser.starts_with("^=") { parser.position += 2; PrefixMatch }
            else { parser.position += 1; Delim(c) }
        },
        '{' => CurlyBracketBlock(consume_block(parser, CloseCurlyBracket)),
        '|' => {
            if parser.starts_with("|=") { parser.position += 2; DashMatch }
            else if parser.starts_with("||") { parser.position += 2; Column }
            else { parser.position += 1; Delim(c) }
        },
        '}' => { parser.position += 1; CloseCurlyBracket },
        '~' => {
            if parser.starts_with("~=") { parser.position += 2; IncludeMath }
            else { parser.position += 1; Delim(c) }
        },
        _ => {
            if c > '\x7F' {  // Non-ASCII
                consume_ident_like(parser)
            } else {
                parser.position += 1;
                Delim(c)
            }
        },
    };
    Some((component_value, start_location))
}


//  ***********  End of public API  ***********


#[cfg(not(test))]
static CFG_TEST: bool = false;

#[cfg(test)]
static CFG_TEST: bool = true;


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

    #[inline]
    fn starts_with(&self, needle: &str) -> bool {
        self.input.slice_from(self.position).starts_with(needle)
    }

    #[inline]
    fn new_line(&mut self) {
        if CFG_TEST {
            assert!(self.input.char_at(self.position - 1) == '\n')
        }
        self.line += 1;
        self.last_line_start = self.position;
    }
}


#[inline]
fn consume_comments(parser: &mut Parser) {
    while parser.starts_with("/*") {
        parser.position += 2;  // +2 to consume "/*"
        while !parser.is_eof() {
            match parser.consume_char() {
                '*' => {
                    if !parser.is_eof() && parser.current_char() == '/' {
                        parser.position += 1;
                        break
                    }
                },
                '\n' => parser.new_line(),
                _ => ()
            }
        }
    }
}


fn consume_block(parser: &mut Parser, ending_token: ComponentValue)
                 -> ~[(ComponentValue, SourceLocation)] {
    parser.position += 1;  // Skip the initial {[(
    let mut content = ~[];
    loop {
        match next_component_value(parser) {
            Some((component_value, location)) => {
                if component_value == ending_token { break }
                else { content.push((component_value, location)) }
            },
            None => break,
        }
    }
    content
}


fn consume_string(parser: &mut Parser, single_quote: bool) -> ComponentValue {
    match consume_quoted_string(parser, single_quote) {
        Some(value) => String(value),
        None => BadString
    }
}


// Return None on syntax error (ie. unescaped newline)
fn consume_quoted_string(parser: &mut Parser, single_quote: bool) -> Option<~str> {
    parser.position += 1;  // Skip the initial quote
    let mut string: ~str = ~"";
    while !parser.is_eof() {
        match parser.consume_char() {
            '"' if !single_quote => break,
            '\'' if single_quote => break,
            '\n' => {
                parser.position -= 1;
                return None;
            },
            '\\' => {
                if !parser.is_eof() {
                    if parser.current_char() == '\n' {  // Escaped newline
                        parser.position += 1;
                        parser.new_line();
                    }
                    else { string.push_char(consume_escape(parser)) }
                }
                // else: escaped EOF, do nothing.
            }
            c => string.push_char(c),
        }
    }
    Some(string)
}


#[inline]
fn is_ident_start(parser: &mut Parser) -> bool {
    !parser.is_eof() && match parser.current_char() {
        'a'..'z' | 'A'..'Z' | '_' => true,
        '-' => parser.position + 1 < parser.length && match parser.char_at(1) {
            'a'..'z' | 'A'..'Z' | '_' => true,
            '\\' => !parser.input.slice_from(parser.position + 1).starts_with("\\\n"),
            c => c > '\x7F',  // Non-ASCII
        },
        '\\' => !parser.starts_with("\\\n"),
        c => c > '\x7F',  // Non-ASCII
    }
}


fn consume_ident_like(parser: &mut Parser) -> ComponentValue {
    let value = consume_name(parser);
    if !parser.is_eof() && parser.current_char() == '(' {
        if eq_ignore_ascii_case(value, "url") { consume_url(parser) }
        else { Function(value, consume_block(parser, CloseParenthesis)) }
    } else {
        Ident(value)
    }
}

fn consume_name(parser: &mut Parser) -> ~str {
    let mut value = ~"";
    while !parser.is_eof() {
        let c = parser.current_char();
        value.push_char(match c {
            'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '-'  => { parser.position += 1; c },
            '\\' => {
                if parser.starts_with("\\\n") { break }
                parser.position += 1;
                consume_escape(parser)
            },
            _ => if c > '\x7F' { parser.consume_char() }  // Non-ASCII
                 else { break }
        })
    }
    value
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
                i64::from_str(representation)
            } else {
                i64::from_str(representation.slice_from(1))
            }.unwrap()
        )} else { None },
        value: f64::from_str(representation).unwrap(),
        representation: representation,
    };
    if !parser.is_eof() && parser.current_char() == '%' {
        parser.position += 1;
        Percentage(value)
    }
    else if is_ident_start(parser) { Dimension(value, consume_name(parser)) }
    else { Number(value) }
}


fn consume_url(parser: &mut Parser) -> ComponentValue {
    parser.position += 1;  // Skip the ( of url(
    while !parser.is_eof() {
        match parser.current_char() {
            '\t' | '\n' | ' ' => parser.position += 1,
            '"' => return consume_quoted_url(parser, false),
            '\'' => return consume_quoted_url(parser, true),
            ')' => { parser.position += 1; break },
            _ => return consume_unquoted_url(parser),
        }
    }
    return URL(~"");

    fn consume_quoted_url(parser: &mut Parser, single_quote: bool) -> ComponentValue {
        match consume_quoted_string(parser, single_quote) {
            Some(value) => consume_url_end(parser, value),
            None => consume_bad_url(parser),
        }
    }

    fn consume_unquoted_url(parser: &mut Parser) -> ComponentValue {
        let mut string = ~"";
        while !parser.is_eof() {
            let next_char = match parser.consume_char() {
                ' ' | '\t' => return consume_url_end(parser, string),
                '\n' => {
                    parser.new_line();
                    return consume_url_end(parser, string)
                },
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

    fn consume_url_end(parser: &mut Parser, string: ~str) -> ComponentValue {
        while !parser.is_eof() {
            match parser.consume_char() {
                ' ' | '\t' => (),
                '\n' => parser.new_line(),
                ')' => break,
                _ => return consume_bad_url(parser)
            }
        }
        URL(string)
    }

    fn consume_bad_url(parser: &mut Parser) -> ComponentValue {
        // Consume up to the closing )
        while !parser.is_eof() {
            match parser.consume_char() {
                ')' => break,
                '\\' => parser.position += 1, // Skip an escaped ')' or '\'
                '\n' => parser.new_line(),
                _ => ()
            }
        }
        BadURL
    }
}



fn consume_unicode_range(parser: &mut Parser) -> ComponentValue {
    parser.position += 2;  // Skip U+
    let mut hex = ~"";
    while hex.len() < 6 && !parser.is_eof()
          && is_match!(parser.current_char(), '0'..'9' | 'A'..'F' | 'a'..'f') {
        hex.push_char(parser.consume_char());
    }
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
        start = char_from_hex(hex + "0".repeat(question_marks));
        end = char_from_hex(hex + "F".repeat(question_marks));
    } else {
        start = char_from_hex(hex);
        hex = ~"";
        if !parser.is_eof() && parser.current_char() == '-' {
            parser.position += 1;
            while hex.len() < 6 && !parser.is_eof() {
                let c = parser.current_char();
                match c {
                    '0'..'9' | 'A'..'F' | 'a'..'f' => {
                        hex.push_char(c); parser.position += 1 },
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
            let mut hex = str::from_char(c);
            while hex.len() < 6 && !parser.is_eof() {
                let c = parser.current_char();
                match c {
                    '0'..'9' | 'A'..'F' | 'a'..'f' => {
                        hex.push_char(c); parser.position += 1 },
                    _ => break
                }
            }
            if !parser.is_eof() {
                match parser.current_char() {
                    ' ' | '\t' => parser.position += 1,
                    '\n' => { parser.position += 1; parser.new_line() },
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
fn char_from_hex(hex: &str) -> char {
    u32::from_str_radix(hex, 16).unwrap() as char
}
