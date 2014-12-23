/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

// http://dev.w3.org/csswg/css3-syntax/#tokenization

use std::{char, num};
use std::ascii::AsciiExt;

use self::Token::*;


#[deriving(PartialEq, Show)]
pub enum Token {
    // Preserved tokens.
    Ident(String),
    AtKeyword(String),
    Hash(String),
    IDHash(String),  // Hash that is a valid ID selector.
    QuotedString(String),
    Url(String),
    Delim(char),
    Number(NumericValue),
    Percentage(NumericValue),
    Dimension(NumericValue, String),
    UnicodeRange(u32, u32),  // (start, end) of range
    WhiteSpace,
    Colon,  // :
    Semicolon,  // ;
    Comma,  // ,
    IncludeMatch, // ~=
    DashMatch, // |=
    PrefixMatch, // ^=
    SuffixMatch, // $=
    SubstringMatch, // *=
    Column, // ||
    CDO,  // <!--
    CDC,  // -->

    // Function
    Function(String),  // name

    // Simple block
    ParenthesisBlock,  // (…)
    SquareBracketBlock,  // […]
    CurlyBracketBlock,  // {…}

    // These are always invalid
    BadUrl,
    BadString,
    CloseParenthesis, // )
    CloseSquareBracket, // ]
    CloseCurlyBracket, // }
}


#[deriving(PartialEq, Show)]
pub struct NumericValue {
    pub representation: String,
    pub value: f64,
    pub int_value: Option<i64>,
}


pub struct Tokenizer<'a> {
    input: &'a str,
    length: uint,  // All counted in bytes, not characters
    position: uint,  // All counted in bytes, not characters

    /// For `peek` and `push_back`
    buffer: Option<Token>,
}

macro_rules! is_match(
    ($value:expr, $($pattern:pat)|+) => (
        match $value { $($pattern)|+ => true, _ => false }
    );
)


impl<'a> Tokenizer<'a> {
    #[inline]
    pub fn new(input: &str) -> Tokenizer {
        Tokenizer {
            length: input.len(),
            input: input,
            position: 0,
            buffer: None,
        }
    }

    #[inline]
    pub fn next(&mut self) -> Result<Token, ()> {
        if let Some(token) = self.buffer.take() {
            Ok(token)
        } else {
            next_token(self).ok_or(())
        }
    }

    #[inline]
    pub fn peek(&mut self) -> Result<&Token, ()> {
        match self.buffer {
            Some(ref token) => Ok(token),
            None => {
                self.buffer = next_token(self);
                self.buffer.as_ref().ok_or(())
            }
        }
    }

    #[inline]
    pub fn push_back(&mut self, token: Token) {
        assert!(self.buffer.is_none(),
                "Parser::push_back can only be called after Parser::next");
        self.buffer = Some(token);
    }

    #[inline]
    fn is_eof(&self) -> bool { self.position >= self.length }

    // Assumes non-EOF
    #[inline]
    fn current_char(&self) -> char { self.char_at(0) }

    #[inline]
    fn char_at(&self, offset: uint) -> char {
        self.input.as_slice().char_at(self.position + offset)
    }

    #[inline]
    fn has_newline_at(&self, offset: uint) -> bool {
        self.position + offset < self.length &&
        is_match!(self.char_at(offset), '\n' | '\r' | '\x0C')
    }

    #[inline]
    fn consume_char(&mut self) -> char {
        let range = self.input.as_slice().char_range_at(self.position);
        self.position = range.next;
        range.ch
    }

    #[inline]
    fn starts_with(&self, needle: &str) -> bool {
        self.input.as_slice().slice_from(self.position).starts_with(needle)
    }
}


fn next_token(tokenizer: &mut Tokenizer) -> Option<Token> {
    consume_comments(tokenizer);
    if tokenizer.is_eof() {
        return None
    }
    let c = tokenizer.current_char();
    let token = match c {
        '\t' | '\n' | ' ' | '\r' | '\x0C' => {
            while !tokenizer.is_eof() {
                match tokenizer.current_char() {
                    ' ' | '\t' | '\n' | '\r' | '\x0C' => tokenizer.position += 1,
                    _ => break,
                }
            }
            WhiteSpace
        },
        '"' => consume_string(tokenizer, false),
        '#' => {
            tokenizer.position += 1;
            if is_ident_start(tokenizer) { IDHash(consume_name(tokenizer)) }
            else if !tokenizer.is_eof() && match tokenizer.current_char() {
                'a'...'z' | 'A'...'Z' | '0'...'9' | '-' | '_' => true,
                '\\' => !tokenizer.has_newline_at(1),
                _ => c > '\x7F',  // Non-ASCII
            } { Hash(consume_name(tokenizer)) }
            else { Delim(c) }
        },
        '$' => {
            if tokenizer.starts_with("$=") { tokenizer.position += 2; SuffixMatch }
            else { tokenizer.position += 1; Delim(c) }
        },
        '\'' => consume_string(tokenizer, true),
        '(' => { tokenizer.position += 1; ParenthesisBlock },
        ')' => { tokenizer.position += 1; CloseParenthesis },
        '*' => {
            if tokenizer.starts_with("*=") { tokenizer.position += 2; SubstringMatch }
            else { tokenizer.position += 1; Delim(c) }
        },
        '+' => {
            if (
                tokenizer.position + 1 < tokenizer.length
                && is_match!(tokenizer.char_at(1), '0'...'9')
            ) || (
                tokenizer.position + 2 < tokenizer.length
                && tokenizer.char_at(1) == '.'
                && is_match!(tokenizer.char_at(2), '0'...'9')
            ) {
                consume_numeric(tokenizer)
            } else {
                tokenizer.position += 1;
                Delim(c)
            }
        },
        ',' => { tokenizer.position += 1; Comma },
        '-' => {
            if (
                tokenizer.position + 1 < tokenizer.length
                && is_match!(tokenizer.char_at(1), '0'...'9')
            ) || (
                tokenizer.position + 2 < tokenizer.length
                && tokenizer.char_at(1) == '.'
                && is_match!(tokenizer.char_at(2), '0'...'9')
            ) {
                consume_numeric(tokenizer)
            } else if tokenizer.starts_with("-->") {
                tokenizer.position += 3;
                CDC
            } else if is_ident_start(tokenizer) {
                consume_ident_like(tokenizer)
            } else {
                tokenizer.position += 1;
                Delim(c)
            }
        },
        '.' => {
            if tokenizer.position + 1 < tokenizer.length
                && is_match!(tokenizer.char_at(1), '0'...'9'
            ) {
                consume_numeric(tokenizer)
            } else {
                tokenizer.position += 1;
                Delim(c)
            }
        }
        '0'...'9' => consume_numeric(tokenizer),
        ':' => { tokenizer.position += 1; Colon },
        ';' => { tokenizer.position += 1; Semicolon },
        '<' => {
            if tokenizer.starts_with("<!--") {
                tokenizer.position += 4;
                CDO
            } else {
                tokenizer.position += 1;
                Delim(c)
            }
        },
        '@' => {
            tokenizer.position += 1;
            if is_ident_start(tokenizer) { AtKeyword(consume_name(tokenizer)) }
            else { Delim(c) }
        },
        'u' | 'U' => {
            if tokenizer.position + 2 < tokenizer.length
               && tokenizer.char_at(1) == '+'
               && is_match!(tokenizer.char_at(2), '0'...'9' | 'a'...'f' | 'A'...'F' | '?')
            { consume_unicode_range(tokenizer) }
            else { consume_ident_like(tokenizer) }
        },
        'a'...'z' | 'A'...'Z' | '_' | '\0' => consume_ident_like(tokenizer),
        '[' => { tokenizer.position += 1; SquareBracketBlock },
        '\\' => {
            if !tokenizer.has_newline_at(1) { consume_ident_like(tokenizer) }
            else { tokenizer.position += 1; Delim(c) }
        },
        ']' => { tokenizer.position += 1; CloseSquareBracket },
        '^' => {
            if tokenizer.starts_with("^=") { tokenizer.position += 2; PrefixMatch }
            else { tokenizer.position += 1; Delim(c) }
        },
        '{' => { tokenizer.position += 1; CurlyBracketBlock },
        '|' => {
            if tokenizer.starts_with("|=") { tokenizer.position += 2; DashMatch }
            else if tokenizer.starts_with("||") { tokenizer.position += 2; Column }
            else { tokenizer.position += 1; Delim(c) }
        },
        '}' => { tokenizer.position += 1; CloseCurlyBracket },
        '~' => {
            if tokenizer.starts_with("~=") { tokenizer.position += 2; IncludeMatch }
            else { tokenizer.position += 1; Delim(c) }
        },
        _ => {
            if c > '\x7F' {  // Non-ASCII
                consume_ident_like(tokenizer)
            } else {
                tokenizer.position += 1;
                Delim(c)
            }
        },
    };
    Some(token)
}


#[inline]
fn consume_comments(tokenizer: &mut Tokenizer) {
    while tokenizer.starts_with("/*") {
        tokenizer.position += 2;  // +2 to consume "/*"
        while !tokenizer.is_eof() {
            if tokenizer.consume_char() == '*' &&
               !tokenizer.is_eof() &&
               tokenizer.current_char() == '/' {
                tokenizer.position += 1;
                break
            }
        }
    }
}


fn consume_string(tokenizer: &mut Tokenizer, single_quote: bool) -> Token {
    match consume_quoted_string(tokenizer, single_quote) {
        Ok(value) => QuotedString(value),
        Err(()) => BadString
    }
}


/// Return `Err(())` on syntax error (ie. unescaped newline)
fn consume_quoted_string(tokenizer: &mut Tokenizer, single_quote: bool) -> Result<String, ()> {
    tokenizer.position += 1;  // Skip the initial quote
    let mut string = String::new();
    while !tokenizer.is_eof() {
        match tokenizer.consume_char() {
            '"' if !single_quote => break,
            '\'' if single_quote => break,
            '\n' | '\r' | '\x0C' => {
                tokenizer.position -= 1;
                return Err(());
            },
            '\\' => {
                if !tokenizer.is_eof() {
                    match tokenizer.current_char() {
                        // Escaped newline
                        '\n' | '\x0C' => tokenizer.position += 1,
                        '\r' => {
                            tokenizer.position += 1;
                            if !tokenizer.is_eof() && tokenizer.current_char() == '\n' {
                                tokenizer.position += 1;
                            }
                        }
                        _ => string.push(consume_escape(tokenizer))
                    }
                }
                // else: escaped EOF, do nothing.
            }
            '\0' => string.push('\uFFFD'),
            c => string.push(c),
        }
    }
    Ok(string)
}


#[inline]
fn is_ident_start(tokenizer: &mut Tokenizer) -> bool {
    !tokenizer.is_eof() && match tokenizer.current_char() {
        'a'...'z' | 'A'...'Z' | '_' | '\0' => true,
        '-' => tokenizer.position + 1 < tokenizer.length && match tokenizer.char_at(1) {
            'a'...'z' | 'A'...'Z' | '-' | '_' | '\0' => true,
            '\\' => !tokenizer.has_newline_at(1),
            c => c > '\x7F',  // Non-ASCII
        },
        '\\' => !tokenizer.has_newline_at(1),
        c => c > '\x7F',  // Non-ASCII
    }
}


fn consume_ident_like(tokenizer: &mut Tokenizer) -> Token {
    let value = consume_name(tokenizer);
    if !tokenizer.is_eof() && tokenizer.current_char() == '(' {
        tokenizer.position += 1;
        if value.eq_ignore_ascii_case("url") { consume_url(tokenizer) }
        else { Function(value) }
    } else {
        Ident(value)
    }
}

fn consume_name(tokenizer: &mut Tokenizer) -> String {
    let mut value = String::new();
    while !tokenizer.is_eof() {
        let c = tokenizer.current_char();
        value.push(match c {
            'a'...'z' | 'A'...'Z' | '0'...'9' | '_' | '-'  => { tokenizer.position += 1; c },
            '\\' => {
                if tokenizer.has_newline_at(1) { break }
                tokenizer.position += 1;
                consume_escape(tokenizer)
            },
            '\0' => { tokenizer.position += 1; '\uFFFD' },
            _ => if c > '\x7F' { tokenizer.consume_char() }  // Non-ASCII
                 else { break }
        })
    }
    value
}


fn consume_numeric(tokenizer: &mut Tokenizer) -> Token {
    // Parse [+-]?\d*(\.\d+)?([eE][+-]?\d+)?
    // But this is always called so that there is at least one digit in \d*(\.\d+)?
    let mut representation = String::new();
    let mut is_integer = true;
    if is_match!(tokenizer.current_char(), '-' | '+') {
         representation.push(tokenizer.consume_char())
    }
    while !tokenizer.is_eof() {
        match tokenizer.current_char() {
            '0'...'9' => representation.push(tokenizer.consume_char()),
            _ => break
        }
    }
    if tokenizer.position + 1 < tokenizer.length && tokenizer.current_char() == '.'
            && is_match!(tokenizer.char_at(1), '0'...'9') {
        is_integer = false;
        representation.push(tokenizer.consume_char());  // '.'
        representation.push(tokenizer.consume_char());  // digit
        while !tokenizer.is_eof() {
            match tokenizer.current_char() {
                '0'...'9' => representation.push(tokenizer.consume_char()),
                _ => break
            }
        }
    }
    if (
        tokenizer.position + 1 < tokenizer.length
        && is_match!(tokenizer.current_char(), 'e' | 'E')
        && is_match!(tokenizer.char_at(1), '0'...'9')
    ) || (
        tokenizer.position + 2 < tokenizer.length
        && is_match!(tokenizer.current_char(), 'e' | 'E')
        && is_match!(tokenizer.char_at(1), '+' | '-')
        && is_match!(tokenizer.char_at(2), '0'...'9')
    ) {
        is_integer = false;
        representation.push(tokenizer.consume_char());  // 'e' or 'E'
        representation.push(tokenizer.consume_char());  // sign or digit
        // If the above was a sign, the first digit it consumed below
        // and we make one extraneous is_eof() check.
        while !tokenizer.is_eof() {
            match tokenizer.current_char() {
                '0'...'9' => representation.push(tokenizer.consume_char()),
                _ => break
            }
        }
    }
    let (value, int_value) = {
        // TODO: handle overflow
        // Remove any + sign as int::from_str() does not parse them.
        let repr = if representation.starts_with("+") {
            representation.slice_from(1)
        } else {
            representation.as_slice()
        };
        (from_str::<f64>(repr).unwrap(), if is_integer {
            Some(from_str::<i64>(repr).unwrap())
        } else {
            None
        })
    };
    let value = NumericValue {
        int_value: int_value,
        value: value,
        representation: representation,
    };
    if !tokenizer.is_eof() && tokenizer.current_char() == '%' {
        tokenizer.position += 1;
        Percentage(value)
    }
    else if is_ident_start(tokenizer) { Dimension(value, consume_name(tokenizer)) }
    else { Number(value) }
}


fn consume_url(tokenizer: &mut Tokenizer) -> Token {
    while !tokenizer.is_eof() {
        match tokenizer.current_char() {
            ' ' | '\t' | '\n' | '\r' | '\x0C' => tokenizer.position += 1,
            '"' => return consume_quoted_url(tokenizer, false),
            '\'' => return consume_quoted_url(tokenizer, true),
            ')' => { tokenizer.position += 1; break },
            _ => return consume_unquoted_url(tokenizer),
        }
    }
    return Url(String::new());

    fn consume_quoted_url(tokenizer: &mut Tokenizer, single_quote: bool) -> Token {
        match consume_quoted_string(tokenizer, single_quote) {
            Ok(value) => consume_url_end(tokenizer, value),
            Err(()) => consume_bad_url(tokenizer),
        }
    }

    fn consume_unquoted_url(tokenizer: &mut Tokenizer) -> Token {
        let mut string = String::new();
        while !tokenizer.is_eof() {
            let next_char = match tokenizer.consume_char() {
                ' ' | '\t' | '\n' | '\r' | '\x0C' => return consume_url_end(tokenizer, string),
                ')' => break,
                '\x01'...'\x08' | '\x0B' | '\x0E'...'\x1F' | '\x7F'  // non-printable
                    | '"' | '\'' | '(' => return consume_bad_url(tokenizer),
                '\\' => {
                    if tokenizer.has_newline_at(0) {
                        return consume_bad_url(tokenizer)
                    }
                    consume_escape(tokenizer)
                },
                '\0' => '\uFFFD',
                c => c
            };
            string.push(next_char)
        }
        Url(string)
    }

    fn consume_url_end(tokenizer: &mut Tokenizer, string: String) -> Token {
        while !tokenizer.is_eof() {
            match tokenizer.consume_char() {
                ' ' | '\t' | '\n' | '\r' | '\x0C' => (),
                ')' => break,
                _ => return consume_bad_url(tokenizer)
            }
        }
        Url(string)
    }

    fn consume_bad_url(tokenizer: &mut Tokenizer) -> Token {
        // Consume up to the closing )
        while !tokenizer.is_eof() {
            match tokenizer.consume_char() {
                ')' => break,
                '\\' => tokenizer.position += 1, // Skip an escaped ')' or '\'
                _ => ()
            }
        }
        BadUrl
    }
}



fn consume_unicode_range(tokenizer: &mut Tokenizer) -> Token {
    tokenizer.position += 2;  // Skip U+
    let mut hex = String::new();
    while hex.len() < 6 && !tokenizer.is_eof()
          && is_match!(tokenizer.current_char(), '0'...'9' | 'A'...'F' | 'a'...'f') {
        hex.push(tokenizer.consume_char());
    }
    let max_question_marks = 6u - hex.len();
    let mut question_marks = 0u;
    while question_marks < max_question_marks && !tokenizer.is_eof()
            && tokenizer.current_char() == '?' {
        question_marks += 1;
        tokenizer.position += 1
    }
    let start;
    let end;
    if question_marks > 0 {
        start = num::from_str_radix((hex + "0".repeat(question_marks)).as_slice(), 16).unwrap();
        end = num::from_str_radix((hex + "F".repeat(question_marks)).as_slice(), 16).unwrap();
    } else {
        start = num::from_str_radix(hex.as_slice(), 16).unwrap();
        hex.truncate(0);
        if !tokenizer.is_eof() && tokenizer.current_char() == '-' {
            tokenizer.position += 1;
            while hex.len() < 6 && !tokenizer.is_eof() {
                let c = tokenizer.current_char();
                match c {
                    '0'...'9' | 'A'...'F' | 'a'...'f' => {
                        hex.push(c); tokenizer.position += 1 },
                    _ => break
                }
            }
        }
        end = if hex.len() > 0 { num::from_str_radix(hex.as_slice(), 16).unwrap() } else { start }
    }
    UnicodeRange(start, end)
}


// Assumes that the U+005C REVERSE SOLIDUS (\) has already been consumed
// and that the next input character has already been verified
// to not be a newline.
fn consume_escape(tokenizer: &mut Tokenizer) -> char {
    if tokenizer.is_eof() { return '\uFFFD' }  // Escaped EOF
    let c = tokenizer.consume_char();
    match c {
        '0'...'9' | 'A'...'F' | 'a'...'f' => {
            let mut hex = String::from_char(1, c);
            while hex.len() < 6 && !tokenizer.is_eof() {
                let c = tokenizer.current_char();
                match c {
                    '0'...'9' | 'A'...'F' | 'a'...'f' => {
                        hex.push(c); tokenizer.position += 1 },
                    _ => break
                }
            }
            if !tokenizer.is_eof() {
                match tokenizer.current_char() {
                    ' ' | '\t' | '\n' | '\x0C' => tokenizer.position += 1,
                    '\r' => {
                        tokenizer.position += 1;
                        if !tokenizer.is_eof() && tokenizer.current_char() == '\n' {
                            tokenizer.position += 1;
                        }
                    }
                    _ => ()
                }
            }
            static REPLACEMENT_CHAR: char = '\uFFFD';
            let c: u32 = num::from_str_radix(hex.as_slice(), 16).unwrap();
            if c != 0 {
                let c = char::from_u32(c);
                c.unwrap_or(REPLACEMENT_CHAR)
            } else {
                REPLACEMENT_CHAR
            }
        },
        c => c
    }
}
