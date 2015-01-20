/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

// http://dev.w3.org/csswg/css3-syntax/#tokenization

use std::cell::Cell;
use std::char;
use std::num;
use std::ascii::AsciiExt;
use std::borrow::{Cow, ToOwned};
use std::str::CowString;
use std::borrow::Cow::{Owned, Borrowed};

use self::Token::*;


#[deriving(PartialEq, Show, Clone)]
pub enum Token<'a> {
    // Preserved tokens.
    Ident(CowString<'a>),
    AtKeyword(CowString<'a>),
    Hash(CowString<'a>),
    IDHash(CowString<'a>),  // Hash that is a valid ID selector.
    QuotedString(CowString<'a>),
    Url(CowString<'a>),
    Delim(char),
    Number(NumericValue),
    Percentage(PercentageValue),
    Dimension(NumericValue, CowString<'a>),
    UnicodeRange(u32, u32),  // (start, end) of range
    WhiteSpace,
    Comment,
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
    Function(CowString<'a>),  // name

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


impl<'a> Token<'a> {
    pub fn into_owned(self) -> Token<'static> {
        match self {
            Token::Ident(value) => Token::Ident(Cow::Owned(value.into_owned())),
            Token::AtKeyword(value) => Token::AtKeyword(Cow::Owned(value.into_owned())),
            Token::Hash(value) => Token::Hash(Cow::Owned(value.into_owned())),
            Token::IDHash(value) => Token::IDHash(Cow::Owned(value.into_owned())),
            Token::QuotedString(value) => Token::QuotedString(Cow::Owned(value.into_owned())),
            Token::Url(value) => Token::Url(Cow::Owned(value.into_owned())),
            Token::Delim(ch) => Token::Delim(ch),
            Token::Number(value) => Token::Number(value),
            Token::Percentage(value) => Token::Percentage(value),
            Token::Dimension(value, unit) => Token::Dimension(value, Cow::Owned(unit.into_owned())),
            Token::UnicodeRange(start, end) => Token::UnicodeRange(start, end),
            Token::WhiteSpace => Token::WhiteSpace,
            Token::Comment => Token::Comment,
            Token::Colon => Token::Colon,
            Token::Semicolon => Token::Semicolon,
            Token::Comma => Token::Comma,
            Token::IncludeMatch => Token::IncludeMatch,
            Token::DashMatch => Token::DashMatch,
            Token::PrefixMatch => Token::PrefixMatch,
            Token::SuffixMatch => Token::SuffixMatch,
            Token::SubstringMatch => Token::SubstringMatch,
            Token::Column => Token::Column,
            Token::CDO => Token::CDO,
            Token::CDC => Token::CDC,
            Token::Function(name) => Token::Function(Cow::Owned(name.into_owned())),
            Token::ParenthesisBlock => Token::ParenthesisBlock,
            Token::SquareBracketBlock => Token::SquareBracketBlock,
            Token::CurlyBracketBlock => Token::CurlyBracketBlock,
            Token::BadUrl => Token::BadUrl,
            Token::BadString => Token::BadString,
            Token::CloseParenthesis => Token::CloseParenthesis,
            Token::CloseSquareBracket => Token::CloseSquareBracket,
            Token::CloseCurlyBracket => Token::CloseCurlyBracket,
        }
    }
}


#[deriving(PartialEq, Show, Copy, Clone)]
pub struct NumericValue {
    pub value: f64,
    pub int_value: Option<i64>,
    /// Whether the number had a `+` or `-` sign.
    pub signed: bool,
}


#[deriving(PartialEq, Show, Copy, Clone)]
pub struct PercentageValue {
    /// This (but not int_value) is divided by 100
    pub unit_value: f64,
    pub int_value: Option<i64>,
    /// Whether the number had a `+` or `-` sign.
    pub signed: bool,
}


#[deriving(Clone)]
pub struct Tokenizer<'a> {
    input: &'a str,

    /// Counted in bytes, not code points. From 0.
    position: uint,

    /// Cache for `source_location()`
    last_known_line_break: Cell<(uint, uint)>,
}


impl<'a> Tokenizer<'a> {
    #[inline]
    pub fn new(input: &str) -> Tokenizer {
        Tokenizer {
            input: input,
            position: 0,
            last_known_line_break: Cell::new((1, 0)),
        }
    }

    #[inline]
    pub fn next(&mut self) -> Result<Token<'a>, ()> {
        next_token(self).ok_or(())
    }

    #[inline]
    pub fn position(&self) -> SourcePosition {
        SourcePosition(self.position)
    }

    #[inline]
    pub fn reset(&mut self, new_position: SourcePosition) {
        self.position = new_position.0;
    }

    #[inline]
    pub fn slice_from(&self, start_pos: SourcePosition) -> &'a str {
        self.input.slice(start_pos.0, self.position)
    }

    #[inline]
    pub fn current_source_location(&self) -> SourceLocation {
        let position = SourcePosition(self.position);
        self.source_location(position)
    }

    pub fn source_location(&self, position: SourcePosition) -> SourceLocation {
        let target = position.0;
        let mut line_number;
        let mut position;
        let (last_known_line_number, position_after_last_known_newline) =
            self.last_known_line_break.get();
        if target >= position_after_last_known_newline {
            position = position_after_last_known_newline;
            line_number = last_known_line_number;
        } else {
            position = 0;
            line_number = 1;
        }
        let mut source = self.input.slice(position, target);
        while let Some(newline_position) = source.find(['\n', '\r', '\x0C'].as_slice()) {
            let offset = newline_position +
            if source.slice_from(newline_position).starts_with("\r\n") {
                2
            } else {
                1
            };
            source = source.slice_from(offset);
            position += offset;
            line_number += 1;
        }
        debug_assert!(position <= target);
        self.last_known_line_break.set((line_number, position));
        SourceLocation {
            line: line_number,
            // `target == position` when `target` is at the beginning of the line,
            // so add 1 so that the column numbers start at 1.
            column: target - position + 1,
        }
    }

    #[inline]
    pub fn next_byte(&self) -> Option<u8> {
        if self.is_eof() {
            None
        } else {
            Some(self.input.as_bytes()[self.position])
        }
    }

    // If false, `tokenizer.next_char()` will not panic.
    #[inline]
    fn is_eof(&self) -> bool { !self.has_at_least(0) }

    // If true, the input has at least `n` bytes left *after* the current one.
    // That is, `tokenizer.char_at(n)` will not panic.
    #[inline]
    fn has_at_least(&self, n: uint) -> bool { self.position + n < self.input.len() }

    #[inline]
    pub fn advance(&mut self, n: uint) { self.position += n }

    // Assumes non-EOF
    #[inline]
    fn next_char(&self) -> char { self.char_at(0) }

    #[inline]
    fn char_at(&self, offset: uint) -> char {
        self.input.char_at(self.position + offset)
    }

    #[inline]
    fn has_newline_at(&self, offset: uint) -> bool {
        self.position + offset < self.input.len() &&
        matches!(self.char_at(offset), '\n' | '\r' | '\x0C')
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
}


#[deriving(PartialEq, Eq, PartialOrd, Ord, Show, Clone, Copy)]
pub struct SourcePosition(uint);


#[deriving(PartialEq, Eq, Show, Clone, Copy)]
pub struct SourceLocation {
    /// Starts at 1
    pub line: uint,
    /// Starts at 1
    pub column: uint,
}


fn next_token<'a>(tokenizer: &mut Tokenizer<'a>) -> Option<Token<'a>> {
    if tokenizer.is_eof() {
        return None
    }
    let c = tokenizer.next_char();
    let token = match c {
        '\t' | '\n' | ' ' | '\r' | '\x0C' => {
            while !tokenizer.is_eof() {
                match tokenizer.next_char() {
                    ' ' | '\t' | '\n' | '\r' | '\x0C' => tokenizer.advance(1),
                    _ => break,
                }
            }
            WhiteSpace
        },
        '"' => consume_string(tokenizer, false),
        '#' => {
            tokenizer.advance(1);
            if is_ident_start(tokenizer) { IDHash(consume_name(tokenizer)) }
            else if !tokenizer.is_eof() && match tokenizer.next_char() {
                'a'...'z' | 'A'...'Z' | '0'...'9' | '-' | '_' => true,
                '\\' => !tokenizer.has_newline_at(1),
                _ => c > '\x7F',  // Non-ASCII
            } { Hash(consume_name(tokenizer)) }
            else { Delim(c) }
        },
        '$' => {
            if tokenizer.starts_with("$=") { tokenizer.advance(2); SuffixMatch }
            else { tokenizer.advance(1); Delim(c) }
        },
        '\'' => consume_string(tokenizer, true),
        '(' => { tokenizer.advance(1); ParenthesisBlock },
        ')' => { tokenizer.advance(1); CloseParenthesis },
        '*' => {
            if tokenizer.starts_with("*=") { tokenizer.advance(2); SubstringMatch }
            else { tokenizer.advance(1); Delim(c) }
        },
        '+' => {
            if (
                tokenizer.has_at_least(1)
                && matches!(tokenizer.char_at(1), '0'...'9')
            ) || (
                tokenizer.has_at_least(2)
                && tokenizer.char_at(1) == '.'
                && matches!(tokenizer.char_at(2), '0'...'9')
            ) {
                consume_numeric(tokenizer)
            } else {
                tokenizer.advance(1);
                Delim(c)
            }
        },
        ',' => { tokenizer.advance(1); Comma },
        '-' => {
            if (
                tokenizer.has_at_least(1)
                && matches!(tokenizer.char_at(1), '0'...'9')
            ) || (
                tokenizer.has_at_least(2)
                && tokenizer.char_at(1) == '.'
                && matches!(tokenizer.char_at(2), '0'...'9')
            ) {
                consume_numeric(tokenizer)
            } else if tokenizer.starts_with("-->") {
                tokenizer.advance(3);
                CDC
            } else if is_ident_start(tokenizer) {
                consume_ident_like(tokenizer)
            } else {
                tokenizer.advance(1);
                Delim(c)
            }
        },
        '.' => {
            if tokenizer.has_at_least(1)
                && matches!(tokenizer.char_at(1), '0'...'9'
            ) {
                consume_numeric(tokenizer)
            } else {
                tokenizer.advance(1);
                Delim(c)
            }
        }
        '/' if tokenizer.starts_with("/*") => {
            tokenizer.advance(2);  // consume "/*"
            match tokenizer.input.slice_from(tokenizer.position).match_indices("*/").next() {
                Some((_start, end)) => {
                    tokenizer.advance(end)
                }
                None => {
                    tokenizer.position = tokenizer.input.len()
                }
            }
            Comment
        }
        '0'...'9' => consume_numeric(tokenizer),
        ':' => { tokenizer.advance(1); Colon },
        ';' => { tokenizer.advance(1); Semicolon },
        '<' => {
            if tokenizer.starts_with("<!--") {
                tokenizer.advance(4);
                CDO
            } else {
                tokenizer.advance(1);
                Delim(c)
            }
        },
        '@' => {
            tokenizer.advance(1);
            if is_ident_start(tokenizer) { AtKeyword(consume_name(tokenizer)) }
            else { Delim(c) }
        },
        'u' | 'U' => {
            if tokenizer.has_at_least(2)
               && tokenizer.char_at(1) == '+'
               && matches!(tokenizer.char_at(2), '0'...'9' | 'a'...'f' | 'A'...'F' | '?')
            { consume_unicode_range(tokenizer) }
            else { consume_ident_like(tokenizer) }
        },
        'a'...'z' | 'A'...'Z' | '_' | '\0' => consume_ident_like(tokenizer),
        '[' => { tokenizer.advance(1); SquareBracketBlock },
        '\\' => {
            if !tokenizer.has_newline_at(1) { consume_ident_like(tokenizer) }
            else { tokenizer.advance(1); Delim(c) }
        },
        ']' => { tokenizer.advance(1); CloseSquareBracket },
        '^' => {
            if tokenizer.starts_with("^=") { tokenizer.advance(2); PrefixMatch }
            else { tokenizer.advance(1); Delim(c) }
        },
        '{' => { tokenizer.advance(1); CurlyBracketBlock },
        '|' => {
            if tokenizer.starts_with("|=") { tokenizer.advance(2); DashMatch }
            else if tokenizer.starts_with("||") { tokenizer.advance(2); Column }
            else { tokenizer.advance(1); Delim(c) }
        },
        '}' => { tokenizer.advance(1); CloseCurlyBracket },
        '~' => {
            if tokenizer.starts_with("~=") { tokenizer.advance(2); IncludeMatch }
            else { tokenizer.advance(1); Delim(c) }
        },
        _ => {
            if c > '\x7F' {  // Non-ASCII
                consume_ident_like(tokenizer)
            } else {
                tokenizer.advance(1);
                Delim(c)
            }
        },
    };
    Some(token)
}


fn consume_string<'a>(tokenizer: &mut Tokenizer<'a>, single_quote: bool) -> Token<'a> {
    match consume_quoted_string(tokenizer, single_quote) {
        Ok(value) => QuotedString(value),
        Err(()) => BadString
    }
}


/// Return `Err(())` on syntax error (ie. unescaped newline)
fn consume_quoted_string<'a>(tokenizer: &mut Tokenizer<'a>, single_quote: bool)
                             -> Result<CowString<'a>, ()> {
    tokenizer.advance(1);  // Skip the initial quote
    let start_pos = tokenizer.position();
    let mut string;
    loop {
        if tokenizer.is_eof() {
            return Ok(Borrowed(tokenizer.slice_from(start_pos)))
        }
        match tokenizer.next_char() {
            '"' if !single_quote => {
                let value = tokenizer.slice_from(start_pos);
                tokenizer.advance(1);
                return Ok(Borrowed(value))
            }
            '\'' if single_quote => {
                let value = tokenizer.slice_from(start_pos);
                tokenizer.advance(1);
                return Ok(Borrowed(value))
            }
            '\\' | '\0' => {
                string = tokenizer.slice_from(start_pos).to_owned();
                break
            }
            '\n' | '\r' | '\x0C' => return Err(()),
            _ => {
                tokenizer.consume_char();
            }
        }
    }

    while !tokenizer.is_eof() {
        if matches!(tokenizer.next_char(), '\n' | '\r' | '\x0C') {
            return Err(());
        }
        match tokenizer.consume_char() {
            '"' if !single_quote => break,
            '\'' if single_quote => break,
            '\\' => {
                if !tokenizer.is_eof() {
                    match tokenizer.next_char() {
                        // Escaped newline
                        '\n' | '\x0C' => tokenizer.advance(1),
                        '\r' => {
                            tokenizer.advance(1);
                            if !tokenizer.is_eof() && tokenizer.next_char() == '\n' {
                                tokenizer.advance(1);
                            }
                        }
                        _ => string.push(consume_escape(tokenizer))
                    }
                }
                // else: escaped EOF, do nothing.
            }
            '\0' => string.push('\u{FFFD}'),
            c => string.push(c),
        }
    }
    Ok(Owned(string))
}


#[inline]
fn is_ident_start(tokenizer: &mut Tokenizer) -> bool {
    !tokenizer.is_eof() && match tokenizer.next_char() {
        'a'...'z' | 'A'...'Z' | '_' | '\0' => true,
        '-' => tokenizer.has_at_least(1) && match tokenizer.char_at(1) {
            'a'...'z' | 'A'...'Z' | '-' | '_' | '\0' => true,
            '\\' => !tokenizer.has_newline_at(1),
            c => c > '\x7F',  // Non-ASCII
        },
        '\\' => !tokenizer.has_newline_at(1),
        c => c > '\x7F',  // Non-ASCII
    }
}


fn consume_ident_like<'a>(tokenizer: &mut Tokenizer<'a>) -> Token<'a> {
    let value = consume_name(tokenizer);
    if !tokenizer.is_eof() && tokenizer.next_char() == '(' {
        tokenizer.advance(1);
        if value.eq_ignore_ascii_case("url") { consume_url(tokenizer) }
        else { Function(value) }
    } else {
        Ident(value)
    }
}

fn consume_name<'a>(tokenizer: &mut Tokenizer<'a>) -> CowString<'a> {
    let start_pos = tokenizer.position();
    let mut value;
    loop {
        if tokenizer.is_eof() {
            return Borrowed(tokenizer.slice_from(start_pos))
        }
        match tokenizer.next_char() {
            'a'...'z' | 'A'...'Z' | '0'...'9' | '_' | '-'  => tokenizer.advance(1),
            '\\' | '\0' => {
                value = tokenizer.slice_from(start_pos).to_owned();
                break
            }
            c if c.is_ascii() => return Borrowed(tokenizer.slice_from(start_pos)),
            _ => {
                tokenizer.consume_char();
            }
        }
    }

    while !tokenizer.is_eof() {
        let c = tokenizer.next_char();
        value.push(match c {
            'a'...'z' | 'A'...'Z' | '0'...'9' | '_' | '-'  => {
                tokenizer.advance(1);
                c
            }
            '\\' => {
                if tokenizer.has_newline_at(1) { break }
                tokenizer.advance(1);
                consume_escape(tokenizer)
            }
            '\0' => { tokenizer.advance(1); '\u{FFFD}' },
            c if c.is_ascii() => break,
            _ => tokenizer.consume_char(),
        })
    }
    Owned(value)
}


fn consume_digits(tokenizer: &mut Tokenizer) {
    while !tokenizer.is_eof() {
        match tokenizer.next_char() {
            '0'...'9' => tokenizer.advance(1),
            _ => break
        }
    }
}


fn consume_numeric<'a>(tokenizer: &mut Tokenizer<'a>) -> Token<'a> {
    // Parse [+-]?\d*(\.\d+)?([eE][+-]?\d+)?
    // But this is always called so that there is at least one digit in \d*(\.\d+)?
    let start_pos = tokenizer.position();
    let mut is_integer = true;
    let signed = matches!(tokenizer.next_char(), '-' | '+');
    if signed {
        tokenizer.advance(1);
    }
    consume_digits(tokenizer);
    if tokenizer.has_at_least(1) && tokenizer.next_char() == '.'
            && matches!(tokenizer.char_at(1), '0'...'9') {
        is_integer = false;
        tokenizer.advance(2);  // '.' and first digit
        consume_digits(tokenizer);
    }
    if (
        tokenizer.has_at_least(1)
        && matches!(tokenizer.next_char(), 'e' | 'E')
        && matches!(tokenizer.char_at(1), '0'...'9')
    ) || (
        tokenizer.has_at_least(2)
        && matches!(tokenizer.next_char(), 'e' | 'E')
        && matches!(tokenizer.char_at(1), '+' | '-')
        && matches!(tokenizer.char_at(2), '0'...'9')
    ) {
        is_integer = false;
        tokenizer.advance(2);  // 'e' or 'E', and sign or first digit
        consume_digits(tokenizer);
    }
    let (value, int_value) = {
        let mut repr = tokenizer.slice_from(start_pos);
        // Remove any + sign as int::from_str() does not parse them.
        if repr.starts_with("+") {
            repr = repr.slice_from(1)
        }
        // TODO: handle overflow
        (from_str::<f64>(repr).unwrap(), if is_integer {
            Some(from_str::<i64>(repr).unwrap())
        } else {
            None
        })
    };
    if !tokenizer.is_eof() && tokenizer.next_char() == '%' {
        tokenizer.advance(1);
        return Percentage(PercentageValue {
            unit_value: value / 100.,
            int_value: int_value,
            signed: signed,
        })
    }
    let value = NumericValue {
        value: value,
        int_value: int_value,
        signed: signed,
    };
    if is_ident_start(tokenizer) { Dimension(value, consume_name(tokenizer)) }
    else { Number(value) }
}


fn consume_url<'a>(tokenizer: &mut Tokenizer<'a>) -> Token<'a> {
    while !tokenizer.is_eof() {
        match tokenizer.next_char() {
            ' ' | '\t' | '\n' | '\r' | '\x0C' => tokenizer.advance(1),
            '"' => return consume_quoted_url(tokenizer, false),
            '\'' => return consume_quoted_url(tokenizer, true),
            ')' => { tokenizer.advance(1); break },
            _ => return consume_unquoted_url(tokenizer),
        }
    }
    return Url(Borrowed(""));

    fn consume_quoted_url<'a>(tokenizer: &mut Tokenizer<'a>, single_quote: bool) -> Token<'a> {
        match consume_quoted_string(tokenizer, single_quote) {
            Ok(value) => consume_url_end(tokenizer, value),
            Err(()) => consume_bad_url(tokenizer),
        }
    }

    fn consume_unquoted_url<'a>(tokenizer: &mut Tokenizer<'a>) -> Token<'a> {
        let start_pos = tokenizer.position();
        let mut string;
        loop {
            if tokenizer.is_eof() {
                return Url(Borrowed(tokenizer.slice_from(start_pos)))
            }
            match tokenizer.next_char() {
                ' ' | '\t' | '\n' | '\r' | '\x0C' => {
                    let value = tokenizer.slice_from(start_pos);
                    tokenizer.advance(1);
                    return consume_url_end(tokenizer, Borrowed(value))
                }
                ')' => {
                    let value = tokenizer.slice_from(start_pos);
                    tokenizer.advance(1);
                    return Url(Borrowed(value))
                }
                '\x01'...'\x08' | '\x0B' | '\x0E'...'\x1F' | '\x7F'  // non-printable
                    | '"' | '\'' | '(' => {
                    tokenizer.advance(1);
                    return consume_bad_url(tokenizer)
                },
                '\\' | '\0' => {
                    string = tokenizer.slice_from(start_pos).to_owned();
                    break
                }
                _ => {
                    tokenizer.consume_char();
                }
            }
        }
        while !tokenizer.is_eof() {
            let next_char = match tokenizer.consume_char() {
                ' ' | '\t' | '\n' | '\r' | '\x0C' => {
                    return consume_url_end(tokenizer, Owned(string))
                }
                ')' => break,
                '\x01'...'\x08' | '\x0B' | '\x0E'...'\x1F' | '\x7F'  // non-printable
                    | '"' | '\'' | '(' => return consume_bad_url(tokenizer),
                '\\' => {
                    if tokenizer.has_newline_at(0) {
                        return consume_bad_url(tokenizer)
                    }
                    consume_escape(tokenizer)
                },
                '\0' => '\u{FFFD}',
                c => c
            };
            string.push(next_char)
        }
        Url(Owned(string))
    }

    fn consume_url_end<'a>(tokenizer: &mut Tokenizer<'a>, string: CowString<'a>) -> Token<'a> {
        while !tokenizer.is_eof() {
            match tokenizer.consume_char() {
                ' ' | '\t' | '\n' | '\r' | '\x0C' => (),
                ')' => break,
                _ => return consume_bad_url(tokenizer)
            }
        }
        Url(string)
    }

    fn consume_bad_url<'a>(tokenizer: &mut Tokenizer<'a>) -> Token<'a> {
        // Consume up to the closing )
        while !tokenizer.is_eof() {
            match tokenizer.consume_char() {
                ')' => break,
                '\\' => tokenizer.advance(1), // Skip an escaped ')' or '\'
                _ => ()
            }
        }
        BadUrl
    }
}



fn consume_unicode_range<'a>(tokenizer: &mut Tokenizer<'a>) -> Token<'a> {
    tokenizer.advance(2);  // Skip U+
    let mut hex = String::new();
    while hex.len() < 6 && !tokenizer.is_eof()
          && matches!(tokenizer.next_char(), '0'...'9' | 'A'...'F' | 'a'...'f') {
        hex.push(tokenizer.consume_char());
    }
    let max_question_marks = 6u - hex.len();
    let mut question_marks = 0u;
    while question_marks < max_question_marks && !tokenizer.is_eof()
            && tokenizer.next_char() == '?' {
        question_marks += 1;
        tokenizer.advance(1)
    }
    let first: u32 = if hex.len() > 0 {
        num::from_str_radix(hex.as_slice(), 16).unwrap()
    } else { 0 };
    let start;
    let end;
    if question_marks > 0 {
        start = first << (question_marks * 4);
        end = ((first + 1) << (question_marks * 4)) - 1;
    } else {
        start = first;
        hex.truncate(0);
        if !tokenizer.is_eof() && tokenizer.next_char() == '-' {
            tokenizer.advance(1);
            while hex.len() < 6 && !tokenizer.is_eof() {
                let c = tokenizer.next_char();
                match c {
                    '0'...'9' | 'A'...'F' | 'a'...'f' => {
                        hex.push(c); tokenizer.advance(1) },
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
    if tokenizer.is_eof() { return '\u{FFFD}' }  // Escaped EOF
    let c = tokenizer.consume_char();
    match c {
        '0'...'9' | 'A'...'F' | 'a'...'f' => {
            let mut hex = String::from_char(1, c);
            while hex.len() < 6 && !tokenizer.is_eof() {
                let c = tokenizer.next_char();
                match c {
                    '0'...'9' | 'A'...'F' | 'a'...'f' => {
                        hex.push(c); tokenizer.advance(1) },
                    _ => break
                }
            }
            if !tokenizer.is_eof() {
                match tokenizer.next_char() {
                    ' ' | '\t' | '\n' | '\x0C' => tokenizer.advance(1),
                    '\r' => {
                        tokenizer.advance(1);
                        if !tokenizer.is_eof() && tokenizer.next_char() == '\n' {
                            tokenizer.advance(1);
                        }
                    }
                    _ => ()
                }
            }
            static REPLACEMENT_CHAR: char = '\u{FFFD}';
            let c: u32 = num::from_str_radix(hex.as_slice(), 16).unwrap();
            if c != 0 {
                let c = char::from_u32(c);
                c.unwrap_or(REPLACEMENT_CHAR)
            } else {
                REPLACEMENT_CHAR
            }
        },
        '\0' => '\u{FFFD}',
        c => c
    }
}
