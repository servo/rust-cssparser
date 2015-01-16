/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use std::ascii::AsciiExt;
use std::str::CowString;
use std::ops;
use tokenizer::{mod, Token, NumericValue, PercentageValue, Tokenizer, SourceLocation};


#[deriving(PartialEq, Eq, Show, Clone, Copy)]
pub struct SourcePosition {
    position: tokenizer::SourcePosition,
    at_start_of: Option<BlockType>,
}


/// Like std::borrow::Cow, except:
///
/// * The Owned variant is boxed
/// * The Borrowed variant contains a mutable reference.
enum MaybeOwned<'a, T: 'a> {
    Owned(Box<T>),
    Borrowed(&'a mut T),
}

impl<'a, T> ops::Deref<T> for MaybeOwned<'a, T> {
    fn deref<'a>(&'a self) -> &'a T {
        match *self {
            MaybeOwned::Owned(ref pointer) => &**pointer,
            MaybeOwned::Borrowed(ref pointer) => &**pointer,
        }
    }
}

impl<'a, T> ops::DerefMut<T> for MaybeOwned<'a, T> {
    fn deref_mut<'a>(&'a mut self) -> &'a mut T {
        match *self {
            MaybeOwned::Owned(ref mut pointer) => &mut **pointer,
            MaybeOwned::Borrowed(ref mut pointer) => &mut **pointer,
        }
    }
}

impl<'a, T> Clone for MaybeOwned<'a, T> where T: Clone {
    fn clone(&self) -> MaybeOwned<'a, T> {
        MaybeOwned::Owned(box() (**self).clone())
    }
}


#[deriving(Clone)]
pub struct Parser<'i: 't, 't> {
    tokenizer: MaybeOwned<'t, Tokenizer<'i>>,
    /// If `Some(_)`, .parse_nested_block() can be called.
    at_start_of: Option<BlockType>,
    /// For parsers from `parse_until` or `parse_nested_block`
    parse_until_before: Delimiters,
}


#[deriving(Copy, Clone, PartialEq, Eq, Show)]
enum BlockType {
    Parenthesis,
    SquareBracket,
    CurlyBracket,
}


impl BlockType {
    fn opening(token: &Token) -> Option<BlockType> {
        match *token {
            Token::Function(_) |
            Token::ParenthesisBlock => Some(BlockType::Parenthesis),
            Token::SquareBracketBlock => Some(BlockType::SquareBracket),
            Token::CurlyBracketBlock => Some(BlockType::CurlyBracket),
            _ => None
        }
    }

    fn closing(token: &Token) -> Option<BlockType> {
        match *token {
            Token::CloseParenthesis => Some(BlockType::Parenthesis),
            Token::CloseSquareBracket => Some(BlockType::SquareBracket),
            Token::CloseCurlyBracket => Some(BlockType::CurlyBracket),
            _ => None
        }
    }
}



#[deriving(Copy, Clone, PartialEq, Eq, Show)]
pub struct Delimiters {
    bits: u8,
}

#[allow(non_upper_case_globals, non_snake_case)]
pub mod Delimiter {
    use super::Delimiters;

    pub const None: Delimiters = Delimiters { bits: 0 };
    /// `{`
    pub const CurlyBracketBlock: Delimiters = Delimiters { bits: 1 << 1 };
    /// `;`
    pub const Semicolon: Delimiters = Delimiters { bits: 1 << 2 };
    /// `!`
    pub const Bang: Delimiters = Delimiters { bits: 1 << 3 };
    /// `,`
    pub const Comma: Delimiters = Delimiters { bits: 1 << 4 };
}

#[allow(non_upper_case_globals, non_snake_case)]
mod ClosingDelimiter {
    use super::Delimiters;

    pub const CloseCurlyBracket: Delimiters = Delimiters { bits: 1 << 5 };
    pub const CloseSquareBracket: Delimiters = Delimiters { bits: 1 << 6 };
    pub const CloseParenthesis: Delimiters = Delimiters { bits: 1 << 7 };
}

impl BitOr<Delimiters, Delimiters> for Delimiters {
    fn bitor(&self, other: &Delimiters) -> Delimiters {
        Delimiters { bits: self.bits | other.bits }
    }
}

impl Delimiters {
    fn contains(self, other: Delimiters) -> bool {
        (self.bits & other.bits) != 0
    }

    fn from_byte(byte: Option<u8>) -> Delimiters {
        match byte {
            Some(b';') => Delimiter::Semicolon,
            Some(b',') => Delimiter::Comma,
            Some(b'!') => Delimiter::Bang,
            Some(b'{') => Delimiter::CurlyBracketBlock,
            Some(b'}') => ClosingDelimiter::CloseCurlyBracket,
            Some(b']') => ClosingDelimiter::CloseSquareBracket,
            Some(b')') => ClosingDelimiter::CloseParenthesis,
            _ => Delimiter::None,
        }
    }
}

impl<'i, 't> Parser<'i, 't> {
    #[inline]
    pub fn new(input: &'i str) -> Parser<'i, 'i> {
        Parser {
            tokenizer: MaybeOwned::Owned(box Tokenizer::new(input)),
            at_start_of: None,
            parse_until_before: Delimiter::None,
        }
    }

    #[inline]
    pub fn is_exhausted(&mut self) -> bool {
        self.expect_exhausted().is_ok()
    }

    #[inline]
    pub fn expect_exhausted(&mut self) -> Result<(), ()> {
        let start_position = self.position();
        match self.next() {
            Err(()) => Ok(()),
            Ok(_) => {
                self.reset(start_position);
                Err(())
            }
        }
    }

    #[inline]
    pub fn position(&self) -> SourcePosition {
        SourcePosition {
            position: self.tokenizer.position(),
            at_start_of: self.at_start_of,
        }
    }

    #[inline]
    pub fn reset(&mut self, new_position: SourcePosition) {
        self.tokenizer.reset(new_position.position);
        self.at_start_of = new_position.at_start_of;
    }

    #[inline]
    pub fn try<T, E>(&mut self, thing: |&mut Parser| -> Result<T, E>) -> Result<T, E> {
        let start_position = self.position();
        let result = thing(self);
        if result.is_err() {
            self.reset(start_position)
        }
        result
    }

    #[inline]
    pub fn slice_from(&self, start_position: SourcePosition) -> &'i str {
        self.tokenizer.slice_from(start_position.position)
    }

    #[inline]
    pub fn current_source_location(&self) -> SourceLocation {
        self.tokenizer.current_source_location()
    }

    #[inline]
    pub fn source_location(&self, target: SourcePosition) -> SourceLocation {
        self.tokenizer.source_location(target.position)
    }

    pub fn next(&mut self) -> Result<Token<'i>, ()> {
        loop {
            match self.next_including_whitespace() {
                Ok(Token::WhiteSpace) => {},
                result => return result
            }
        }
    }

    pub fn next_including_whitespace(&mut self) -> Result<Token<'i>, ()> {
        if let Some(block_type) = self.at_start_of.take() {
            consume_until_end_of_block(block_type, &mut *self.tokenizer);
        }
        if self.parse_until_before.contains(Delimiters::from_byte(self.tokenizer.next_byte())) {
            return Err(())
        }
        let token = try!(self.tokenizer.next());
        if let Some(block_type) = BlockType::opening(&token) {
            self.at_start_of = Some(block_type);
        }
        Ok(token)
    }

    // FIXME: Take an unboxed `FnOnce` closure.
    #[inline]
    pub fn parse_entirely<T>(&mut self, parse: |&mut Parser| -> Result<T, ()>)
                             -> Result<T, ()> {
        let result = parse(self);
        try!(self.expect_exhausted());
        result
    }

    #[inline]
    pub fn parse_comma_separated<T>(&mut self, parse_one: |&mut Parser| -> Result<T, ()>)
                                    -> Result<Vec<T>, ()> {
        let mut values = vec![];
        loop {
            values.push(try!(self.parse_until_before(Delimiter::Comma, |parser| parse_one(parser))));
            match self.next() {
                Err(()) => return Ok(values),
                Ok(Token::Comma) => continue,
                Ok(_) => unreachable!(),
            }
        }
    }

    #[inline]
    pub fn parse_nested_block<T>(&mut self, parse: |&mut Parser| -> Result<T, ()>)
                                 -> Result <T, ()> {
        let block_type = self.at_start_of.take().expect("\
            A nested parser can only be created when a Function, \
            ParenthesisBlock, SquareBracketBlock, or CurlyBracketBlock \
            token was just consumed.\
        ");
        let closing_delimiter = match block_type {
            BlockType::CurlyBracket => ClosingDelimiter::CloseCurlyBracket,
            BlockType::SquareBracket => ClosingDelimiter::CloseSquareBracket,
            BlockType::Parenthesis => ClosingDelimiter::CloseParenthesis,
        };
        let result;
        // Introduce a new scope to limit duration of nested_parser’s borrow
        {
            let mut nested_parser = Parser {
                tokenizer: MaybeOwned::Borrowed(&mut *self.tokenizer),
                at_start_of: None,
                parse_until_before: closing_delimiter,
            };
            result = nested_parser.parse_entirely(parse);
        }
        consume_until_end_of_block(block_type, &mut *self.tokenizer);
        result
    }

    #[inline]
    pub fn parse_until_before<T>(&mut self, delimiters: Delimiters,
                                 parse: |&mut Parser| -> Result<T, ()>)
                                 -> Result <T, ()> {
        let delimiters = self.parse_until_before | delimiters;
        let result;
        // Introduce a new scope to limit duration of nested_parser’s borrow
        {
            let mut delimited_parser = Parser {
                tokenizer: MaybeOwned::Borrowed(&mut *self.tokenizer),
                at_start_of: self.at_start_of.take(),
                parse_until_before: delimiters,
            };
            result = delimited_parser.parse_entirely(parse);
        }
        // FIXME: have a special-purpose tokenizer method for this that does less work.
        loop {
            if delimiters.contains(Delimiters::from_byte(self.tokenizer.next_byte())) {
                break
            }
            if let Ok(token) = self.tokenizer.next() {
                if let Some(block_type) = BlockType::opening(&token) {
                    consume_until_end_of_block(block_type, &mut *self.tokenizer);
                }
            } else {
                break
            }
        }
        result
    }

    #[inline]
    pub fn parse_until_after<T>(&mut self, delimiters: Delimiters,
                                parse: |&mut Parser| -> Result<T, ()>)
                                -> Result <T, ()> {
        let result = self.parse_until_before(delimiters, parse);
        let next_byte = self.tokenizer.next_byte();
        if next_byte.is_some() && !self.parse_until_before.contains(Delimiters::from_byte(next_byte)) {
            debug_assert!(delimiters.contains(Delimiters::from_byte(next_byte)));
            self.tokenizer.advance(1);
        }
        result
    }

    #[inline]
    pub fn expect_ident(&mut self) -> Result<CowString<'i>, ()> {
        match try!(self.next()) {
            Token::Ident(value) => Ok(value),
            _ => Err(())
        }
    }

    /// Expect an *ident* token whose value is an ASCII-insensitive match for the given value.
    #[inline]
    pub fn expect_ident_matching<'a>(&mut self, expected_value: &str) -> Result<(), ()> {
        match try!(self.next()) {
            Token::Ident(ref value) if value.eq_ignore_ascii_case(expected_value) => Ok(()),
            _ => Err(())
        }
    }

    #[inline]
    pub fn expect_string(&mut self) -> Result<CowString<'i>, ()> {
        match try!(self.next()) {
            Token::QuotedString(value) => Ok(value),
            _ => Err(())
        }
    }

    #[inline]
    pub fn expect_ident_or_string(&mut self) -> Result<CowString<'i>, ()> {
        match try!(self.next()) {
            Token::Ident(value) => Ok(value),
            Token::QuotedString(value) => Ok(value),
            _ => Err(())
        }
    }

    #[inline]
    pub fn expect_url(&mut self) -> Result<CowString<'i>, ()> {
        match try!(self.next()) {
            Token::Url(value) => Ok(value),
            _ => Err(())
        }
    }

    #[inline]
    pub fn expect_url_or_string(&mut self) -> Result<CowString<'i>, ()> {
        match try!(self.next()) {
            Token::Url(value) => Ok(value),
            Token::QuotedString(value) => Ok(value),
            _ => Err(())
        }
    }

    #[inline]
    pub fn expect_number(&mut self) -> Result<f64, ()> {
        match try!(self.next()) {
            Token::Number(NumericValue { value, .. }) => Ok(value),
            _ => Err(())
        }
    }

    #[inline]
    pub fn expect_integer(&mut self) -> Result<i64, ()> {
        match try!(self.next()) {
            Token::Number(NumericValue { int_value, .. }) => int_value.ok_or(()),
            _ => Err(())
        }
    }

    #[inline]
    pub fn expect_percentage(&mut self) -> Result<f64, ()> {
        match try!(self.next()) {
            Token::Percentage(PercentageValue { unit_value, .. }) => Ok(unit_value),
            _ => Err(())
        }
    }

    #[inline]
    pub fn expect_colon(&mut self) -> Result<(), ()> {
        match try!(self.next()) {
            Token::Colon => Ok(()),
            _ => Err(())
        }
    }

    #[inline]
    pub fn expect_semicolon(&mut self) -> Result<(), ()> {
        match try!(self.next()) {
            Token::Semicolon => Ok(()),
            _ => Err(())
        }
    }

    #[inline]
    pub fn expect_comma(&mut self) -> Result<(), ()> {
        match try!(self.next()) {
            Token::Comma => Ok(()),
            _ => Err(())
        }
    }

    #[inline]
    pub fn expect_delim(&mut self, expected_value: char) -> Result<(), ()> {
        match try!(self.next()) {
            Token::Delim(value) if value == expected_value => Ok(()),
            _ => Err(())
        }
    }

    #[inline]
    pub fn expect_curly_bracket_block(&mut self) -> Result<(), ()> {
        match try!(self.next()) {
            Token::CurlyBracketBlock => Ok(()),
            _ => Err(())
        }
    }

    #[inline]
    pub fn expect_function(&mut self) -> Result<CowString<'i>, ()> {
        match try!(self.next()) {
            Token::Function(name) => Ok(name),
            _ => Err(())
        }
    }
}


/// Return value indicates whether the end of the input was reached.
fn consume_until_end_of_block(block_type: BlockType, tokenizer: &mut Tokenizer) {
    // FIXME: have a special-purpose tokenizer method for this that does less work.
    while let Ok(ref token) = tokenizer.next() {
        if BlockType::closing(token) == Some(block_type) {
            return
        }
        if let Some(block_type) = BlockType::opening(token) {
            consume_until_end_of_block(block_type, tokenizer);
        }
    }
}
