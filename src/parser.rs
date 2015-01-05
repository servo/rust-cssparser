/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use std::str::CowString;
use super::{Token, NumericValue, Tokenizer};


pub struct Parser<'i: 't, 't> {
    tokenizer: Option<&'t mut Tokenizer<'i>>,
    parent_state: Option<&'t mut ParserState>,
    state: ParserState,
}

struct ParserState {
    nested_blocks: NestedBlockList,
    /// For block/function parsers that need to stop at the matching `)`, `]`, or `}`
    at_start_of: Option<BlockType>,
    /// For block/function parsers that need to stop at the matching `)`, `]`, or `}`
    stop_at_end_of: Option<BlockType>,
    /// For parsers from `parse_until`
    stop_before: Delimiters,
}


#[deriving(Copy, PartialEq, Eq, Show)]
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


type NestedBlockList = Option<Box<NestedBlockListItem>>;

struct NestedBlockListItem {
    next: NestedBlockList,
    block_type: BlockType,
}

trait NestedBlockListExt {
    fn push(&mut self, block_type: BlockType);
    fn consume(self, tokenizer: &mut Tokenizer) -> bool;
}

impl NestedBlockListExt for NestedBlockList {
    fn push(&mut self, block_type: BlockType) {
        debug_assert!(self.is_none());
        *self = Some(box NestedBlockListItem {
            next: self.take(),
            block_type: block_type,
        })
    }

    /// Return value indicates whether the end of the input was reached.
    fn consume(self, tokenizer: &mut Tokenizer) -> bool {
        if let Some(box NestedBlockListItem { block_type, next }) = self {
            // Recursion first: the inner-most item of the list
            // is for the inner-most nested block.
            next.consume(tokenizer) || consume_until_end_of_block(block_type, tokenizer)
        } else {
            false
        }
    }
}


#[unsafe_destructor] // FIXME What does this mean?
impl<'i, 't> Drop for Parser<'i, 't> {
    fn drop(&mut self) {
        if self.tokenizer.is_none() {
            // We’ve already reached the end of our delimited input:
            // nothing to inform the parent of.
            debug_assert!(self.state.nested_blocks.is_none());
            return
        }
        if let Some(parent_state) = self.parent_state.take() {
            // Inform our parent parser of what they need to consume.
            debug_assert!(parent_state.nested_blocks.is_none());
            parent_state.nested_blocks = self.state.nested_blocks.take();
            if let Some(block_type) = self.state.at_start_of {
                parent_state.nested_blocks.push(block_type)
            }
            // Don’t propagate stop_at_end_of back for delimited parsers:
            if self.state.stop_before.is_none() {
                debug_assert!(parent_state.at_start_of.is_none());
                parent_state.at_start_of = self.state.stop_at_end_of;
            }
        }
    }
}


#[deriving(Copy, PartialEq, Eq, Show)]
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

impl BitOr<Delimiters, Delimiters> for Delimiters {
    fn bitor(&self, other: &Delimiters) -> Delimiters {
        Delimiters { bits: self.bits | other.bits }
    }
}

impl Delimiters {
    fn contains(&self, other: Delimiters) -> bool {
        (self.bits & other.bits) != 0
    }

    fn is_none(&self) -> bool {
        self.bits == 0
    }

    fn from_token(token: &Token) -> Delimiters {
        match *token {
            Token::Semicolon => Delimiter::Semicolon,
            Token::Comma  => Delimiter::Comma,
            Token::Delim('!') => Delimiter::Bang,
            Token::CurlyBracketBlock => Delimiter::CurlyBracketBlock,
            _ => Delimiter::None,
        }
    }
}

impl<'i, 't> Parser<'i, 't> {
    #[inline]
    pub fn new(tokenizer: &'t mut Tokenizer<'i>) -> Parser<'i, 't> {
        Parser {
            tokenizer: Some(tokenizer),
            parent_state: None,
            state: ParserState {
                nested_blocks: None,
                at_start_of: None,
                stop_at_end_of: None,
                stop_before: Delimiter::None,
            },
        }
    }

    #[inline]
    pub fn parse_str<T>(input: &str, parse: |&mut Parser| -> T) -> T {
        parse(&mut Parser::new(&mut Tokenizer::new(input.as_slice())))
    }

    #[inline]
    pub fn is_exhausted(&mut self) -> bool {
        self.peek().is_err()
    }

    #[inline]
    pub fn expect_exhausted(&mut self) -> Result<(), ()> {
        if self.is_exhausted() {
            Ok(())
        } else {
            Err(())
        }
    }

    #[inline]
    pub fn peek(&mut self) -> Result<&Token<'i>, ()> {
        // Consume whatever needs to be consumed (e.g. open blocks).
        let token = try!(self.next());
        self.push_back(token);

        self.tokenizer().peek()
    }

    #[inline]
    pub fn push_back(&mut self, token: Token<'i>) {
        if BlockType::opening(&token) == self.state.at_start_of {
            self.state.at_start_of = None;
        }
        self.tokenizer.as_mut().expect(
            "Can not use Parser::push_back after the end of the input was reached."
        ).push_back(token)
    }

    #[inline]
    pub fn push_back_result(&mut self, token_result: Result<Token<'i>, ()>) {
        if let Ok(token) = token_result {
            self.push_back(token)
        }
    }

    #[inline]
    pub fn unexpected<T>(&mut self, token: Token<'i>) -> Result<T, ()> {
        self.push_back(token);
        Err(())
    }

    #[inline]
    fn tokenizer<'a>(&'a mut self) -> &'a mut Tokenizer<'i> {
        &mut **self.tokenizer.as_mut().unwrap()
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
        if self.tokenizer.is_none() {
            return Err(())
        }
        if self.state.nested_blocks.take().consume(self.tokenizer()) {
            self.tokenizer = None;
            return Err(())
        }
        if let Some(block_type) = self.state.at_start_of.take() {
            if consume_until_end_of_block(block_type, self.tokenizer()) {
                self.tokenizer = None;
                return Err(())
            }
        }
        match self.tokenizer().next() {
            Err(()) => {
                self.tokenizer = None;
                Err(())
            },
            Ok(token) => {
                if self.state.stop_before.contains(Delimiters::from_token(&token)) {
                    self.tokenizer.take().unwrap().push_back(token);
                    return Err(())
                }
                if self.state.stop_at_end_of.is_some() &&
                        BlockType::closing(&token) == self.state.stop_at_end_of {
                    self.tokenizer = None;
                    return Err(())
                }
                if let Some(block_type) = BlockType::opening(&token) {
                    self.state.at_start_of = Some(block_type);
                }
                Ok(token)
            }
        }
    }

    #[inline]
    pub fn err_consume_until_after<T>(&mut self, stop_after: Delimiters) -> Result<T, ()> {
        self.consume_until_after(stop_after);
        Err(())
    }

    pub fn consume_until_after(&mut self, stop_after: Delimiters) {
        if self.tokenizer.is_none() {
            return
        }
        // FIXME: have a special-purpose tokenizer method for this that does less work.
        while let Ok(token) = self.tokenizer().next() {
            if stop_after.contains(Delimiters::from_token(&token)) {
                return
            }
            if self.state.stop_before.contains(Delimiters::from_token(&token)) {
                self.tokenizer.take().unwrap().push_back(token);
                return
            }
            if self.state.stop_at_end_of.is_some() &&
                    BlockType::closing(&token) == self.state.stop_at_end_of {
                self.tokenizer = None;
                return
            }
            if let Some(block_type) = BlockType::opening(&token) {
                if consume_until_end_of_block(block_type, self.tokenizer()) {
                    self.tokenizer = None;
                    return
                }
            }
        }
        self.tokenizer = None;
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
    pub fn parse_nested_block<'a>(&'a mut self) -> Parser<'i, 'a> {
        if let Some(block_type) = self.state.at_start_of.take() {
            Parser {
                // Unwrap here should never fail
                // because `self.tokenizer` is only ever set to `None`
                // when `self.state` is also `None`.
                tokenizer: Some(&mut **self.tokenizer.as_mut().unwrap()),
                state: ParserState {
                    nested_blocks: self.state.nested_blocks.take(),
                    at_start_of: None,
                    stop_at_end_of: Some(block_type),
                    stop_before: Delimiter::None,
                },
                parent_state: Some(&mut self.state),
            }
        } else {
            panic!("\
                parse_block can only be called when a Function, \
                ParenthesisBlock, SquareBracketBlock, or CurlyBracketBlock \
                token was just consumed.\
            ");
        }
    }

    #[inline]
    pub fn parse_until_before<'a>(&'a mut self, stop_before: Delimiters) -> Parser<'i, 'a> {
        if self.state.stop_before != Delimiter::None {
            panic!("`parse_until_before` cannot be used on a `Parser` \
                    that is itself from `parse_until_before`")
        }
        Parser {
            tokenizer: self.tokenizer.as_mut().map(|t| &mut **t),
            state: ParserState {
                nested_blocks: self.state.nested_blocks.take(),
                at_start_of: self.state.at_start_of.take(),
                stop_at_end_of: self.state.stop_at_end_of,
                stop_before: stop_before,
            },
            parent_state: Some(&mut self.state),
        }
    }

    #[inline]
    pub fn expect_ident(&mut self) -> Result<CowString<'i>, ()> {
        match try!(self.next()) {
            Token::Ident(value) => Ok(value),
            token => self.unexpected(token)
        }
    }

    #[inline]
    pub fn expect_quoted_string(&mut self) -> Result<CowString<'i>, ()> {
        match try!(self.next()) {
            Token::QuotedString(value) => Ok(value),
            token => self.unexpected(token)
        }
    }

    #[inline]
    pub fn expect_url(&mut self) -> Result<CowString<'i>, ()> {
        match try!(self.next()) {
            Token::Url(value) => Ok(value),
            token => self.unexpected(token)
        }
    }

    #[inline]
    pub fn expect_number(&mut self) -> Result<f64, ()> {
        match try!(self.next()) {
            Token::Number(NumericValue { value, .. }) => Ok(value),
            token => self.unexpected(token)
        }
    }

    #[inline]
    pub fn expect_integer(&mut self) -> Result<i64, ()> {
        match try!(self.next()) {
            Token::Number(NumericValue { int_value, .. }) => int_value.ok_or(()),
            token => self.unexpected(token)
        }
    }

    #[inline]
    pub fn expect_percentage(&mut self) -> Result<f64, ()> {
        match try!(self.next()) {
            Token::Percentage(NumericValue { value, .. }) => Ok(value),
            token => self.unexpected(token)
        }
    }

    #[inline]
    pub fn expect_colon(&mut self) -> Result<(), ()> {
        match try!(self.next()) {
            Token::Colon => Ok(()),
            token => self.unexpected(token)
        }
    }

    #[inline]
    pub fn expect_semicolon(&mut self) -> Result<(), ()> {
        match try!(self.next()) {
            Token::Semicolon => Ok(()),
            token => self.unexpected(token)
        }
    }

    #[inline]
    pub fn expect_comma(&mut self) -> Result<(), ()> {
        match try!(self.next()) {
            Token::Comma => Ok(()),
            token => self.unexpected(token)
        }
    }

    #[inline]
    pub fn expect_curly_bracke_block(&mut self) -> Result<(), ()> {
        match try!(self.next()) {
            Token::CurlyBracketBlock => Ok(()),
            token => self.unexpected(token)
        }
    }
}


/// Return value indicates whether the end of the input was reached.
fn consume_until_end_of_block(block_type: BlockType, tokenizer: &mut Tokenizer) -> bool {
    // FIXME: have a special-purpose tokenizer method for this that does less work.
    while let Ok(ref token) = tokenizer.next() {
        if BlockType::closing(token) == Some(block_type) {
            return false
        }
        if let Some(block_type) = BlockType::opening(token) {
            if consume_until_end_of_block(block_type, tokenizer) {
                return true
            }
        }
    }
    true
}
