/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use std::str::CowString;
use super::{Token, NumericValue, Tokenizer, SourcePosition, SourceLocation};


pub struct Parser<'i: 't, 't> {
    tokenizer: &'t mut Tokenizer<'i>,
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
    exhausted: bool,
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
        if self.state.exhausted {
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
            tokenizer: tokenizer,
            parent_state: None,
            state: ParserState {
                nested_blocks: None,
                at_start_of: None,
                stop_at_end_of: None,
                stop_before: Delimiter::None,
                exhausted: false,
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

        self.tokenizer.peek()
    }

    #[inline]
    pub fn push_back(&mut self, token: Token<'i>) {
        if BlockType::opening(&token) == self.state.at_start_of {
            self.state.at_start_of = None;
        }
        self.tokenizer.push_back(token)
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
    pub fn position(&self) -> SourcePosition {
        self.tokenizer.position()
    }

    #[inline]
    pub fn slice_from(&self, start_pos: SourcePosition) -> &'i str {
        self.tokenizer.slice_from(start_pos)
    }

    #[inline]
    pub fn current_source_location(&mut self) -> SourceLocation {
        self.tokenizer.current_source_location()
    }

    #[inline]
    pub fn source_location(&mut self, target: SourcePosition) -> SourceLocation {
        self.tokenizer.source_location(target)
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
        if self.state.exhausted {
            return Err(())
        }
        if self.state.nested_blocks.take().consume(self.tokenizer) {
            self.state.exhausted = true;
            return Err(())
        }
        if let Some(block_type) = self.state.at_start_of.take() {
            if consume_until_end_of_block(block_type, self.tokenizer) {
                self.state.exhausted = true;
                return Err(())
            }
        }
        match self.tokenizer.next() {
            Err(()) => {
                self.state.exhausted = true;
                Err(())
            },
            Ok(token) => {
                if self.state.stop_before.contains(Delimiters::from_token(&token)) {
                    self.tokenizer.push_back(token);
                    self.state.exhausted = true;
                    return Err(())
                }
                if self.state.stop_at_end_of.is_some() &&
                        BlockType::closing(&token) == self.state.stop_at_end_of {
                    self.state.exhausted = true;
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
        if self.state.exhausted {
            return
        }
        // FIXME: have a special-purpose tokenizer method for this that does less work.
        while let Ok(token) = self.tokenizer.next() {
            if stop_after.contains(Delimiters::from_token(&token)) {
                return
            }
            if self.state.stop_before.contains(Delimiters::from_token(&token)) {
                self.tokenizer.push_back(token);
                self.state.exhausted = true;
                return
            }
            if self.state.stop_at_end_of.is_some() &&
                    BlockType::closing(&token) == self.state.stop_at_end_of {
                self.state.exhausted = true;
                return
            }
            if let Some(block_type) = BlockType::opening(&token) {
                if consume_until_end_of_block(block_type, self.tokenizer) {
                    self.state.exhausted = true;
                    return
                }
            }
        }
        self.state.exhausted = true;
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
        let mut values = vec![try!(self.parse_until_before(Delimiter::Comma)
                                       .parse_entirely(|parser| parse_one(parser)))];
        loop {
            match self.next() {
                Ok(Token::Comma) => {
                    values.push(try!(self.parse_until_before(Delimiter::Comma)
                                         .parse_entirely(|parser| parse_one(parser))))
                }
                Ok(token) => return self.unexpected(token),
                Err(()) => return Ok(values),
            }
        }
    }

    #[inline]
    pub fn parse_nested_block<'a>(&'a mut self) -> Parser<'i, 'a> {
        if let Some(block_type) = self.state.at_start_of.take() {
            debug_assert!(!self.state.exhausted);
            Parser {
                tokenizer: self.tokenizer,
                state: ParserState {
                    nested_blocks: self.state.nested_blocks.take(),
                    at_start_of: None,
                    stop_at_end_of: Some(block_type),
                    stop_before: Delimiter::None,
                    exhausted: false,
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
        Parser {
            tokenizer: self.tokenizer,
            state: ParserState {
                nested_blocks: self.state.nested_blocks.take(),
                at_start_of: self.state.at_start_of.take(),
                stop_at_end_of: self.state.stop_at_end_of,
                stop_before: self.state.stop_before | stop_before,
                exhausted: self.state.exhausted,
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
    pub fn expect_string(&mut self) -> Result<CowString<'i>, ()> {
        match try!(self.next()) {
            Token::QuotedString(value) => Ok(value),
            token => self.unexpected(token)
        }
    }

    #[inline]
    pub fn expect_ident_or_string(&mut self) -> Result<CowString<'i>, ()> {
        match try!(self.next()) {
            Token::Ident(value) => Ok(value),
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
    pub fn expect_url_or_string(&mut self) -> Result<CowString<'i>, ()> {
        match try!(self.next()) {
            Token::Url(value) => Ok(value),
            Token::QuotedString(value) => Ok(value),
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
