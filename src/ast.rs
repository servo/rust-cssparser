/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use std::fmt;
use std::slice;
use std::vec;


#[deriving(PartialEq, Show)]
pub struct NumericValue {
    pub representation: String,
    pub value: f64,
    pub int_value: Option<i64>,
}


#[deriving(PartialEq, Show)]
pub struct SourceLocation {
    pub line: uint,  // First line is 1
    pub column: uint,  // First character of a line is at column 1
}


pub type Node = (ComponentValue, SourceLocation);  // TODO this is not a good name


#[deriving(PartialEq, Show)]
pub enum ComponentValue {
    // Preserved tokens.
    Ident(String),
    AtKeyword(String),
    Hash(String),
    IDHash(String),  // Hash that is a valid ID selector.
    QuotedString(String),
    URL(String),
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
    Function(String, Vec<ComponentValue>),  // name, arguments

    // Simple block
    ParenthesisBlock(Vec<ComponentValue>),  // (…)
    SquareBracketBlock(Vec<ComponentValue>),  // […]
    CurlyBracketBlock(Vec<Node>),  // {…}

    // These are always invalid
    BadURL,
    BadString,
    CloseParenthesis, // )
    CloseSquareBracket, // ]
    CloseCurlyBracket, // }
}


impl ComponentValue {
    pub fn to_css(&self) -> String {
        let mut css = String::new();
        self.to_css_push(&mut css);
        css
    }

    pub fn to_css_push(&self, css: &mut String) {
        ::serializer::to_css_push(self, css)
    }
}


#[deriving(PartialEq)]
pub struct Declaration {
    pub location: SourceLocation,
    pub name: String,
    pub value: Vec<ComponentValue>,
    pub important: bool,
}

#[deriving(PartialEq)]
pub struct QualifiedRule {
    pub location: SourceLocation,
    pub prelude: Vec<ComponentValue>,
    pub block: Vec<Node>,
}

#[deriving(PartialEq)]
pub struct AtRule {
    pub location: SourceLocation,
    pub name: String,
    pub prelude: Vec<ComponentValue>,
    pub block: Option<Vec<Node>>,
}

#[deriving(PartialEq)]
pub enum DeclarationListItem {
    Declaration_(Declaration),
    // A better idea for a name that means "at-rule" but is not "AtRule"?
    DeclAtRule(AtRule),
}

#[deriving(PartialEq)]
pub enum Rule {
    QualifiedRule_(QualifiedRule),
    AtRule_(AtRule),
}

#[deriving(PartialEq)]
pub struct SyntaxError {
    pub location: SourceLocation,
    pub reason: ErrorReason,
}

#[deriving(PartialEq, Show)]
pub enum ErrorReason {
    ErrEmptyInput,  // Parsing a single "thing", found only whitespace.
    ErrExtraInput,  // Found more non-whitespace after parsing a single "thing".
    ErrMissingQualifiedRuleBlock,  // EOF in a qualified rule prelude, before '{'
    ErrInvalidDeclarationSyntax,
    ErrInvalidBangImportantSyntax,
    // This is meant to be extended
}

impl fmt::Show for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:u}:{:u} {}", self.location.line, self.location.column, self.reason)
    }
}


pub trait SkipWhitespaceIterable<'a> {
    fn skip_whitespace(self) -> SkipWhitespaceIterator<'a>;
}

impl<'a> SkipWhitespaceIterable<'a> for &'a [ComponentValue] {
    fn skip_whitespace(self) -> SkipWhitespaceIterator<'a> {
        SkipWhitespaceIterator{ iter_with_whitespace: self.iter() }
    }
}

#[deriving(Clone)]
pub struct SkipWhitespaceIterator<'a> {
    pub iter_with_whitespace: slice::Items<'a, ComponentValue>,
}

impl<'a> Iterator<&'a ComponentValue> for SkipWhitespaceIterator<'a> {
    fn next(&mut self) -> Option<&'a ComponentValue> {
        for component_value in self.iter_with_whitespace {
            if component_value != &WhiteSpace { return Some(component_value) }
        }
        None
    }
}


pub trait MoveSkipWhitespaceIterable {
    fn move_skip_whitespace(self) -> MoveSkipWhitespaceIterator;
}

impl MoveSkipWhitespaceIterable for Vec<ComponentValue> {
    fn move_skip_whitespace(self) -> MoveSkipWhitespaceIterator {
        MoveSkipWhitespaceIterator{ iter_with_whitespace: self.into_iter() }
    }
}

pub struct MoveSkipWhitespaceIterator {
    iter_with_whitespace: vec::MoveItems<ComponentValue>,
}

impl Iterator<ComponentValue> for MoveSkipWhitespaceIterator {
    fn next(&mut self) -> Option<ComponentValue> {
        for component_value in self.iter_with_whitespace {
            if component_value != WhiteSpace { return Some(component_value) }
        }
        None
    }
}
