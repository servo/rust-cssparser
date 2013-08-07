/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use std::str::ToStr;
use std::vec;


#[deriving(Eq)]
pub struct NumericValue {
    representation: ~str,
    value: f64,
    int_value: Option<i64>,
}


#[deriving(Eq)]
pub struct SourceLocation {
    line: uint,  // First line is 1
    column: uint,  // First character of a line is at column 1
}


#[deriving(Eq)]
pub enum ComponentValue {
    // Preserved tokens. Same as in the tokenizer.
    Ident(~str),
    AtKeyword(~str),
    Hash(~str),
    IDHash(~str),  // Hash token that is a valid ID selector.
    String(~str),
    URL(~str),
    Delim(char),
    Number(NumericValue),
    Percentage(NumericValue),
    Dimension(NumericValue, ~str),
    UnicodeRange(char, char), // UnicodeRange {start: char, end: char},
    EmptyUnicodeRange,
    WhiteSpace,
    Colon,  // :
    Semicolon,  // ;
    Comma,  // ,
    IncludeMath, // ~=
    DashMatch, // |=
    PrefixMatch, // ^=
    SuffixMatch, // $=
    SubstringMatch, // *=
    Column, // ||
    CDO,  // <!--
    CDC,  // -->

    // Function
    Function(~str, ~[(ComponentValue, SourceLocation)]),  // name, arguments

    // Simple block
    ParenthesisBlock(~[(ComponentValue, SourceLocation)]),  // (…)
    SquareBracketBlock(~[(ComponentValue, SourceLocation)]),  // […]
    CurlyBracketBlock(~[(ComponentValue, SourceLocation)]),  // {…}

    // These are always invalid
    BadURL,
    BadString,
    CloseParenthesis, // )
    CloseSquareBracket, // ]
    CloseCurlyBracket, // }
}


#[deriving(Eq)]
pub struct Declaration {
    location: SourceLocation,
    name: ~str,
    value: ~[(ComponentValue, SourceLocation)],
    important: bool,
}

#[deriving(Eq)]
pub struct QualifiedRule {
    location: SourceLocation,
    prelude: ~[(ComponentValue, SourceLocation)],
    block: ~[(ComponentValue, SourceLocation)],
}

#[deriving(Eq)]
pub struct AtRule {
    location: SourceLocation,
    name: ~str,
    prelude: ~[(ComponentValue, SourceLocation)],
    block: Option<~[(ComponentValue, SourceLocation)]>,
}

#[deriving(Eq)]
pub enum DeclarationListItem {
    Declaration(Declaration),
    // A better idea for a name that means "at-rule" but is not "AtRule"?
    Decl_AtRule(AtRule),
}

#[deriving(Eq)]
pub enum Rule {
    QualifiedRule(QualifiedRule),
    AtRule(AtRule),
}

#[deriving(Eq)]
pub enum ErrorReason {
    ErrEmptyInput,  // Parsing a single "thing", found only whitespace.
    ErrExtraInput,  // Found more non-whitespace after parsing a single "thing".
    ErrMissingQualifiedRuleBlock,  // EOF in a qualified rule prelude, before '{'
    ErrInvalidDeclarationSyntax,
    ErrInvalidBangImportantSyntax,
    // This is meant to be extended
}

impl ToStr for ErrorReason {
    fn to_str(&self) -> ~str { fmt!("%?", self) }
}


pub trait SkipWhitespaceIterable<'self> {
    pub fn skip_whitespace(self) -> SkipWhitespaceIterator<'self>;
}

impl<'self> SkipWhitespaceIterable<'self> for &'self [(ComponentValue, SourceLocation)] {
    pub fn skip_whitespace(self) -> SkipWhitespaceIterator<'self> {
        SkipWhitespaceIterator{ iter: self.iter() }
    }
}

struct SkipWhitespaceIterator<'self> {
    iter: vec::VecIterator<'self, (ComponentValue, SourceLocation)>,
}

impl<'self> Iterator<&'self ComponentValue> for SkipWhitespaceIterator<'self> {
    fn next(&mut self) -> Option<&'self ComponentValue> {
        loop {
            match self.iter.next() {
                Some(&(WhiteSpace, _)) => (),
                Some(&(ref component_value, _)) => return Some(component_value),
                None => return None
            }
        }
    }
}
