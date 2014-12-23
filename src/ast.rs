/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use std::fmt;


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


pub type Node = (Token, SourceLocation);  // TODO this is not a good name


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


#[deriving(PartialEq)]
pub struct Declaration {
    pub location: SourceLocation,
    pub name: String,
    pub value: Vec<Token>,
    pub important: bool,
}

#[deriving(PartialEq)]
pub struct QualifiedRule {
    pub location: SourceLocation,
    pub prelude: Vec<Token>,
    pub block: Vec<Node>,
}

#[deriving(PartialEq)]
pub struct AtRule {
    pub location: SourceLocation,
    pub name: String,
    pub prelude: Vec<Token>,
    pub block: Option<Vec<Node>>,
}

#[deriving(PartialEq)]
pub enum DeclarationListItem {
    Declaration(Declaration),
    AtRule(AtRule),
}

#[deriving(PartialEq)]
pub enum Rule {
    QualifiedRule(QualifiedRule),
    AtRule(AtRule),
}

#[deriving(PartialEq)]
pub struct SyntaxError {
    pub location: SourceLocation,
    pub reason: ErrorReason,
}

#[deriving(PartialEq, Show)]
pub enum ErrorReason {
    EmptyInput,  // Parsing a single "thing", found only whitespace.
    ExtraInput,  // Found more non-whitespace after parsing a single "thing".
    MissingQualifiedRuleBlock,  // EOF in a qualified rule prelude, before '{'
    InvalidDeclarationSyntax,
    InvalidBangImportantSyntax,
    // This is meant to be extended
}

impl fmt::Show for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{} {}", self.location.line, self.location.column, self.reason)
    }
}
