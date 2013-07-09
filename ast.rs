use std::{f64, i32};


#[deriving(Eq)]
pub struct NumericValue {
    representation: ~str,
    value: f64,
    int_value: Option<i32>,
}

impl NumericValue {
    pub fn new(representation: ~str, is_integer: bool) -> NumericValue {
        // TODO: handle overflow
        NumericValue {
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
        }
    }
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
    IncludeMath, // ~=
    DashMatch, // |=
    PrefixMatch, // ^=
    SuffixMatch, // $=
    SubstringMatch, // *=
    Column, // ||
    CDO,  // <!--
    CDC,  // -->

    // Function
    Function(~str, ~[ComponentValue]),  // name, arguments

    // Simple block
    ParenthesisBlock(~[ComponentValue]),  // (…)
    SquareBraketBlock(~[ComponentValue]),  // […]
    CurlyBraketBlock(~[ComponentValue]),  // {…}

    // These are always invalid
    BadURL,
    BadString,
    CloseParenthesis, // )
    CloseSquareBraket, // ]
    CloseCurlyBraket, // }
}


#[deriving(Eq)]
pub struct Declaration {
    name: ~str,
    value: ~[ComponentValue],
    important: bool,
}

#[deriving(Eq)]
pub struct QualifiedRule {
    prelude: ~[ComponentValue],
    block: ~[ComponentValue],
}

#[deriving(Eq)]
pub struct AtRule {
    name: ~str,
    prelude: ~[ComponentValue],
    block: Option<~[ComponentValue]>,
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
