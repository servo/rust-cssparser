#[deriving(Eq)]
pub struct NumericValue {
    representation: ~str,
    value: f64,
    int_value: Option<i32>,
}

pub impl NumericValue {
    fn new(representation: ~str, is_integer: bool) -> NumericValue {
        // TODO: handle overflow
        NumericValue {
            int_value: if is_integer { Some(
                // Remove any + sign as int::from_str() does not parse them.
                if representation[0] != '+' as u8 {
                    i32::from_str(representation)
                } else {
                    i32::from_str(str::slice(
                        representation, 1, representation.len()))
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
    CDO,  // <!--
    CDC,  // -->

    // Function
    Function(~str, ~[ComponentValue]),  // name, arguments

    // Simple block
    ParenthesisBlock(~[ComponentValue]),  // (…)
    SquareBraketBlock(~[ComponentValue]),  // […]
    CurlyBraketBlock(~[ComponentValue]),  // {…}

    // These are alwas invalid
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
pub enum DeclarationBlockItem {
    Declaration(Declaration),
    // A better idea for a name that means "at-rule" but is not "AtRule"?
    Decl_AtRule(AtRule),
}

#[deriving(Eq)]
pub enum Rule {
    QualifiedRule(QualifiedRule),
    AtRule(AtRule),
}
