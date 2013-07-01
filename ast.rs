#[deriving(Eq)]
pub enum NumericValue {
    Integer(int),
    // The spec calls this "number".
    // Use "float" instead to reduce term overloading with "number token".
    Float(float),
}


impl NumericValue {
    fn to_float(&self) -> float {
        match *self {
            Integer(value) => value as float,
            Float(value) => value,
        }
    }
}


#[deriving(Eq)]
pub enum Primitive {
    // Preserved tokens. Same as in the tokenizer.
    Ident(~str),
    AtKeyword(~str),
    Hash(~str),
    String(~str),
    URL(~str),
    Delim(char),
    Number(NumericValue, ~str),  // value, representation
    Percentage(NumericValue, ~str),  // value, representation
    Dimension(NumericValue, ~str, ~str),  // value, representation, unit
    UnicodeRange(char, char),  // start, end
    EmptyUnicodeRange,
    WhiteSpace,
    Colon,  // :
    Semicolon,  // ;

    // Function
    Function(~str, ~[~[Primitive]]),  // name, arguments

    // Simple block
    ParenthesisBlock(~[Primitive]),  // (…)
    SquareBraketBlock(~[Primitive]),  // […]
    CurlyBraketBlock(~[Primitive]),  // {…}

    // These are alwas invalid
    BadURL,
    BadString,
    CDO,  // <!--
    CDC,  // -->
    CloseParenthesis, // )
    CloseSquareBraket, // ]
    CloseCurlyBraket, // }
}


#[deriving(Eq)]
pub struct Declaration {
    name: ~str,
    value: ~[Primitive],
    important: bool,
}

#[deriving(Eq)]
pub struct StyleRule {
    selector: ~[Primitive],
    value: ~[DeclarationBlockItem],
}

#[deriving(Eq)]
pub struct AtRule {
    name: ~str,
    prelude: ~[Primitive],
    block: Option<~[Primitive]>,
}

#[deriving(Eq)]
pub enum DeclarationBlockItem {
    Declaration(Declaration),
    // A better idea for a name that means "at-rule" but is not "AtRule"?
    Decl_AtRule(AtRule),
}

#[deriving(Eq)]
pub enum Rule {
    StyleRule(StyleRule),
    AtRule(AtRule),
}
