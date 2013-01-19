// http://dev.w3.org/csswg/css3-syntax/#tokenization
//
// The output of the tokenization step is a series of zero or more
// of the following tokens:
// ident, function, at-keyword, hash, string, bad-string, url, bad-url,
// delim, number, percentage, dimension, unicode-range, whitespace, comment,
// cdo, cdc, colon, semicolon, [, ], (, ), {, }.
//
// ident, function, at-keyword, hash, string, and url tokens
// have a value composed of zero or more characters.
// Delim tokens have a value composed of a single character.
// Number, percentage, and dimension tokens have a representation
// composed of 1 or more character, a numeric value,
// and a type flag set to either "integer" or "number".
// The type flag defaults to "integer" if not otherwise set.
// Dimension tokens additionally have a unit
// composed of one or more characters.
// Unicode-range tokens have a range of characters.


use cssparser;


#[deriving_eq]
enum NumericValue {
    Integer(int),
    // The spec calls this "number".
    // Use "float" instead to reduce term overloading with "number token".
    Float(float),
}
// TODO: add a NumberValue.as_float() method.


#[deriving_eq]
enum Token {
    Ident(~str),
    Function(~str),
    AtKeyword(~str),
    Hash(~str),
    String(~str),
    BadString,
    URL(~str),
    BadURL,
    Delim(char),
    Number(NumericValue, ~str),  // value, representation
    Percentage(NumericValue, ~str),  // value, representation
    Dimension(NumericValue, ~str, ~str),  // value, representation, unit
    UnicodeRange(char, char),  // start, end. XXX what about empty ranges?
    WhiteSpace,
    Comment,
    CDO,  // <!--
    CDC,  // -->
    Colon,  // ,
    Semicolon,  // ;
    OpenBraket, // [
    OpenParen, // (
    OpenBrace, // {
    CloseBraket, // ]
    CloseParen, // (
    CloseBrace, // }
}


struct State {
    input: ~str,
    mut position: uint,
    errors: ~[~str]
}


fn is_eof(state: &State) -> bool {
    state.input[state.position] == 0
}


fn consume_char(state: &State) -> char {
    let range = str::char_range_at(state.input, state.position);
    state.position = range.next;
    range.ch
}


// http://dev.w3.org/csswg/css3-syntax/#tokenization
fn tokenize(input: &str//, transform_function_whitespace: bool,
//            quirks_mode: bool
            ) -> ~[Token] {
    let state = &State {
        input: cssparser::preprocess(input) + "\x00",
        position: 0, errors: ~[] };
    let mut tokens: ~[Token] = ~[];

    // 3.3.4. Data state
    while !is_eof(state) {
        tokens.push(match consume_char(state) {
            c => Delim(c),
        })
    }
    tokens
}


#[test]
fn test_tokenizer() {
    fn assert_tokens(input: &str, expected: &[Token]) {
        let result: &[Token] = tokenize(input//, false, false
        );
        if result != expected {
            fail fmt!("%? != %?", result, expected);
        }
    }
    assert_tokens("", []);
    assert_tokens(",", [Delim(',')]);
}
