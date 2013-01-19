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
    length: uint,
    mut position: uint,
    mut errors: ~[~str]
}


fn is_eof(state: &State) -> bool {
    state.position >= state.length
}


fn current_char(state: &State) -> char {
    str::char_at(state.input, state.position)
}


fn consume_char(state: &State) -> char {
    let range = str::char_range_at(state.input, state.position);
    state.position = range.next;
    range.ch
}


// 3.3.9. Comment state
fn consume_comment(state: &State) -> Token {
    state.position += 1;  // consume the * in /*
    match str::find_str_from(state.input, "*/", state.position) {
        Some(end_position) => state.position = end_position + 2,
        None => {
            state.errors.push(~"EOF in comment");
            state.position = state.input.len();
        }
    }
    Comment
}


// http://dev.w3.org/csswg/css3-syntax/#tokenization
fn tokenize(input: &str//, transform_function_whitespace: bool,
//            quirks_mode: bool
            ) -> {tokens: ~[Token], parse_errors: ~[~str]} {
    let input = cssparser::preprocess(input);
    let state = &State {
        input: input, length: input.len(),
        position: 0, errors: ~[] };
    let mut tokens: ~[Token] = ~[];

    // 3.3.4. Data state
    while !is_eof(state) {
        tokens.push(match consume_char(state) {
            '/' if !is_eof(state) && current_char(state) == '*'
                => consume_comment(state),
            c => Delim(c),
        })
    }

    // Work around `error: moving out of mutable field`
    // TODO: find a cleaner way.
    let mut errors: ~[~str] = ~[];
    errors <-> state.errors;
    {tokens: tokens, parse_errors: errors}
}


#[test]
fn test_tokenizer() {
    fn assert_tokens(input: &str, expected_tokens: &[Token],
                     expected_errors: &[~str]) {
        let result = tokenize(input//, false, false
        );
        let tokens: &[Token] = result.tokens;
        let parse_errors: &[~str] = result.parse_errors;
        if tokens != expected_tokens {
            fail fmt!("%? != %?", tokens, expected_tokens);
        }
        if parse_errors != expected_errors {
            fail fmt!("%? != %?", tokens, expected_errors);
        }
    }
    assert_tokens("", [], []);
    assert_tokens(",/", [Delim(','), Delim('/')], []);
    assert_tokens(",/* Li/*psum… */", [Delim(','), Comment], []);
    assert_tokens(",/* Li/*psum… *//", [Delim(','), Comment, Delim('/')], []);
    assert_tokens(",/* Lipsum", [Delim(','), Comment], [~"EOF in comment"]);
    assert_tokens(",/*", [Delim(','), Comment], [~"EOF in comment"]);
}
