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


const MAX_UNICODE: uint = 0x10FFFF;


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
    Colon,  // :
    Semicolon,  // ;
    OpenBraket, // [
    OpenParen, // (
    OpenBrace, // {
    CloseBraket, // ]
    CloseParen, // )
    CloseBrace, // }
}


struct State {
    input: ~str,
    length: uint,
    transform_function_whitespace: bool,
    quirks_mode: bool,
    mut position: uint,
    mut errors: ~[~str]
}


#[inline(always)]
fn is_eof(state: &State) -> bool {
    state.position >= state.length
}


#[inline(always)]
fn current_char(state: &State) -> char {
    str::char_at(state.input, state.position)
}


// Return value may be smaller than n if we’re near the end of the input.
#[inline(always)]
fn next_n_bytes(state: &State, n: uint) -> ~str {
    str::slice(state.input, state.position,
               uint::min(state.position + n, state.length))
}


#[inline(always)]
fn consume_char(state: &State) -> char {
    let range = str::char_range_at(state.input, state.position);
    state.position = range.next;
    range.ch
}


// http://dev.w3.org/csswg/css3-syntax/#tokenization
pub fn tokenize(input: &str, transform_function_whitespace: bool,
            quirks_mode: bool) -> {tokens: ~[Token], parse_errors: ~[~str]} {
    let input = cssparser::preprocess(input);
    let state = &State {
        input: input, length: input.len(), quirks_mode: quirks_mode,
        transform_function_whitespace: transform_function_whitespace,
        position: 0, errors: ~[] };
    let mut tokens: ~[Token] = ~[];

    while !is_eof(state) {
        tokens.push(consume_token(state))
    }

    // Work around `error: moving out of mutable field`
    // TODO: find a cleaner way.
    let mut errors: ~[~str] = ~[];
    errors <-> state.errors;
    {tokens: tokens, parse_errors: errors}
}


// 3.3.4. Data state
fn consume_token(state: &State) -> Token {
    let c = consume_char(state);
    match c {
        '\t' | '\n' | '\x0C' | ' ' => {
            while !is_eof(state) {
                match current_char(state) {
                    '\t' | '\n' | '\x0C' | ' ' => state.position += 1,
                    _ => break,
                }
            }
            WhiteSpace
        },
        '"' => consume_quoted_string(state, false),
        '\'' => consume_quoted_string(state, true),
        '(' => OpenParen,
        ')' => CloseParen,
        '-' => {
            if is_eof(state) { Delim('-') } else {
                // TODO: CDC, negative numbers
                match current_char(state) {
                    '\\' => {
                        state.position += 1;
                        if is_eof(state) { state.position -= 1; Delim('-') }
                        else {
                            let c = current_char(state);
                            state.position -= 1;
                            match c {
                                '\n' | '\x0C' => Delim('-'),
                                _ => consume_ident(state, '-')
                            }
                        }
                    },
                    'a'..'z' | 'A'..'Z' | '_' => consume_ident(state, c),
                    c if c >= '\xA0' => consume_ident(state, c),  // Non-ASCII
                    _ => {
                        if next_n_bytes(state, 2) == ~"->"
                        { state.position += 2; CDC } else { Delim('-') }
                    }
                }
            }
        }
        '/' if !is_eof(state) && current_char(state) == '*'
            => consume_comment(state),
        ':' => Colon,
        ';' => Semicolon,
        '<' => {
            if next_n_bytes(state, 3) == ~"!--"
            { state.position += 3; CDO } else { Delim('<') }
        }
        '[' => OpenBraket,
        '\\' => {
            if is_eof(state) {
                state.errors.push(~"Invalid escape");
                Delim('\\')
            } else {
                match current_char(state) {
                    '\n' | '\x0C' => {
                        state.errors.push(~"Invalid escape"); Delim('\\') },
                    _ => consume_ident(state, consume_escape(state))
                }
            }
        }
        ']' => CloseBraket,
        '{' => OpenBrace,
        '}' => CloseBrace,
        'a'..'z' | 'A'..'Z' | '_' => consume_ident(state, c),
        c if c >= '\xA0' => consume_ident(state, c),  // Non-ASCII
        _ => Delim(c),
    }
}


// 3.3.5. Double-quote-string state
// 3.3.6. Single-quote-string state
fn consume_quoted_string(state: &State, single_quote: bool) -> Token {
    let mut string: ~str = ~"";
    loop {
        if is_eof(state) {
            state.errors.push(~"EOF in quoted string");
            return String(string);
        }
        match consume_char(state) {
            '"' if !single_quote => return String(string),
            '\'' if single_quote => return String(string),
            '\n' | '\x0C' => {
                state.errors.push(~"Newline in quoted string");
                return BadString
            },
            '\\' => {
                if is_eof(state) {
                    state.errors.push(~"EOF in quoted string");
                    return BadString
                }
                match current_char(state) {
                    // Consume quoted newline
                    '\n' | '\x0C' => state.position += 1,
                    _ =>  str::push_char(&mut string, consume_escape(state))
                }
            }
            c => str::push_char(&mut string, c),
        }
    }
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


// 3.3.12. Ident state
// 3.3.13. Ident-rest state
fn consume_ident(state: &State, initial_char: char) -> Token {
    let mut string = str::from_char(initial_char);
    while !is_eof(state) {
        let c = current_char(state);
        let cc = match c {
            'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '-'  => {
                state.position += 1; c },
            _ if c >= '\xA0' => consume_char(state),  // Non-ASCII
            '\\' => {
                state.position += 1;
                if is_eof(state) { state.position -= 1; break }
                match current_char(state) {
                    '\n' | '\x0C' => { state.position -= 1; break },
                    _ => consume_escape(state)
                }
            },
            '\t' | '\n' | '\x0C' | ' ' if state.transform_function_whitespace
            => {
                state.position += 1;
                return handle_transform_function_whitespace(state, string)
            }
            '(' => {
                state.position += 1;
                // TODO: URLs
                return Function(string)
            },
            _ => break
        };
        str::push_char(&mut string, cc)
    }
    Ident(string)
}


// 3.3.14. Transform-function-whitespace state
fn handle_transform_function_whitespace(state: &State, string: ~str) -> Token {
    while !is_eof(state) {
        match current_char(state) {
            '\t' | '\n' | '\x0C' | ' ' => state.position += 1,
            '(' => { state.position += 1; return Function(string) }
            _ => break,
        }
    }
    // XXX I think the spec is wrong here.
    // See http://lists.w3.org/Archives/Public/www-style/2013Jan/0266.html
    // Go back for one whitespace character.
    state.position -= 1;
    return Ident(string)
}


// 3.3.27. Consume an escaped character
// Assumes that the U+005C REVERSE SOLIDUS (\) has already been consumed
// and that the next input character has already been verified
// to not be a newline or EOF.
fn consume_escape(state: &State) -> char {
    let c = consume_char(state);
    match c {
        '0'..'9' | 'A'..'F' | 'a'..'f' => {
            let mut hex = ~[c];
            while hex.len() < 6 && !is_eof(state) {
                let c = current_char(state);
                match c {
                    '0'..'9' | 'A'..'F' | 'a'..'f' => {
                        hex.push(c); state.position += 1 },
                    _ => break
                }
            }
            if !is_eof(state) {
                match current_char(state) {
                    '\t' | '\n' | '\x0C' | ' ' => state.position += 1,
                    _ => ()
                }
            }
            char_from_hex(hex)
        },
        c => c
    }
}


fn char_from_hex(hex: &[char]) -> char {
    let mut value = 0u;
    for hex.each |cc| {
        let c = *cc;
        value = value * 16 + c as uint - match c {
            '0'..'9' => '0' as uint,
            'A'..'F' => 'A' as uint - 10,
            'a'..'f' => 'a' as uint - 10,
            _ => fail
        }
    }
    if value == 0 || value > MAX_UNICODE {
        '\uFFFD'  // Replacement character
    } else {
        value as char
    }
}


#[test]
fn test_tokenizer() {

    fn assert_tokens(input: &str, expected_tokens: &[Token],
                     expected_errors: &[~str]) {
        assert_tokens_flags(
            input, false, false, expected_tokens, expected_errors)
    }

    fn assert_tokens_flags(
            input: &str,
            transform_function_whitespace: bool, quirks_mode: bool,
            expected_tokens: &[Token], expected_errors: &[~str]) {
        let result = tokenize(
            input, transform_function_whitespace, quirks_mode);
        let tokens: &[Token] = result.tokens;
        let parse_errors: &[~str] = result.parse_errors;
        if tokens != expected_tokens {
            fail fmt!("%? != %?", tokens, expected_tokens);
        }
        if parse_errors != expected_errors {
            fail fmt!("%? != %?", parse_errors, expected_errors);
        }
    }
    assert_tokens("", [], []);
    assert_tokens("?/", [Delim('?'), Delim('/')], []);
    assert_tokens("?/* Li/*psum… */", [Delim('?'), Comment], []);
    assert_tokens("?/* Li/*psum… *//", [Delim('?'), Comment, Delim('/')], []);
    assert_tokens("?/* Lipsum", [Delim('?'), Comment], [~"EOF in comment"]);
    assert_tokens("?/*", [Delim('?'), Comment], [~"EOF in comment"]);
    assert_tokens("[?}{)",
        [OpenBraket, Delim('?'), CloseBrace, OpenBrace, CloseParen], []);

    assert_tokens("(\n \t'Lore\\6d \"ipsu\\6D'",
        [OpenParen, WhiteSpace, String(~"Lorem\"ipsum")], []);
    assert_tokens("'\\''", [String(~"'")], []);
    assert_tokens("\"\\\"\"", [String(~"\"")], []);
    assert_tokens("\"\\\"", [String(~"\"")], [~"EOF in quoted string"]);
    assert_tokens("'\\", [BadString], [~"EOF in quoted string"]);
    assert_tokens("\"0\\0000000\"", [String(~"0\uFFFD0")], []);
    assert_tokens("\"0\\000000 0\"", [String(~"0\uFFFD0")], []);
    assert_tokens("'z\n'a", [BadString, String(~"a")],
        [~"Newline in quoted string", ~"EOF in quoted string"]);

    assert_tokens("Lorem\\ ipsu\\6D dolor \\sit",
        [Ident(~"Lorem ipsumdolor"), WhiteSpace, Ident(~"sit")], []);
    assert_tokens("foo\\", [Ident(~"foo"), Delim('\\')], [~"Invalid escape"]);
    assert_tokens("foo\\\nbar",
        [Ident(~"foo"), Delim('\\'), WhiteSpace, Ident(~"bar")],
        [~"Invalid escape"]);
    assert_tokens("-Lipsum", [Ident(~"-Lipsum")], []);
    assert_tokens("-\\Lipsum", [Ident(~"-Lipsum")], []);
    assert_tokens("func()", [Function(~"func"), CloseParen], []);
    assert_tokens("func ()",
        [Ident(~"func"), WhiteSpace, OpenParen, CloseParen], []);
    assert_tokens_flags("func ()", true, false,
        [Function(~"func"), CloseParen], []);

    assert_tokens("<!-<!-----><",
        [Delim('<'), Delim('!'), Delim('-'), CDO, Delim('-'), CDC, Delim('<')],
        []);
}
