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


const MAX_UNICODE: char = '\U0010FFFF';


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
    UnicodeRange(char, char),  // start, end
    EmptyUnicodeRange,
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
    transform_function_whitespace: bool,
    quirks_mode: bool,
    input: ~str,
    length: uint,  // Counted in bytes, not characters
    mut position: uint,  // Counted in bytes, not characters
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


// Return value may be smaller than n if we’re near the end of the input.
#[inline(always)]
fn next_n_chars(state: &State, n: uint) -> ~[char] {
    let mut chars: ~[char] = ~[];
    let mut position = state.position;
    for n.times {
        if position >= state.length { break }
        let range = str::char_range_at(state.input, position);
        position = range.next;
        chars.push(range.ch);
    }
    chars
}


#[inline(always)]
fn consume_char(state: &State) -> char {
    let range = str::char_range_at(state.input, state.position);
    state.position = range.next;
    range.ch
}


macro_rules! is_match(
    ($value:expr, $pattern:pat) => (
        match $value { $pattern => true, _ => false }
    );
)


#[inline(always)]
fn is_invalid_escape(state: &State) -> bool {
    match next_n_bytes(state, 2) {
        ~"\\\n" | ~"\\\x0C" | ~"\\" => true,
        _ => false,
    }
}


#[inline(always)]
fn is_namestart_or_escape(state: &State) -> bool {
    match current_char(state) {
        'a'..'z' | 'A'..'Z' | '_' => true,
        '\\' => !is_invalid_escape(state),
        c => c >= '\xA0',  // Non-ASCII
    }
}


#[inline(always)]
fn next_is_namestart_or_escape(state: &State) -> bool {
    state.position += 1;
    let result = !is_eof(state) && is_namestart_or_escape(state);
    state.position -= 1;
    result
}


macro_rules! push_char(
    ($string:ident, $c:expr) => (
        str::push_char(&mut $string, $c)
    );
)


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
    let c = current_char(state);
    match c {
        '-' => {
            if next_n_bytes(state, 3) == ~"-->" { state.position += 3; CDC }
            else { consume_numeric(state) }
        },
        '<' => {
            if next_n_bytes(state, 4) == ~"<!--" { state.position += 4; CDO }
            else  { state.position += 1; Delim('<') }
        },
        '0'..'9' | '.' | '+' => consume_numeric(state),
        'u' | 'U' => consume_unicode_range(state),
        'a'..'z' | 'A'..'Z' | '_' | '\\' => consume_ident(state),
        _ if c >= '\xA0' => consume_ident(state),  // Non-ASCII
        _ => {
            match consume_char(state) {
                '\t' | '\n' | '\x0C' | ' ' => {
                    while !is_eof(state) {
                        match current_char(state) {
                            '\t' | '\n' | '\x0C' | ' ' => state.position += 1,
                            _ => break,
                        }
                    }
                    WhiteSpace
                },
                '"' => consume_quoted_string(state, false, false),
                '#' => consume_hash(state),
                '\'' => consume_quoted_string(state, true, false),
                '(' => OpenParen,
                ')' => CloseParen,
                '/' if !is_eof(state) && current_char(state) == '*'
                    => consume_comment(state),
                ':' => Colon,
                ';' => Semicolon,
                '@' => consume_at_keyword(state),
                '[' => OpenBraket,
                ']' => CloseBraket,
                '{' => OpenBrace,
                '}' => CloseBrace,
                _ => Delim(c)
            }
        }
    }
}


// 3.3.5. Double-quote-string state
// 3.3.6. Single-quote-string state
fn consume_quoted_string(state: &State, single_quote: bool,
                         eof_is_bad: bool) -> Token {
    let mut string: ~str = ~"";
    while !is_eof(state) {
        match consume_char(state) {
            '"' if !single_quote => return String(string),
            '\'' if single_quote => return String(string),
            '\n' | '\x0C' => {
                state.errors.push(~"Newline in quoted string");
                return BadString
            },
            '\\' => {
                match next_n_bytes(state, 1) {
                    ~"\n" | ~"\x0C" => state.position += 1, // Quoted newline
                    ~"" => {
                        // Even if not eof_is_bad
                        state.errors.push(~"EOF in quoted string");
                        return BadString
                    },
                    _ => push_char!(string, consume_escape(state))
                }
            }
            c => push_char!(string, c),
        }
    }
    state.errors.push(~"EOF in quoted string");
    if eof_is_bad { BadString } else { String(string) }
}


// 3.3.7. Hash state
fn consume_hash(state: &State) -> Token {
    let string = consume_ident_string_rest(state);
    if string == ~"" { Delim('#') } else { Hash(string) }
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


// 3.3.10. At-keyword state
fn consume_at_keyword(state: &State) -> Token {
    match consume_ident_string(state) {
        Some(string) => AtKeyword(string),
        None => Delim('@')
    }
}


// 3.3.12. Ident state
fn consume_ident(state: &State) -> Token {
    match consume_ident_string(state) {
        Some(string) => {
            if is_eof(state) { return Ident(string) }
            match current_char(state) {
                '\t' | '\n' | '\x0C' | ' '
                        if state.transform_function_whitespace => {
                    state.position += 1;
                    handle_transform_function_whitespace(state, string)
                }
                '(' => {
                    state.position += 1;
                    if cssparser::ascii_lower(string) == ~"url"
                    { consume_url(state) } else { Function(string) }
                },
                _ => Ident(string)
            }
        },
        None => match current_char(state) {
            '-' => {
                state.position += 1;
                Delim('-')
            },
            '\\' => {
                state.errors.push(~"Invalid escape");
                state.position += 1;
                Delim('\\')
            },
            _ => fail,  // Should not have called consume_ident() here.
        }
    }
}

fn consume_ident_string(state: &State) -> Option<~str> {
    match current_char(state) {
        '-' => if !next_is_namestart_or_escape(state) { None }
               else { Some(consume_ident_string_rest(state)) },
        '\\' if is_invalid_escape(state) => return None,
        _ if !is_namestart_or_escape(state) => return None,
        _ => Some(consume_ident_string_rest(state))
    }
}


// 3.3.13. Ident-rest state
fn consume_ident_string_rest(state: &State) -> ~str {
    let mut string = ~"";
    while !is_eof(state) {
        let c = current_char(state);
        let next_char = match c {
            'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '-'  => {
                state.position += 1; c },
            _ if c >= '\xA0' => consume_char(state),  // Non-ASCII
            '\\' => {
                if is_invalid_escape(state) { break }
                state.position += 1;
                consume_escape(state)
            },
            _ => break
        };
        push_char!(string, next_char)
    }
    string
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
    // Go back for one whitespace character.
    state.position -= 1;
    return Ident(string)
}


// 3.3.15. Number state
fn consume_numeric(state: &State) -> Token {
    let c = consume_char(state);
    match c {
        '-' | '+' => match consume_numeric_sign(state, c) {
            Delim('-') => { state.position -= 1; return consume_ident(state) },
            token => token,
        },
        '.' => match current_char(state) {
            '0'..'9' => consume_numeric_fraction(state, ~"."),
            _ => Delim('.'),
        },
        '0'..'9' => consume_numeric_rest(state, c),
        _ => fail, // consume_numeric() should not have been called here.
    }
}


fn consume_numeric_sign(state: &State, sign: char) -> Token {
    if is_eof(state) { return Delim(sign) }
    match current_char(state) {
        '.' => {
            state.position += 1;
            if !is_eof(state) && is_match!(current_char(state), '0'..'9') {
                consume_numeric_fraction(state, str::from_char(sign) + ~".")
            } else {
                state.position -= 1; Delim(sign)
            }
        },
        '0'..'9' => consume_numeric_rest(state, sign),
        _ => Delim(sign)
    }
}


// 3.3.16. Number-rest state
fn consume_numeric_rest(state: &State, initial_char: char) -> Token {
    let mut string = str::from_char(initial_char);
    while !is_eof(state) {
        let c = current_char(state);
        match c {
            '0'..'9' => { push_char!(string, c); state.position += 1 },
            '.' => {
                state.position += 1;
                if !is_eof(state) && is_match!(current_char(state), '0'..'9') {
                    push_char!(string, '.');
                    return consume_numeric_fraction(state, string);
                } else {
                    state.position -= 1; break
                }
            },
            _ => match consume_scientific_number(state, string) {
                Ok(token) => return token,
                Err(s) => { string = s; break }
            }
        }
    }
    let value = Integer(int::from_str(string).get());
    consume_numeric_end(state, string, value)
}


// 3.3.17. Number-fraction state
fn consume_numeric_fraction(state: &State, string: ~str) -> Token {
    let mut string: ~str = string;
    while !is_eof(state) {
        match current_char(state) {
            '0'..'9' => push_char!(string, consume_char(state)),
            _ => match consume_scientific_number(state, string) {
                Ok(token) => return token,
                Err(s) => { string = s; break }
            }
        }
    }
    let value = Float(float::from_str(string).get());
    consume_numeric_end(state, string, value)
}


fn consume_numeric_end(state: &State, string: ~str,
                       value: NumericValue) -> Token {
    match current_char(state) {
        '%' => { state.position += 1; Percentage(value, string) },
        _ => {
            // 3.3.18. Dimension state (kind of)
            match consume_ident_string(state) {
                Some(unit) => Dimension(value, string, unit),
                None => Number(value, string),
            }
        },
    }
}


fn consume_scientific_number(state: &State, string: ~str)
        -> Result<Token, ~str> {
    let next_3 = next_n_chars(state, 3);
    if (next_3.len() == 3
        && (next_3[0] == 'e' || next_3[0] == 'E')
        && (next_3[1] == '+' || next_3[1] == '-')
        && is_match!(next_3[2], '0'..'9')
    ) {
        let mut string: ~str = string;
        push_char!(string, next_3[0]);
        push_char!(string, next_3[1]);
        push_char!(string, next_3[2]);
        state.position += 3;
        // 3.3.19. Sci-notation state
        while !is_eof(state) && is_match!(current_char(state), '0'..'9') {
            push_char!(string, consume_char(state))
        }
        let value = Float(float::from_str(string).get());
        Ok(Number(value, string))
    } else {
        Err(string)
    }
}


// 3.3.20. URL state
fn consume_url(state: &State) -> Token {
    while !is_eof(state) {
        match current_char(state) {
            '\t' | '\n' | '\x0C' | ' ' => state.position += 1,
            '"' => return consume_quoted_url(state, false),
            '\'' => return consume_quoted_url(state, true),
            ')' => { state.position += 1; return URL(~"") },
            _ => return consume_unquoted_url(state),
        }
    }
    state.errors.push(~"EOF in URL");
    return BadURL
}


// 3.3.21. URL-double-quote state
// 3.3.22. URL-single-quote state
fn consume_quoted_url(state: &State, single_quote: bool) -> Token {
    state.position += 1;  // The initial quote
    match consume_quoted_string(state, single_quote, true) {
        String(string) => consume_url_end(state, string),
        BadString => consume_bad_url(state, false),
        _ => fail,
    }
}



// 3.3.23. URL-end state
fn consume_url_end(state: &State, string: ~str) -> Token {
    while !is_eof(state) {
        match consume_char(state) {
            '\t' | '\n' | '\x0C' | ' ' => (),
            ')' => return URL(string),
            _ => return consume_bad_url(state, true)
        }
    }
    state.errors.push(~"EOF in URL");
    BadURL
}


// 3.3.24. URL-unquoted state
fn consume_unquoted_url(state: &State) -> Token {
    let mut string = ~"";
    while !is_eof(state) {
        let next_char = match consume_char(state) {
            '\t' | '\n' | '\x0C' | ' ' => return consume_url_end(state, string),
            ')' => return URL(string),
            '\x00'..'\x08' | '\x0E'..'\x1F' | '\x7F'..'\x9F'  // non-printable
                | '"' | '\'' | '(' => return consume_bad_url(state, true),
            '\\' => match next_n_bytes(state, 1) {
                ~"\n" | ~"\x0C" | ~"" => return consume_bad_url(state, true),
                _ => consume_escape(state)
            },
            c => c
        };
        push_char!(string, next_char)
    }
    state.errors.push(~"EOF in URL");
    BadURL
}


// 3.3.25. Bad-URL state
fn consume_bad_url(state: &State, log_error: bool) -> Token {
    if log_error { state.errors.push(~"Invalid URL syntax"); }
    // Consume up to the closing )
    while !is_eof(state) {
        match consume_char(state) {
            ')' => break,
            '\\' => state.position += 1, // Skip an escaped ) or \
            _ => ()
        }
    }
    BadURL
}


// 3.3.26. Unicode-range state
fn consume_unicode_range(state: &State) -> Token {
    let next_3 = next_n_chars(state, 3);
    // We got here with U or u
    assert next_3[0] == 'u' || next_3[0] == 'U';
    // Check if this is indeed an unicode range. Fallback on ident.
    if next_3.len() == 3 && next_3[1] == '+' {
        match next_3[2] {
            '0'..'9' | 'a'..'f' | 'A'..'F' => state.position += 2,
            _ => { return consume_ident(state) }
        }
    } else { return consume_ident(state) }

    let mut hex = ~[];
    while hex.len() < 6 && !is_eof(state) {
        let c = current_char(state);
        match c {
            '0'..'9' | 'A'..'F' | 'a'..'f' => {
                hex.push(c); state.position += 1 },
            _ => break
        }
    }
    assert hex.len() > 0;
    let max_question_marks = 6u - hex.len();
    let mut question_marks = 0u;
    while question_marks < max_question_marks && !is_eof(state)
            && current_char(state) == '?' {
        question_marks += 1;
        state.position += 1
    }
    let start: char, end: char;
    if question_marks > 0 {
        start = char_from_hex(hex + vec::from_elem(question_marks, '0'));
        end = char_from_hex(hex + vec::from_elem(question_marks, 'F'));
    } else {
        start = char_from_hex(hex);
        hex = ~[];
        if !is_eof(state) && current_char(state) == '-' {
            state.position += 1;
            while hex.len() < 6 && !is_eof(state) {
                let c = current_char(state);
                match c {
                    '0'..'9' | 'A'..'F' | 'a'..'f' => {
                        hex.push(c); state.position += 1 },
                    _ => break
                }
            }
        }
        end = if hex.len() > 0 { char_from_hex(hex) } else { start }
    }
    // 3.3.28. Set the unicode-range token's range
    if start > MAX_UNICODE || end < start { EmptyUnicodeRange }
    else { UnicodeRange(start,
                        if end <= MAX_UNICODE { end } else { MAX_UNICODE }) }
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
            let c = char_from_hex(hex);
            if '\x00' < c && c <= MAX_UNICODE { c }
            else { '\uFFFD' }  // Replacement character
        },
        c => c
    }
}


fn char_from_hex(hex: &[char]) -> char {
    uint::from_str_radix(str::from_chars(hex), 16).get() as char
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
            fail fmt!("%?\n!=\n%?", tokens, expected_tokens);
        }
        if parse_errors != expected_errors {
            fail fmt!("%?\n!=\n%?", parse_errors, expected_errors);
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

    assert_tokens("##00(#\\##\\\n#\\",
        [Delim('#'), Hash(~"00"), OpenParen, Hash(~"#"), Delim('#'),
         Delim('\\'), WhiteSpace, Delim('#'), Delim('\\')],
        [~"Invalid escape", ~"Invalid escape"]);

    assert_tokens("@@page(@\\x@-x@-\\x@--@\\\n@\\",
        [Delim('@'), AtKeyword(~"page"), OpenParen, AtKeyword(~"x"),
         AtKeyword(~"-x"), AtKeyword(~"-x"), Delim('@'), Delim('-'), Delim('-'),
         Delim('@'), Delim('\\'), WhiteSpace, Delim('@'), Delim('\\')],
        [~"Invalid escape", ~"Invalid escape"]);

    assert_tokens("<!-<!-----><",
        [Delim('<'), Delim('!'), Delim('-'), CDO, Delim('-'), CDC, Delim('<')],
        []);
    assert_tokens("u+g u+fU+4?U+030-000039f U+FFFFF?U+42-42U+42-41U+42-110000",
        [Ident(~"u"), Delim('+'), Ident(~"g"), WhiteSpace,
         UnicodeRange('\x0F', '\x0F'), UnicodeRange('\x40', '\x4F'),
         UnicodeRange('0', '9'), Ident(~"f"), WhiteSpace, EmptyUnicodeRange,
         UnicodeRange('B', 'B'), EmptyUnicodeRange,
         UnicodeRange('B', '\U0010FFFF')],
        []);

    assert_tokens("url()URL()uRl()Ürl()",
        [URL(~""), URL(~""), URL(~""), Function(~"Ürl"), CloseParen], []);
    assert_tokens("url(  )url(\ta\n)url(\t'a'\n)url(\t'a'z)url(  ",
        [URL(~""), URL(~"a"), URL(~"a"), BadURL, BadURL],
        [~"Invalid URL syntax", ~"EOF in URL"]);
    assert_tokens("url('a\nb')url('a", [BadURL, BadURL],
        [~"Newline in quoted string", ~"EOF in quoted string"]);
    assert_tokens("url(a'b)url(\x08z)url('a'", [BadURL, BadURL, BadURL],
        [~"Invalid URL syntax", ~"Invalid URL syntax", ~"EOF in URL"]);
    assert_tokens("url(Lorem\\ ipsu\\6D dolo\\r)url(a\nb)url(a\\\nb)",
        [URL(~"Lorem ipsumdolor"), BadURL, BadURL],
        [~"Invalid URL syntax", ~"Invalid URL syntax"]);
}
