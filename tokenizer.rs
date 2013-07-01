// http://dev.w3.org/csswg/css3-syntax/#tokenization
//
// The output of the tokenization step is a series of zero or more
// of the following tokens:
// ident, function, at-keyword, hash, string, bad-string, url, bad-url,
// delim, number, percentage, dimension, unicode-range, whitespace,
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


use super::utils::*;
use super::ast::{NumericValue,Float,Integer};


pub impl Tokenizer {
    fn from_str(input: &str)
            -> ~Tokenizer {
        let input = preprocess(input);
        ~Tokenizer {
            length: input.len(),
            input: input,
            position: 0,
        }
    }

    fn next_token(&mut self) -> (Token, Option<ParseError>) {
        if is_eof(self) { (EOF, None) } else { consume_token(self) }
    }
}


pub struct Tokenizer {
    priv input: ~str,
    priv length: uint,  // Counted in bytes, not characters
    priv position: uint,  // Counted in bytes, not characters
}


#[deriving(Eq)]
pub enum Token {
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
    CDO,  // <!--
    CDC,  // -->
    Colon,  // :
    Semicolon,  // ;
    OpenParenthesis, // (
    OpenSquareBraket, // [
    OpenCurlyBraket, // {
    CloseParenthesis, // )
    CloseSquareBraket, // ]
    CloseCurlyBraket, // }
    EOF,
}


//  ***********  End of public API  ***********


static MAX_UNICODE: char = '\U0010FFFF';


// 3.2.1. Preprocessing the input stream
fn preprocess(input: &str) -> ~str {
    // TODO: Is this faster if done in one pass?
    str::replace(str::replace(str::replace(input,
    "\r\n", "\n"),
    "\r", "\n"),
    "\x00", "\uFFFD")
}


#[test]
fn test_preprocess() {
    assert!(preprocess("") == ~"");
    assert!(preprocess("Lorem\r\n\t\x00ipusm\ndoror\uFFFD\r")
        == ~"Lorem\n\t\uFFFDipusm\ndoror\uFFFD\n");
}


#[inline(always)]
fn error_token(t: Token, message: ~str) -> (Token, Option<ParseError>) {
    (t, Some(ParseError{message: message}))
}


#[inline(always)]
fn is_eof(tokenizer: &mut Tokenizer) -> bool {
    tokenizer.position >= tokenizer.length
}


#[inline(always)]
fn current_char(tokenizer: &mut Tokenizer) -> char {
    str::char_at(tokenizer.input, tokenizer.position)
}


// Return value may be smaller than n if we’re near the end of the input.
#[inline(always)]
fn match_here(tokenizer: &mut Tokenizer, needle: ~str) -> bool {
    // XXX Duplicate str::match_at which is not public.
    let mut i = tokenizer.position;
    if i + needle.len() > tokenizer.length { return false }
    let haystack: &str = tokenizer.input;
    for needle.each |c| { if haystack[i] != c { return false; } i += 1u; }
    return true;
}


// Return value may be smaller than n if we’re near the end of the input.
#[inline(always)]
fn next_n_chars(tokenizer: &mut Tokenizer, n: uint) -> ~[char] {
    let mut chars: ~[char] = ~[];
    let mut position = tokenizer.position;
    for n.times {
        if position >= tokenizer.length { break }
        let range = str::char_range_at(tokenizer.input, position);
        position = range.next;
        chars.push(range.ch);
    }
    chars
}


#[inline(always)]
fn consume_char(tokenizer: &mut Tokenizer) -> char {
    let range = str::char_range_at(tokenizer.input, tokenizer.position);
    tokenizer.position = range.next;
    range.ch
}


macro_rules! is_match(
    ($value:expr, $pattern:pat) => (
        match $value { $pattern => true, _ => false }
    );
)


#[inline(always)]
fn is_invalid_escape(tokenizer: &mut Tokenizer) -> bool {
    match next_n_chars(tokenizer, 2) {
        ['\\', '\n'] | ['\\', '\x0C'] | ['\\'] => true,
        _ => false,
    }
}


#[inline(always)]
fn is_namestart_or_escape(tokenizer: &mut Tokenizer) -> bool {
    match current_char(tokenizer) {
        'a'..'z' | 'A'..'Z' | '_' => true,
        '\\' => !is_invalid_escape(tokenizer),
        c => c >= '\x80',  // Non-ASCII
    }
}


#[inline(always)]
fn next_is_namestart_or_escape(tokenizer: &mut Tokenizer) -> bool {
    tokenizer.position += 1;
    let result = !is_eof(tokenizer) && is_namestart_or_escape(tokenizer);
    tokenizer.position -= 1;
    result
}


macro_rules! push_char(
    ($string:ident, $c:expr) => (
        str::push_char(&mut $string, $c)
    );
)


// 4.4.1. Data state
fn consume_token(tokenizer: &mut Tokenizer) -> (Token, Option<ParseError>) {
    // Comments are special because they do not even emit a token,
    // unless they reach EOF which is an error.
    match consume_comments(tokenizer) {
        Some(result) => return result,
        None => ()
    }
    if is_eof(tokenizer) { return (EOF, None) }
    let c = current_char(tokenizer);
    match c {
        '-' => {
            if match_here(tokenizer, ~"-->") {
                tokenizer.position += 3;
                (CDC, None)
            }
            else if next_is_namestart_or_escape(tokenizer) {
                consume_ident(tokenizer)
            } else {
                consume_numeric(tokenizer)
            }
        },
        '<' => {
            if match_here(tokenizer, ~"<!--") {
                tokenizer.position += 4;
                (CDO, None)
            } else {
                tokenizer.position += 1;
                (Delim('<'), None)
            }
        },
        '0'..'9' | '.' | '+' => consume_numeric(tokenizer),
        'u' | 'U' => consume_unicode_range(tokenizer),
        'a'..'z' | 'A'..'Z' | '_' | '\\' => consume_ident(tokenizer),
        _ if c >= '\x80' => consume_ident(tokenizer),  // Non-ASCII
        _ => {
            match consume_char(tokenizer) {
                '\t' | '\n' | '\x0C' | ' ' => {
                    while !is_eof(tokenizer) {
                        match current_char(tokenizer) {
                            '\t' | '\n' | '\x0C' | ' '
                                => tokenizer.position += 1,
                            _ => break,
                        }
                    }
                    (WhiteSpace, None)
                },
                '"' => consume_quoted_string(tokenizer, false),
                '#' => consume_hash(tokenizer),
                '\'' => consume_quoted_string(tokenizer, true),
                '(' => (OpenParenthesis, None),
                ')' => (CloseParenthesis, None),
                ':' => (Colon, None),
                ';' => (Semicolon, None),
                '@' => consume_at_keyword(tokenizer),
                '[' => (OpenSquareBraket, None),
                ']' => (CloseSquareBraket, None),
                '{' => (OpenCurlyBraket, None),
                '}' => (CloseCurlyBraket, None),
                _ => (Delim(c), None)
            }
        }
    }
}


// 4.4.2. Double-quote-string state
// 4.4.3. Single-quote-string state
fn consume_quoted_string(tokenizer: &mut Tokenizer, single_quote: bool)
        -> (Token, Option<ParseError>) {
    let mut string: ~str = ~"";
    while !is_eof(tokenizer) {
        match consume_char(tokenizer) {
            '"' if !single_quote => return (String(string), None),
            '\'' if single_quote => return (String(string), None),
            '\n' | '\x0C' => {
                return error_token(BadString, ~"Newline in quoted string");
            },
            '\\' => {
                match next_n_chars(tokenizer, 1) {
                    // Quoted newline
                    ['\n'] | ['\x0C'] => tokenizer.position += 1,
                    [] =>
                        return error_token(BadString, ~"EOF in quoted string"),
                    _ => push_char!(string, consume_escape(tokenizer))
                }
            }
            c => push_char!(string, c),
        }
    }
    error_token(String(string), ~"EOF in quoted string")
}


// 4.4.4. Hash state
fn consume_hash(tokenizer: &mut Tokenizer) -> (Token, Option<ParseError>) {
    let string = consume_ident_string_rest(tokenizer);
    (if string == ~"" { Delim('#') } else { Hash(string) }, None)
}


// 4.4.6. Comment state
fn consume_comments(tokenizer: &mut Tokenizer)
        -> Option<(Token, Option<ParseError>)> {
    while match_here(tokenizer, ~"/*") {
        tokenizer.position += 2;  // consume /*
        match str::find_str_from(tokenizer.input, "*/", tokenizer.position) {
            Some(end_position) => tokenizer.position = end_position + 2,
            None => {
                tokenizer.position = tokenizer.length;
                return Some(error_token(EOF, ~"Unclosed comment"))
            }
        }
    }
    None
}


// 4.4.7. At-keyword state
fn consume_at_keyword(tokenizer: &mut Tokenizer) -> (Token, Option<ParseError>) {
    (match consume_ident_string(tokenizer) {
        Some(string) => AtKeyword(string),
        None => Delim('@')
    }, None)
}


// 4.4.9. Ident state
fn consume_ident(tokenizer: &mut Tokenizer) -> (Token, Option<ParseError>) {
    match consume_ident_string(tokenizer) {
        Some(string) => {
            if is_eof(tokenizer) { return (Ident(string), None) }
            match current_char(tokenizer) {
                '(' => {
                    tokenizer.position += 1;
                    if ascii_lower(string) == ~"url" { consume_url(tokenizer) }
                    else { (Function(string), None) }
                },
                _ => (Ident(string), None)
            }
        },
        None => match current_char(tokenizer) {
            '-' => {
                tokenizer.position += 1;
                (Delim('-'), None)
            },
            '\\' => {
                tokenizer.position += 1;
                error_token(Delim('\\'), ~"Invalid escape")
            },
            _ => fail!(),  // Should not have called consume_ident() here.
        }
    }
}

fn consume_ident_string(tokenizer: &mut Tokenizer) -> Option<~str> {
    match current_char(tokenizer) {
        '-' => if !next_is_namestart_or_escape(tokenizer) { None }
               else { Some(consume_ident_string_rest(tokenizer)) },
        '\\' if is_invalid_escape(tokenizer) => return None,
        _ if !is_namestart_or_escape(tokenizer) => return None,
        _ => Some(consume_ident_string_rest(tokenizer))
    }
}


// 4.4.10. Ident-rest state
fn consume_ident_string_rest(tokenizer: &mut Tokenizer) -> ~str {
    let mut string = ~"";
    while !is_eof(tokenizer) {
        let c = current_char(tokenizer);
        let next_char = match c {
            'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '-'  => {
                tokenizer.position += 1; c },
            _ if c >= '\x80' => consume_char(tokenizer),  // Non-ASCII
            '\\' => {
                if is_invalid_escape(tokenizer) { break }
                tokenizer.position += 1;
                consume_escape(tokenizer)
            },
            _ => break
        };
        push_char!(string, next_char)
    }
    string
}


// 4.4.12. Number state
fn consume_numeric(tokenizer: &mut Tokenizer) -> (Token, Option<ParseError>) {
    let c = consume_char(tokenizer);
    match c {
        '-' | '+' => consume_numeric_sign(tokenizer, c),
        '.' => {
            if is_eof(tokenizer) { return (Delim('.'), None) }
            match current_char(tokenizer) {
                '0'..'9' => consume_numeric_fraction(tokenizer, ~"."),
                _ => (Delim('.'), None),
            }
        },
        '0'..'9' => consume_numeric_rest(tokenizer, c),
        _ => fail!(),  // consume_numeric() should not have been called here.
    }
}


fn consume_numeric_sign(tokenizer: &mut Tokenizer, sign: char)
        -> (Token, Option<ParseError>) {
    if is_eof(tokenizer) { return (Delim(sign), None) }
    match current_char(tokenizer) {
        '.' => {
            tokenizer.position += 1;
            if !is_eof(tokenizer)
                    && is_match!(current_char(tokenizer), '0'..'9') {
                consume_numeric_fraction(
                    tokenizer, str::from_char(sign) + ~".")
            } else {
                tokenizer.position -= 1;
                (Delim(sign), None)
            }
        },
        '0'..'9' => consume_numeric_rest(tokenizer, sign),
        _ => (Delim(sign), None)
    }
}


// 4.4.13. Number-rest state
fn consume_numeric_rest(tokenizer: &mut Tokenizer, initial_char: char)
        -> (Token, Option<ParseError>) {
    let mut string = str::from_char(initial_char);
    while !is_eof(tokenizer) {
        let c = current_char(tokenizer);
        match c {
            '0'..'9' => { push_char!(string, c); tokenizer.position += 1 },
            '.' => {
                tokenizer.position += 1;
                if !is_eof(tokenizer)
                        && is_match!(current_char(tokenizer), '0'..'9') {
                    push_char!(string, '.');
                    return consume_numeric_fraction(tokenizer, string);
                } else {
                    tokenizer.position -= 1; break
                }
            },
            _ => match consume_scientific_number(tokenizer, string) {
                Ok(token) => return (token, None),
                Err(s) => { string = s; break }
            }
        }
    }
    let value = Integer((
        // Remove any + sign as int::from_str() does not parse them.
        if string[0] != '+' as u8 { int::from_str(string) }
        else { int::from_str(str::slice(string, 1, string.len())) }
    ).get());  // XXX handle overflow
    consume_numeric_end(tokenizer, string, value)
}


// 4.4.14. Number-fraction state
fn consume_numeric_fraction(tokenizer: &mut Tokenizer, string: ~str)
        -> (Token, Option<ParseError>) {
    let mut string: ~str = string;
    while !is_eof(tokenizer) {
        match current_char(tokenizer) {
            '0'..'9' => push_char!(string, consume_char(tokenizer)),
            _ => match consume_scientific_number(tokenizer, string) {
                Ok(token) => return (token, None),
                Err(s) => { string = s; break }
            }
        }
    }
    let value = Float(float::from_str(string).get());  // XXX handle overflow
    consume_numeric_end(tokenizer, string, value)
}


fn consume_numeric_end(tokenizer: &mut Tokenizer, string: ~str,
                       value: NumericValue) -> (Token, Option<ParseError>) {
    if is_eof(tokenizer) { return (Number(value, string), None) }
    (match current_char(tokenizer) {
        '%' => { tokenizer.position += 1; Percentage(value, string) },
        _ => {
            // 4.4.15. Dimension state (kind of)
            match consume_ident_string(tokenizer) {
                Some(unit) => Dimension(value, string, unit),
                None => Number(value, string),
            }
        },
    }, None)
}


fn consume_scientific_number(tokenizer: &mut Tokenizer, string: ~str)
        -> Result<Token, ~str> {
    let next_3 = next_n_chars(tokenizer, 3);
    let mut string: ~str = string;
    if (next_3.len() >= 2
        && (next_3[0] == 'e' || next_3[0] == 'E')
        && (is_match!(next_3[1], '0'..'9'))
    ) {
        push_char!(string, next_3[0]);
        push_char!(string, next_3[1]);
        tokenizer.position += 2;
    } else if (
        next_3.len() == 3
        && (next_3[0] == 'e' || next_3[0] == 'E')
        && (next_3[1] == '+' || next_3[1] == '-')
        && is_match!(next_3[2], '0'..'9')
    ) {
        push_char!(string, next_3[0]);
        push_char!(string, next_3[1]);
        push_char!(string, next_3[2]);
        tokenizer.position += 3;
    } else {
        return Err(string)
    }
    // 4.4.16. Sci-notation state
    while !is_eof(tokenizer) && is_match!(current_char(tokenizer), '0'..'9') {
        push_char!(string, consume_char(tokenizer))
    }
    let value = Float(float::from_str(string).get());
    Ok(Number(value, string))
}


// 4.4.17. URL state
fn consume_url(tokenizer: &mut Tokenizer) -> (Token, Option<ParseError>) {
    while !is_eof(tokenizer) {
        match current_char(tokenizer) {
            '\t' | '\n' | '\x0C' | ' ' => tokenizer.position += 1,
            '"' => return consume_quoted_url(tokenizer, false),
            '\'' => return consume_quoted_url(tokenizer, true),
            ')' => { tokenizer.position += 1; return (URL(~""), None) },
            _ => return consume_unquoted_url(tokenizer),
        }
    }
    error_token(BadURL, ~"EOF in URL")
}


// 4.4.18. URL-double-quote state
// 4.4.19. URL-single-quote state
fn consume_quoted_url(tokenizer: &mut Tokenizer, single_quote: bool)
        -> (Token, Option<ParseError>) {
    tokenizer.position += 1;  // The initial quote
    let (token, err) = consume_quoted_string(tokenizer, single_quote);
    match err {
        Some(_) => {
            let (token, _) = consume_bad_url(tokenizer);
            (token, err)
        },
        None => match token {
            String(string) => consume_url_end(tokenizer, string),
            // consume_quoted_string() never returns a non-String token
            // without error:
            _ => fail!(),
        }
    }
}


// 4.4.20. URL-end state
fn consume_url_end(tokenizer: &mut Tokenizer, string: ~str)
        -> (Token, Option<ParseError>) {
    while !is_eof(tokenizer) {
        match consume_char(tokenizer) {
            '\t' | '\n' | '\x0C' | ' ' => (),
            ')' => return (URL(string), None),
            _ => return consume_bad_url(tokenizer)
        }
    }
    error_token(BadURL, ~"EOF in URL")
}


// 4.4.21. URL-unquoted state
fn consume_unquoted_url(tokenizer: &mut Tokenizer) -> (Token, Option<ParseError>) {
    let mut string = ~"";
    while !is_eof(tokenizer) {
        let next_char = match consume_char(tokenizer) {
            '\t' | '\n' | '\x0C' | ' '
                => return consume_url_end(tokenizer, string),
            ')' => return (URL(string), None),
            '\x00'..'\x08' | '\x0E'..'\x1F' | '\x7F'..'\x9F'  // non-printable
                | '"' | '\'' | '(' => return consume_bad_url(tokenizer),
            '\\' => match next_n_chars(tokenizer, 1) {
                ['\n'] | ['\x0C'] | [] => return consume_bad_url(tokenizer),
                _ => consume_escape(tokenizer)
            },
            c => c
        };
        push_char!(string, next_char)
    }
    error_token(BadURL, ~"EOF in URL")
}


// 4.4.22. Bad-URL state
fn consume_bad_url(tokenizer: &mut Tokenizer) -> (Token, Option<ParseError>) {
    // Consume up to the closing )
    while !is_eof(tokenizer) {
        match consume_char(tokenizer) {
            ')' => break,
            '\\' => tokenizer.position += 1, // Skip an escaped ) or \
            _ => ()
        }
    }
    error_token(BadURL, ~"Invalid URL syntax")
}


// 4.4.23. Unicode-range state
fn consume_unicode_range(tokenizer: &mut Tokenizer)
        -> (Token, Option<ParseError>) {
    let next_3 = next_n_chars(tokenizer, 3);
    // We got here with U or u
    assert!(next_3[0] == 'u' || next_3[0] == 'U');
    // Check if this is indeed an unicode range. Fallback on ident.
    if next_3.len() == 3 && next_3[1] == '+' {
        match next_3[2] {
            '0'..'9' | 'a'..'f' | 'A'..'F' => tokenizer.position += 2,
            _ => { return consume_ident(tokenizer) }
        }
    } else { return consume_ident(tokenizer) }

    let mut hex = ~[];
    while hex.len() < 6 && !is_eof(tokenizer) {
        let c = current_char(tokenizer);
        match c {
            '0'..'9' | 'A'..'F' | 'a'..'f' => {
                hex.push(c); tokenizer.position += 1 },
            _ => break
        }
    }
    assert!(hex.len() > 0);
    let max_question_marks = 6u - hex.len();
    let mut question_marks = 0u;
    while question_marks < max_question_marks && !is_eof(tokenizer)
            && current_char(tokenizer) == '?' {
        question_marks += 1;
        tokenizer.position += 1
    }
    let start: char, end: char;
    if question_marks > 0 {
        start = char_from_hex(hex + vec::from_elem(question_marks, '0'));
        end = char_from_hex(hex + vec::from_elem(question_marks, 'F'));
    } else {
        start = char_from_hex(hex);
        hex = ~[];
        if !is_eof(tokenizer) && current_char(tokenizer) == '-' {
            tokenizer.position += 1;
            while hex.len() < 6 && !is_eof(tokenizer) {
                let c = current_char(tokenizer);
                match c {
                    '0'..'9' | 'A'..'F' | 'a'..'f' => {
                        hex.push(c); tokenizer.position += 1 },
                    _ => break
                }
            }
        }
        end = if hex.len() > 0 { char_from_hex(hex) } else { start }
    }
    // 4.6. Set the unicode-range token's range
    (if start > MAX_UNICODE || end < start {
        EmptyUnicodeRange
    } else {
        let end = if end <= MAX_UNICODE { end } else { MAX_UNICODE };
        UnicodeRange(start, end)
    }, None)
}


// 4.5. Consume an escaped character
// Assumes that the U+005C REVERSE SOLIDUS (\) has already been consumed
// and that the next input character has already been verified
// to not be a newline or EOF.
fn consume_escape(tokenizer: &mut Tokenizer) -> char {
    let c = consume_char(tokenizer);
    match c {
        '0'..'9' | 'A'..'F' | 'a'..'f' => {
            let mut hex = ~[c];
            while hex.len() < 6 && !is_eof(tokenizer) {
                let c = current_char(tokenizer);
                match c {
                    '0'..'9' | 'A'..'F' | 'a'..'f' => {
                        hex.push(c); tokenizer.position += 1 },
                    _ => break
                }
            }
            if !is_eof(tokenizer) {
                match current_char(tokenizer) {
                    '\t' | '\n' | '\x0C' | ' ' => tokenizer.position += 1,
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
        let mut tokenizer = Tokenizer::from_str(input);
        let mut tokens: ~[Token] = ~[];
        let mut errors: ~[~str] = ~[];
        loop {
            let (token, err) = tokenizer.next_token();
            match err {
                Some(ParseError{message: message}) => errors.push(message),
                None => (),
            }
            match token {
                EOF => break,
                token => tokens.push(token),
            }
        }
        check_results(tokens, expected_tokens, errors, expected_errors)
    }

    assert_tokens("", [], []);
    assert_tokens("?/", [Delim('?'), Delim('/')], []);
    assert_tokens("?/* Li/*psum… */", [Delim('?')], []);
    assert_tokens("?/* Li/*psum… *//", [Delim('?'), Delim('/')], []);
    assert_tokens("?/* Lipsum", [Delim('?')], [~"Unclosed comment"]);
    assert_tokens("?/*", [Delim('?')], [~"Unclosed comment"]);
    assert_tokens("?/*/", [Delim('?')], [~"Unclosed comment"]);
    assert_tokens("?/**/!", [Delim('?'), Delim('!')], []);
    assert_tokens("?/**/", [Delim('?')], []);
    assert_tokens("[?}{)", [
        OpenSquareBraket, Delim('?'), CloseCurlyBraket,
        OpenCurlyBraket, CloseParenthesis
    ], []);

    assert_tokens("(\n \t'Lore\\6d \"ipsu\\6D'",
        [OpenParenthesis, WhiteSpace, String(~"Lorem\"ipsum")], []);
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
    assert_tokens("-L\\ïpsum", [Ident(~"-Lïpsum")], []);
    assert_tokens("-\\Lipsum", [Ident(~"-Lipsum")], []);
    assert_tokens("-", [Delim('-')], []);
    assert_tokens("--Lipsum", [Delim('-'), Ident(~"-Lipsum")], []);
    assert_tokens("-\\-Lipsum", [Ident(~"--Lipsum")], []);
    assert_tokens("\\Lipsum", [Ident(~"Lipsum")], []);
    assert_tokens("\\\nLipsum", [Delim('\\'), WhiteSpace, Ident(~"Lipsum")],
        [~"Invalid escape"]);
    assert_tokens("\\", [Delim('\\')], [~"Invalid escape"]);
    assert_tokens("\x7f\x80\x81", [Delim('\x7F'), Ident(~"\x80\x81")], []);

    assert_tokens("func()", [Function(~"func"), CloseParenthesis], []);
    assert_tokens("func ()",
        [Ident(~"func"), WhiteSpace, OpenParenthesis, CloseParenthesis], []);

    assert_tokens("##00(#\\##\\\n#\\",
        [Delim('#'), Hash(~"00"), OpenParenthesis, Hash(~"#"), Delim('#'),
         Delim('\\'), WhiteSpace, Delim('#'), Delim('\\')],
        [~"Invalid escape", ~"Invalid escape"]);

    assert_tokens("@@page(@\\x@-x@-\\x@--@\\\n@\\", [
        Delim('@'), AtKeyword(~"page"), OpenParenthesis, AtKeyword(~"x"),
        AtKeyword(~"-x"), AtKeyword(~"-x"), Delim('@'), Delim('-'), Delim('-'),
        Delim('@'), Delim('\\'), WhiteSpace, Delim('@'), Delim('\\')
    ], [~"Invalid escape", ~"Invalid escape"]);

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
        [URL(~""), URL(~""), URL(~""), Function(~"Ürl"), CloseParenthesis],
        []);
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

    assert_tokens("42+42-42. 1.5+1.5-1.5.5+.5-.5+-.", [
        Number(Integer(42), ~"42"),
        Number(Integer(42), ~"+42"),
        Number(Integer(-42), ~"-42"), Delim('.'), WhiteSpace,
        Number(Float(1.5), ~"1.5"),
        Number(Float(1.5), ~"+1.5"),
        Number(Float(-1.5), ~"-1.5"),
        Number(Float(0.5), ~".5"),
        Number(Float(0.5), ~"+.5"),
        Number(Float(-0.5), ~"-.5"), Delim('+'), Delim('-'), Delim('.')
    ], []);
    assert_tokens("42e2px 42e+2 42e-2.", [
        Number(Float(4200.), ~"42e2"), Ident(~"px"), WhiteSpace,
        Number(Float(4200.), ~"42e+2"), WhiteSpace,
        Number(Float(0.42), ~"42e-2"), Delim('.')
    ], []);
    assert_tokens("42%+.5%-1%", [
        Percentage(Integer(42), ~"42"),
        Percentage(Float(0.5), ~"+.5"),
        Percentage(Integer(-1), ~"-1"),
    ], []);
    assert_tokens("42-Px+.5\\u -1url(7-0\\", [
        Dimension(Integer(42), ~"42", ~"-Px"),
        Dimension(Float(0.5), ~"+.5", ~"u"), WhiteSpace,
        Dimension(Integer(-1), ~"-1", ~"url"), OpenParenthesis,
        Number(Integer(7), ~"7"),
        Number(Integer(0), ~"-0"), Delim('\\'),
    ], [~"Invalid escape"]);
}
