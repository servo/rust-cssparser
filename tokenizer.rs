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
use super::ast::NumericValue;


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
    AtKeyword(~str),
    Hash(~str),
    String(~str),
    BadString,
    URL(~str),
    BadURL,
    Delim(char),
    Number(NumericValue),
    Percentage(NumericValue),
    Dimension(NumericValue, ~str),
    UnicodeRange {start: char, end: char},
    EmptyUnicodeRange,
    WhiteSpace,
    CDO,  // <!--
    CDC,  // -->
    Colon,  // :
    Semicolon,  // ;
    CloseParenthesis, // )
    CloseSquareBraket, // ]
    CloseCurlyBraket, // }

    // Non-preserved tokens
    Function(~str),
    OpenParenthesis, // (
    OpenSquareBraket, // [
    OpenCurlyBraket, // {
    EOF,
}


//  ***********  End of public API  ***********


static MAX_UNICODE: char = '\U0010FFFF';


fn preprocess(input: &str) -> ~str {
    // TODO: Is this faster if done in one pass?
    str::replace(str::replace(str::replace(str::replace(input,
    "\r\n", "\n"),
    "\r", "\n"),
    "\x0C", "\n"),
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
        ['\\', '\n'] | ['\\'] => true,
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
                '\t' | '\n' | ' ' => {
                    while !is_eof(tokenizer) {
                        match current_char(tokenizer) {
                            '\t' | '\n' | ' '
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


fn consume_quoted_string(tokenizer: &mut Tokenizer, single_quote: bool)
        -> (Token, Option<ParseError>) {
    let mut string: ~str = ~"";
    while !is_eof(tokenizer) {
        match consume_char(tokenizer) {
            '"' if !single_quote => return (String(string), None),
            '\'' if single_quote => return (String(string), None),
            '\n' => {
                return error_token(BadString, ~"Newline in quoted string");
            },
            '\\' => {
                match next_n_chars(tokenizer, 1) {
                    // Quoted newline
                    ['\n'] => tokenizer.position += 1,
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


fn consume_hash(tokenizer: &mut Tokenizer) -> (Token, Option<ParseError>) {
    let string = consume_ident_string_rest(tokenizer);
    (if string == ~"" { Delim('#') } else { Hash(string) }, None)
}


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


fn consume_at_keyword(tokenizer: &mut Tokenizer) -> (Token, Option<ParseError>) {
    (match consume_ident_string(tokenizer) {
        Some(string) => AtKeyword(string),
        None => Delim('@')
    }, None)
}


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
    consume_numeric_end(tokenizer, NumericValue::new(string, true))
}


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
    consume_numeric_end(tokenizer, NumericValue::new(string, false))
}


fn consume_numeric_end(tokenizer: &mut Tokenizer, value: NumericValue)
        -> (Token, Option<ParseError>) {
    if is_eof(tokenizer) { return (Number(value), None) }
    (match current_char(tokenizer) {
        '%' => { tokenizer.position += 1; Percentage(value) },
        _ => {
            match consume_ident_string(tokenizer) {
                Some(unit) => Dimension(value, unit),
                None => Number(value),
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
    while !is_eof(tokenizer) && is_match!(current_char(tokenizer), '0'..'9') {
        push_char!(string, consume_char(tokenizer))
    }
    Ok(Number(NumericValue::new(string, false)))
}


fn consume_url(tokenizer: &mut Tokenizer) -> (Token, Option<ParseError>) {
    while !is_eof(tokenizer) {
        match current_char(tokenizer) {
            '\t' | '\n' | ' ' => tokenizer.position += 1,
            '"' => return consume_quoted_url(tokenizer, false),
            '\'' => return consume_quoted_url(tokenizer, true),
            ')' => { tokenizer.position += 1; return (URL(~""), None) },
            _ => return consume_unquoted_url(tokenizer),
        }
    }
    error_token(BadURL, ~"EOF in URL")
}


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


fn consume_url_end(tokenizer: &mut Tokenizer, string: ~str)
        -> (Token, Option<ParseError>) {
    while !is_eof(tokenizer) {
        match consume_char(tokenizer) {
            '\t' | '\n' | ' ' => (),
            ')' => return (URL(string), None),
            _ => return consume_bad_url(tokenizer)
        }
    }
    error_token(BadURL, ~"EOF in URL")
}


fn consume_unquoted_url(tokenizer: &mut Tokenizer) -> (Token, Option<ParseError>) {
    let mut string = ~"";
    while !is_eof(tokenizer) {
        let next_char = match consume_char(tokenizer) {
            '\t' | '\n' | ' '
                => return consume_url_end(tokenizer, string),
            ')' => return (URL(string), None),
            '\x00'..'\x08' | '\x0E'..'\x1F' | '\x7F'..'\x9F'  // non-printable
                | '"' | '\'' | '(' => return consume_bad_url(tokenizer),
            '\\' => match next_n_chars(tokenizer, 1) {
                ['\n'] | [] => return consume_bad_url(tokenizer),
                _ => consume_escape(tokenizer)
            },
            c => c
        };
        push_char!(string, next_char)
    }
    error_token(BadURL, ~"EOF in URL")
}


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
    (if start > MAX_UNICODE || end < start {
        EmptyUnicodeRange
    } else {
        let end = if end <= MAX_UNICODE { end } else { MAX_UNICODE };
        UnicodeRange {start: start, end: end}
    }, None)
}


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
                    '\t' | '\n' | ' ' => tokenizer.position += 1,
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
