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

use std::{str, u32, vec};
use extra::json;
use super::utils::*;
use super::ast::*;


pub struct SyntaxError {
    message: ~str,
//    source_location: ~str,
    source_line: uint,
    source_column: uint,
}


pub struct Parser {
    priv input: ~str,
    priv length: uint,  // All counted in bytes, not characters
    priv position: uint,  // All counted in bytes, not characters
    priv line: uint,
    priv column: uint,  // All counted in bytes, not characters
    priv errors: ~[SyntaxError],
}


impl Parser {
    pub fn from_str(input: &str) -> ~Parser {
        let input = preprocess(input);
        ~Parser {
            length: input.len(),
            input: input,
            position: 0,
            line: 1,
            column: 1,
            errors: ~[],
        }
    }
}


pub fn iter_component_values(parser: &mut Parser, it: &fn (v: ComponentValue) -> bool) -> bool {
    loop {
        match consume_component_value(parser) {
            Some(component_value) => if !it(component_value) { return false },
            None => return true,
        }
    }
}


pub fn consume_component_value(parser: &mut Parser) -> Option<ComponentValue> {
    consume_comments(parser);
    if parser.is_eof() { return None }
    let c = parser.current_char();
    Some(match c {
        '-' => {
            if parser.starts_with(~"-->") {
                parser.position += 3;
                CDC
            }
            else if next_is_namestart_or_escape(parser) {
                consume_ident(parser)
            } else {
                consume_numeric(parser)
            }
        },
        '<' => {
            if parser.starts_with(~"<!--") {
                parser.position += 4;
                CDO
            } else {
                parser.position += 1;
                Delim('<')
            }
        },
        '0'..'9' | '.' | '+' => consume_numeric(parser),
        'u' | 'U' => consume_unicode_range(parser),
        'a'..'z' | 'A'..'Z' | '_' | '\\' => consume_ident(parser),
        _ if c >= '\x80' => consume_ident(parser),  // Non-ASCII
        _ => {
            match parser.consume_char() {
                '\t' | '\n' | ' ' => {
                    while !parser.is_eof() {
                        match parser.current_char() {
                            '\t' | '\n' | ' '
                                => parser.position += 1,
                            _ => break,
                        }
                    }
                    WhiteSpace
                },
                '"' => consume_quoted_string(parser, false),
                '#' => consume_hash(parser),
                '\'' => consume_quoted_string(parser, true),
                '(' => ParenthesisBlock(consume_block(parser, CloseParenthesis)),
                ')' => CloseParenthesis,
                ':' => Colon,
                ';' => Semicolon,
                '@' => consume_at_keyword(parser),
                '[' => SquareBraketBlock(consume_block(parser, CloseSquareBraket)),
                ']' => CloseSquareBraket,
                '{' => CurlyBraketBlock(consume_block(parser, CloseCurlyBraket)),
                '}' => CloseCurlyBraket,
                _ => Delim(c)
            }
        }
    })
}


//  ***********  End of public API  ***********


#[inline]
fn preprocess(input: &str) -> ~str {
    // TODO: Is this faster if done in one pass?
    input.replace("\r\n", "\n").replace("\r", "\n").replace("\x0C", "\n").replace("\x00", "\uFFFD")
}


#[test]
fn test_preprocess() {
    assert!(preprocess("") == ~"");
    assert!(preprocess("Lorem\r\n\t\x00ipusm\ndoror\uFFFD\r")
            == ~"Lorem\n\t\uFFFDipusm\ndoror\uFFFD\n");
}


impl Parser {
    fn error(&mut self, message: ~str) {
        self.errors.push(SyntaxError{
            message: message, source_line: self.line, source_column: self.column })
    }

    #[inline]
    fn is_eof(&self) -> bool { self.position >= self.length }

    // Assumes non-EOF
    #[inline]
    fn current_char(&self) -> char { self.input.char_at(self.position) }

    #[inline]
    fn consume_char(&mut self) -> char {
        let range = self.input.char_range_at(self.position);
        self.position = range.next;
        range.ch
    }

    // Return value may be smaller than n if we’re near the end of the input.
    #[inline]
    fn next_n_chars(&mut self, n: uint) -> ~[char] {
        let mut chars: ~[char] = ~[];
        let mut position = self.position;
        for n.times {
            if position >= self.length { break }
            let range = self.input.char_range_at(position);
            position = range.next;
            chars.push(range.ch);
        }
        chars
    }

    #[inline]
    fn starts_with(&self, needle: ~str) -> bool {
        // XXX Duplicate str::match_at which is not public.
        let mut i = self.position;
        if i + needle.len() > self.length { return false }
        let haystack: &str = self.input;
        for needle.bytes_iter().advance |c| { if haystack[i] != c { return false; } i += 1u; }
        return true;
    }
}


#[inline]
fn consume_comments(parser: &mut Parser) {
    while parser.starts_with(~"/*") {
        parser.position += 2;  // +2 to consume "/*"
        match parser.input.slice_from(parser.position).find_str("*/") {
            // +2 to consume "*/"
            Some(offset) => parser.position += offset + 2,
            None => parser.position = parser.length  // EOF
        }
    }
}


fn consume_block(parser: &mut Parser, ending_token: ComponentValue) -> ~[ComponentValue] {
    let mut content = ~[];
    for iter_component_values(parser) |component_value| {
        if component_value == ending_token { break }
        content.push(component_value)
    }
    content
}


macro_rules! is_match(
    ($value:expr, $pattern:pat) => (
        match $value { $pattern => true, _ => false }
    );
)


#[inline]
fn is_invalid_escape(parser: &mut Parser) -> bool {
    match parser.next_n_chars(2) {
        ['\\', '\n'] | ['\\'] => true,
        _ => false,
    }
}


#[inline]
fn is_namestart_or_escape(parser: &mut Parser) -> bool {
    match parser.current_char() {
        'a'..'z' | 'A'..'Z' | '_' => true,
        '\\' => !is_invalid_escape(parser),
        c => c >= '\x80',  // Non-ASCII
    }
}


#[inline]
fn next_is_namestart_or_escape(parser: &mut Parser) -> bool {
    parser.position += 1;
    let result = !parser.is_eof() && is_namestart_or_escape(parser);
    parser.position -= 1;
    result
}


fn consume_quoted_string(parser: &mut Parser, single_quote: bool) -> ComponentValue {
    let mut string: ~str = ~"";
    while !parser.is_eof() {
        match parser.consume_char() {
            '"' if !single_quote => break,
            '\'' if single_quote => break,
            '\n' => {
                parser.error(~"Newline in quoted string");
                return BadString;
            },
            '\\' => {
                match parser.next_n_chars(1) {
                    // Quoted newline
                    ['\n'] => parser.position += 1,
                    [] => {
                        parser.error(~"Escaped EOF");
                        return BadString
                    },
                    _ => string.push_char(consume_escape(parser))
                }
            }
            c => string.push_char(c),
        }
    }
    String(string)
}


fn consume_hash(parser: &mut Parser) -> ComponentValue {
    let string = consume_ident_string_rest(parser);
    if string == ~"" { Delim('#') } else { Hash(string) }
}


fn consume_at_keyword(parser: &mut Parser) -> ComponentValue {
    match consume_ident_string(parser) {
        Some(string) => AtKeyword(string),
        None => Delim('@')
    }
}


fn consume_ident(parser: &mut Parser) -> ComponentValue {
    match consume_ident_string(parser) {
        Some(string) => {
            if parser.is_eof() { return Ident(string) }
            match parser.current_char() {
                '(' => {
                    parser.position += 1;
                    if ascii_lower(string) == ~"url" { consume_url(parser) }
                    else { Function(string, consume_block(parser, CloseParenthesis)) }
                },
                _ => Ident(string)
            }
        },
        None => match parser.current_char() {
            '-' => {
                parser.position += 1;
                Delim('-')
            },
            '\\' => {
                parser.position += 1;
                parser.error(~"Invalid escape");
                Delim('\\')
            },
            _ => fail!(),  // Should not have called consume_ident() here.
        }
    }
}

fn consume_ident_string(parser: &mut Parser) -> Option<~str> {
    match parser.current_char() {
        '-' => if !next_is_namestart_or_escape(parser) { None }
               else { Some(consume_ident_string_rest(parser)) },
        '\\' if is_invalid_escape(parser) => return None,
        _ if !is_namestart_or_escape(parser) => return None,
        _ => Some(consume_ident_string_rest(parser))
    }
}


fn consume_ident_string_rest(parser: &mut Parser) -> ~str {
    let mut string = ~"";
    while !parser.is_eof() {
        let c = parser.current_char();
        let next_char = match c {
            'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '-'  => {
                parser.position += 1; c },
            _ if c >= '\x80' => parser.consume_char(),  // Non-ASCII
            '\\' => {
                if is_invalid_escape(parser) { break }
                parser.position += 1;
                consume_escape(parser)
            },
            _ => break
        };
        string.push_char(next_char)
    }
    string
}


fn consume_numeric(parser: &mut Parser) -> ComponentValue {
    let c = parser.consume_char();
    match c {
        '-' | '+' => consume_numeric_sign(parser, c),
        '.' => {
            if parser.is_eof() { return Delim('.') }
            match parser.current_char() {
                '0'..'9' => consume_numeric_fraction(parser, ~"."),
                _ => Delim('.'),
            }
        },
        '0'..'9' => consume_numeric_rest(parser, c),
        _ => fail!(),  // consume_numeric() should not have been called here.
    }
}


fn consume_numeric_sign(parser: &mut Parser, sign: char)
        -> ComponentValue {
    if parser.is_eof() { return Delim(sign) }
    match parser.current_char() {
        '.' => {
            parser.position += 1;
            if !parser.is_eof()
                    && is_match!(parser.current_char(), '0'..'9') {
                consume_numeric_fraction(parser, str::from_char(sign) + ".")
            } else {
                parser.position -= 1;
                Delim(sign)
            }
        },
        '0'..'9' => consume_numeric_rest(parser, sign),
        _ => Delim(sign)
    }
}


fn consume_numeric_rest(parser: &mut Parser, initial_char: char)
        -> ComponentValue {
    let mut string = str::from_char(initial_char);
    while !parser.is_eof() {
        let c = parser.current_char();
        match c {
            '0'..'9' => { string.push_char(c); parser.position += 1 },
            '.' => {
                parser.position += 1;
                if !parser.is_eof()
                        && is_match!(parser.current_char(), '0'..'9') {
                    string.push_char('.');
                    return consume_numeric_fraction(parser, string);
                } else {
                    parser.position -= 1; break
                }
            },
            _ => match consume_scientific_number(parser, string) {
                Ok(token) => return token,
                Err(s) => { string = s; break }
            }
        }
    }
    consume_numeric_end(parser, NumericValue::new(string, true))
}


fn consume_numeric_fraction(parser: &mut Parser, string: ~str)
        -> ComponentValue {
    let mut string: ~str = string;
    while !parser.is_eof() {
        match parser.current_char() {
            '0'..'9' => string.push_char(parser.consume_char()),
            _ => match consume_scientific_number(parser, string) {
                Ok(token) => return token,
                Err(s) => { string = s; break }
            }
        }
    }
    consume_numeric_end(parser, NumericValue::new(string, false))
}


fn consume_numeric_end(parser: &mut Parser, value: NumericValue)
        -> ComponentValue {
    if parser.is_eof() { return Number(value) }
    match parser.current_char() {
        '%' => { parser.position += 1; Percentage(value) },
        _ => {
            match consume_ident_string(parser) {
                Some(unit) => Dimension(value, unit),
                None => Number(value),
            }
        },
    }
}


fn consume_scientific_number(parser: &mut Parser, string: ~str)
        -> Result<ComponentValue, ~str> {
    let next_3 = parser.next_n_chars(3);
    let mut string: ~str = string;
    if (next_3.len() >= 2
        && (next_3[0] == 'e' || next_3[0] == 'E')
        && (is_match!(next_3[1], '0'..'9'))
    ) {
        string.push_char(next_3[0]);
        string.push_char(next_3[1]);
        parser.position += 2;
    } else if (
        next_3.len() == 3
        && (next_3[0] == 'e' || next_3[0] == 'E')
        && (next_3[1] == '+' || next_3[1] == '-')
        && is_match!(next_3[2], '0'..'9')
    ) {
        string.push_char(next_3[0]);
        string.push_char(next_3[1]);
        string.push_char(next_3[2]);
        parser.position += 3;
    } else {
        return Err(string)
    }
    while !parser.is_eof() && is_match!(parser.current_char(), '0'..'9') {
        string.push_char(parser.consume_char())
    }
    Ok(Number(NumericValue::new(string, false)))
}


fn consume_url(parser: &mut Parser) -> ComponentValue {
    while !parser.is_eof() {
        match parser.current_char() {
            '\t' | '\n' | ' ' => parser.position += 1,
            '"' => return consume_quoted_url(parser, false),
            '\'' => return consume_quoted_url(parser, true),
            ')' => { parser.position += 1; break },
            _ => return consume_unquoted_url(parser),
        }
    }
    URL(~"")
}


fn consume_quoted_url(parser: &mut Parser, single_quote: bool)
        -> ComponentValue {
    parser.position += 1;  // The initial quote
    match consume_quoted_string(parser, single_quote) {
        String(string) => consume_url_end(parser, string),
        BadString => consume_bad_url(parser),
        // consume_quoted_string() only returns String or BadString
        _ => fail!(),
    }
}


fn consume_url_end(parser: &mut Parser, string: ~str)
        -> ComponentValue {
    while !parser.is_eof() {
        match parser.consume_char() {
            '\t' | '\n' | ' ' => (),
            ')' => break,
            _ => return consume_bad_url(parser)
        }
    }
    URL(string)
}


fn consume_unquoted_url(parser: &mut Parser) -> ComponentValue {
    let mut string = ~"";
    while !parser.is_eof() {
        let next_char = match parser.consume_char() {
            '\t' | '\n' | ' '
                => return consume_url_end(parser, string),
            ')' => break,
            '\x00'..'\x08' | '\x0E'..'\x1F' | '\x7F'..'\x9F'  // non-printable
                | '"' | '\'' | '(' => return consume_bad_url(parser),
            '\\' => match parser.next_n_chars(1) {
                ['\n'] | [] => return consume_bad_url(parser),
                _ => consume_escape(parser)
            },
            c => c
        };
        string.push_char(next_char)
    }
    URL(string)
}


fn consume_bad_url(parser: &mut Parser) -> ComponentValue {
    // Consume up to the closing )
    while !parser.is_eof() {
        match parser.consume_char() {
            ')' => break,
            '\\' => parser.position += 1, // Skip an escaped ) or \
            _ => ()
        }
    }
    parser.error(~"Invalid URL syntax");
    BadURL
}


fn consume_unicode_range(parser: &mut Parser)
        -> ComponentValue {
    let next_3 = parser.next_n_chars(3);
    // We got here with U or u
    assert!(next_3[0] == 'u' || next_3[0] == 'U');
    // Check if this is indeed an unicode range. Fallback on ident.
    if next_3.len() == 3 && next_3[1] == '+' {
        match next_3[2] {
            '0'..'9' | 'a'..'f' | 'A'..'F' => parser.position += 2,
            _ => { return consume_ident(parser) }
        }
    } else { return consume_ident(parser) }

    let mut hex = ~[];
    while hex.len() < 6 && !parser.is_eof() {
        let c = parser.current_char();
        match c {
            '0'..'9' | 'A'..'F' | 'a'..'f' => {
                hex.push(c); parser.position += 1 },
            _ => break
        }
    }
    assert!(hex.len() > 0);
    let max_question_marks = 6u - hex.len();
    let mut question_marks = 0u;
    while question_marks < max_question_marks && !parser.is_eof()
            && parser.current_char() == '?' {
        question_marks += 1;
        parser.position += 1
    }
    let start: char;
    let end: char;
    if question_marks > 0 {
        start = char_from_hex(hex + vec::from_elem(question_marks, '0'));
        end = char_from_hex(hex + vec::from_elem(question_marks, 'F'));
    } else {
        start = char_from_hex(hex);
        hex = ~[];
        if !parser.is_eof() && parser.current_char() == '-' {
            parser.position += 1;
            while hex.len() < 6 && !parser.is_eof() {
                let c = parser.current_char();
                match c {
                    '0'..'9' | 'A'..'F' | 'a'..'f' => {
                        hex.push(c); parser.position += 1 },
                    _ => break
                }
            }
        }
        end = if hex.len() > 0 { char_from_hex(hex) } else { start }
    }
    if start > MAX_UNICODE || end < start {
        EmptyUnicodeRange
    } else {
        let end = if end <= MAX_UNICODE { end } else { MAX_UNICODE };
//        UnicodeRange {start: start, end: end}
        UnicodeRange(start, end)
    }
}


static MAX_UNICODE: char = '\U0010FFFF';


// Assumes that the U+005C REVERSE SOLIDUS (\) has already been consumed
// and that the next input character has already been verified
// to not be a newline or EOF.
fn consume_escape(parser: &mut Parser) -> char {
    let c = parser.consume_char();
    match c {
        '0'..'9' | 'A'..'F' | 'a'..'f' => {
            let mut hex = ~[c];
            while hex.len() < 6 && !parser.is_eof() {
                let c = parser.current_char();
                match c {
                    '0'..'9' | 'A'..'F' | 'a'..'f' => {
                        hex.push(c); parser.position += 1 },
                    _ => break
                }
            }
            if !parser.is_eof() {
                match parser.current_char() {
                    '\t' | '\n' | ' ' => parser.position += 1,
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


#[inline]
fn char_from_hex(hex: &[char]) -> char {
    u32::from_str_radix(str::from_chars(hex), 16).get() as char
}


#[test]
fn test_component_values() {
    fn assert_component_values(input: &str, expected_component_values: &[ComponentValue],
                               expected_errors: &[~str]) {
        let mut parser = Parser::from_str(input);
        let mut result = ~[];
        for iter_component_values(parser) |v| { result.push(v) }
        let errors = vec::map_consume(match parser { ~Parser {errors: e, _} => e },
                                      |SyntaxError {message: m, _}| m);
        check_results(input, result, expected_component_values,
                      errors, expected_errors);
    }


    assert_component_values("", [], []);
    assert_component_values("?/", [Delim('?'), Delim('/')], []);
    assert_component_values("?/* Li/*psum… */", [Delim('?')], []);
    assert_component_values("?/* Li/*psum… *//", [Delim('?'), Delim('/')], []);
    assert_component_values("?/* Lipsum", [Delim('?')], []);
    assert_component_values("?/*", [Delim('?')], []);
    assert_component_values("?/*/", [Delim('?')], []);
    assert_component_values("?/**/!", [Delim('?'), Delim('!')], []);
    assert_component_values("?/**/", [Delim('?')], []);
    assert_component_values("[?}{)", [
        SquareBraketBlock(~[Delim('?'), CloseCurlyBraket,
            CurlyBraketBlock(~[CloseParenthesis])])
    ], []);

    assert_component_values("(\n \t'Lore\\6d \"ipsu\\6D'",
        [ParenthesisBlock(~[WhiteSpace, String(~"Lorem\"ipsum")])], []);
    assert_component_values("'\\''", [String(~"'")], []);
    assert_component_values("\"\\\"\"", [String(~"\"")], []);
    assert_component_values("\"\\\"", [String(~"\"")], []);
    assert_component_values("'\\", [BadString], [~"Escaped EOF"]);
    assert_component_values("\"0\\0000000\"", [String(~"0\uFFFD0")], []);
    assert_component_values("\"0\\000000 0\"", [String(~"0\uFFFD0")], []);
    assert_component_values("'z\n'a", [BadString, String(~"a")],
        [~"Newline in quoted string"]);

    assert_component_values("Lorem\\ ipsu\\6D dolor \\sit",
        [Ident(~"Lorem ipsumdolor"), WhiteSpace, Ident(~"sit")], []);
    assert_component_values("foo\\", [Ident(~"foo"), Delim('\\')], [~"Invalid escape"]);
    assert_component_values("foo\\\nbar",
        [Ident(~"foo"), Delim('\\'), WhiteSpace, Ident(~"bar")],
        [~"Invalid escape"]);
    assert_component_values("-Lipsum", [Ident(~"-Lipsum")], []);
    assert_component_values("-L\\ïpsum", [Ident(~"-Lïpsum")], []);
    assert_component_values("-\\Lipsum", [Ident(~"-Lipsum")], []);
    assert_component_values("-", [Delim('-')], []);
    assert_component_values("--Lipsum", [Delim('-'), Ident(~"-Lipsum")], []);
    assert_component_values("-\\-Lipsum", [Ident(~"--Lipsum")], []);
    assert_component_values("\\Lipsum", [Ident(~"Lipsum")], []);
    assert_component_values("\\\nLipsum", [Delim('\\'), WhiteSpace, Ident(~"Lipsum")],
        [~"Invalid escape"]);
    assert_component_values("\\", [Delim('\\')], [~"Invalid escape"]);
    assert_component_values("\x7f\x80\x81", [Delim('\x7F'), Ident(~"\x80\x81")], []);

    assert_component_values("func()", [Function(~"func", ~[])], []);
    assert_component_values("func ()",
        [Ident(~"func"), WhiteSpace, ParenthesisBlock(~[])], []);

    assert_component_values("##00(#\\##\\\n#\\",
        [Delim('#'), Hash(~"00"), ParenthesisBlock(~[
            Hash(~"#"), Delim('#'),
            Delim('\\'), WhiteSpace, Delim('#'), Delim('\\')])],
        [~"Invalid escape", ~"Invalid escape"]);

    assert_component_values("@@page(@\\x@-x@-\\x@--@\\\n@\\", [
        Delim('@'), AtKeyword(~"page"), ParenthesisBlock(~[
            AtKeyword(~"x"), AtKeyword(~"-x"), AtKeyword(~"-x"),
            Delim('@'), Delim('-'), Delim('-'),
            Delim('@'), Delim('\\'), WhiteSpace, Delim('@'), Delim('\\')])
    ], [~"Invalid escape", ~"Invalid escape"]);

    assert_component_values("<!-<!-----><",
        [Delim('<'), Delim('!'), Delim('-'), CDO, Delim('-'), CDC, Delim('<')],
        []);
    assert_component_values("u+g u+fU+4?U+030-000039f U+FFFFF?U+42-42U+42-41U+42-110000",
        [Ident(~"u"), Delim('+'), Ident(~"g"), WhiteSpace,
         UnicodeRange('\x0F', '\x0F'), UnicodeRange('\x40', '\x4F'),
         UnicodeRange('0', '9'), Ident(~"f"), WhiteSpace, EmptyUnicodeRange,
         UnicodeRange('B', 'B'), EmptyUnicodeRange,
         UnicodeRange('B', '\U0010FFFF')],
//         UnicodeRange {start: '\x0F', end: '\x0F'}, UnicodeRange {start: '\x40', end: '\x4F'},
//         UnicodeRange {start: '0', end: '9'}, Ident(~"f"), WhiteSpace, EmptyUnicodeRange,
//         UnicodeRange {start: 'B', end: 'B'}, EmptyUnicodeRange,
//         UnicodeRange {start: 'B', end: '\U0010FFFF'}],
        []);

    assert_component_values("url()URL()uRl()Ürl()",
        [URL(~""), URL(~""), URL(~""), Function(~"Ürl", ~[])],
        []);
    assert_component_values("url(  )url(\ta\n)url(\t'a'\n)url(\t'a'z)url(  ",
        [URL(~""), URL(~"a"), URL(~"a"), BadURL, URL(~"")],
        [~"Invalid URL syntax"]);
    assert_component_values("url('a\nb')url('a", [BadURL, URL(~"a")],
        [~"Newline in quoted string", ~"Invalid URL syntax"]);
    assert_component_values("url(a'b)url(\x08z)url('a'", [BadURL, BadURL, URL(~"a")],
        [~"Invalid URL syntax", ~"Invalid URL syntax"]);
    assert_component_values("url(Lorem\\ ipsu\\6D dolo\\r)url(a\nb)url(a\\\nb)",
        [URL(~"Lorem ipsumdolor"), BadURL, BadURL],
        [~"Invalid URL syntax", ~"Invalid URL syntax"]);

    macro_rules! Integer(
        ($value:expr, $repr:expr) => (NumericValue {
            value: $value as f64, int_value: Some($value), representation: $repr });
    )

    macro_rules! Float(
        ($value:expr, $repr:expr) => (NumericValue {
            value: $value, int_value: None, representation: $repr });
    )

    assert_component_values("42+42-42. 1.5+1.5-1.5.5+.5-.5+-.", [
        Number(Integer!(42, ~"42")),
        Number(Integer!(42, ~"+42")),
        Number(Integer!(-42, ~"-42")), Delim('.'), WhiteSpace,
        Number(Float!(1.5, ~"1.5")),
        Number(Float!(1.5, ~"+1.5")),
        Number(Float!(-1.5, ~"-1.5")),
        Number(Float!(0.5, ~".5")),
        Number(Float!(0.5, ~"+.5")),
        Number(Float!(-0.5, ~"-.5")), Delim('+'), Delim('-'), Delim('.')
    ], []);
    assert_component_values("42e2px 42e+2 42e-2.", [
        Number(Float!(4200., ~"42e2")), Ident(~"px"), WhiteSpace,
        Number(Float!(4200., ~"42e+2")), WhiteSpace,
        Number(Float!(0.42, ~"42e-2")), Delim('.')
    ], []);
    assert_component_values("42%+.5%-1%", [
        Percentage(Integer!(42, ~"42")),
        Percentage(Float!(0.5, ~"+.5")),
        Percentage(Integer!(-1, ~"-1")),
    ], []);
    assert_component_values("42-Px+.5\\u -1url(7-0\\", [
        Dimension(Integer!(42, ~"42"), ~"-Px"),
        Dimension(Float!(0.5, ~"+.5"), ~"u"), WhiteSpace,
        Dimension(Integer!(-1, ~"-1"), ~"url"), ParenthesisBlock(~[
            Number(Integer!(7, ~"7")),
            Number(Integer!(0, ~"-0")), Delim('\\'),
        ]),
    ], [~"Invalid escape"]);

    assert_component_values("", [], []);
    assert_component_values("42 foo([aa ()b], -){\n  }", [
        Number(Integer!(42, ~"42")),
        WhiteSpace,
        Function(~"foo", ~[
            SquareBraketBlock(~[
                Ident(~"aa"), WhiteSpace, ParenthesisBlock(~[]), Ident(~"b")]),
            Delim(','), WhiteSpace, Delim('-')]),
        CurlyBraketBlock(~[
            WhiteSpace])], []);
    assert_component_values("'foo", [String(~"foo")], []);
}


#[cfg(test)]
pub fn json_to_component_value_list(json_items: ~[json::Json])
                                    -> (~[ComponentValue], ~[~str]) {
    use JList = extra::json::List;
    use JString = extra::json::String;

    fn numeric(representation: ~str, value: float, number_type: ~str) -> NumericValue {
        let is_integer = match number_type {
            ~"integer" => true,
            ~"number" => false,
            _ => fail!(),
        };
        let result = NumericValue::new(representation, is_integer);
        assert_eq!(result.value as float, value);
        result
    }

    let mut component_values = ~[];
    let mut errors = ~[];

    macro_rules! recurse(
        ($nested_json:expr) => ({
            let (content, nested_errors) = json_to_component_value_list($nested_json.to_owned());
            do vec::consume(nested_errors) |_, err| { errors.push(err) }
            content
        });
    )

    do vec::consume(json_items) |_, item| {
        match item {
            JList([JString(~"ident"), JString(v)]) => component_values.push(Ident(v)),
            JList([JString(~"at-keyword"), JString(v)]) => component_values.push(AtKeyword(v)),
            JList([JString(~"url"), JString(v)]) => component_values.push(URL(v)),
            JList([JString(~"string"), JString(v)]) => component_values.push(String(v)),

            JList([JString(~"hash"), JString(v), JString(~"unrestricted")])
            => component_values.push(Hash(v)),
            JList([JString(~"hash"), JString(v), JString(~"id")])
            => component_values.push(IDHash(v)),

            JList([JString(~"number"), JString(representation), json::Number(value),
                   JString(number_type)])
            => component_values.push(Number(numeric(representation, value, number_type))),
            JList([JString(~"percentage"), JString(representation), json::Number(value),
                   JString(number_type)])
            => component_values.push(Percentage(numeric(representation, value, number_type))),
            JList([JString(~"dimension"), JString(representation), json::Number(value),
                   JString(number_type), JString(dimension)])
            => component_values.push(Dimension(numeric(representation, value, number_type),
                                               dimension)),
            // XXX TODO: unicode ranges

            JString(~" ") => component_values.push(WhiteSpace),
            JString(~":") => component_values.push(Colon),
            JString(~";") => component_values.push(Semicolon),
            JString(~"<!--") => component_values.push(CDO),
            JString(~"-->") => component_values.push(CDC),
            JString(string) => {
                assert_eq!(string.len(), 1);
                component_values.push(Delim(string[0] as char))
            },

            // XXX These seem like they shouldn’t be necessary
            JList([JString(~"{}")]) => component_values.push(CurlyBraketBlock(~[])),
            JList([JString(~"[]")]) => component_values.push(SquareBraketBlock(~[])),
            JList([JString(~"()")]) => component_values.push(ParenthesisBlock(~[])),
            JList([JString(~"function"), JString(name)])
            => component_values.push(Function(name, ~[])),
            //

            JList([JString(~"{}"), ..content])
            => component_values.push(CurlyBraketBlock(recurse!(content))),
            JList([JString(~"[]"), ..content])
            => component_values.push(SquareBraketBlock(recurse!(content))),
            JList([JString(~"()"), ..content])
            => component_values.push(ParenthesisBlock(recurse!(content))),
            JList([JString(~"function"), JString(name), ..content])
            => component_values.push(Function(name, recurse!(content))),

            JList([JString(~"error"), JString(~"}")])
            => component_values.push(CloseCurlyBraket),
            JList([JString(~"error"), JString(~"]")])
            => component_values.push(CloseSquareBraket),
            JList([JString(~"error"), JString(~")")])
            => component_values.push(CloseSquareBraket),

            JList([JString(~"error"), JString(~"bad-string")])
            => component_values.push(BadString),
            JList([JString(~"error"), JString(~"bad-url")])
            => component_values.push(BadURL),
            JList([JString(~"error"), JString(~"bad-escape")])
            => errors.push(~"Invalid escape"),

            item => fail!(fmt!("%?", item))
        }
    }
    (component_values, errors)
}


#[test]
fn test_component_value_list_json() {
    let items = match json::from_str(include_str!(
            // https://github.com/SimonSapin/tinycss2/tree/master/tinycss2/tests
            // TODO: use git subtree or something to have the JSON files in this repository.
            "../tinycss2/tinycss2/tests/component_value_list.json")) {
        Ok(json::List(items)) => items,
        _ => fail!()
    };
    assert!(items.len() % 2 == 0);
    let mut input: Option<~str> = None;
    do vec::consume(items) |_, item| {
        match (&input, item) {
            (&None, json::String(string)) => input = Some(string),
            (&Some(_), json::List(expected)) => {
                let input = input.swap_unwrap();
                let mut parser = Parser::from_str(input);
                let mut results = ~[];
                for iter_component_values(parser) |c| { results.push(c) }
                let errors = vec::map_consume(match parser { ~Parser {errors: e, _} => e },
                                              |SyntaxError {message: m, _}| m);
                let (expected, expected_errors) = json_to_component_value_list(expected);
                check_results(input, results, expected, errors, expected_errors);
            }
            _ => fail!()
        };
    }
}
