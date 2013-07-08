// http://dev.w3.org/csswg/css3-syntax/#tokenization

use std::{str, u32, vec};
use extra::json;
use utils::*;
use ast::*;


pub fn consume_component_values_list(parser: &mut Parser) -> ~[ComponentValue] {
    let mut result = ~[];
    for each_component_values(parser) |component_value| { result.push(component_value) }
    result
}


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


// Old style for loop
pub fn each_component_values(parser: &mut Parser, it: &fn (v: ComponentValue) -> bool) -> bool {
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
                '~' if !parser.is_eof() && parser.current_char() == '='
                => { parser.position += 1; IncludeMath }
                '|' if !parser.is_eof() && parser.current_char() == '='
                => { parser.position += 1; DashMatch }
                '|' if !parser.is_eof() && parser.current_char() == '|'
                => { parser.position += 1; Column }
                '^' if !parser.is_eof() && parser.current_char() == '='
                => { parser.position += 1; PrefixMatch }
                '$' if !parser.is_eof() && parser.current_char() == '='
                => { parser.position += 1; SuffixMatch }
                '*' if !parser.is_eof() && parser.current_char() == '='
                => { parser.position += 1; SubstringMatch }
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
    for each_component_values(parser) |component_value| {
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
    parser.next_n_chars(2) == ~['\\', '\n']
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
                parser.position -= 1;
                return BadString;
            },
            '\\' => {
                match parser.next_n_chars(1) {
                    ['\n'] => parser.position += 1,  // Escaped newline
                    [] => (),  // Escaped EOF
                    _ => string.push_char(consume_escape(parser))
                }
            }
            c => string.push_char(c),
        }
    }
    String(string)
}


fn consume_hash(parser: &mut Parser) -> ComponentValue {
    match consume_ident_string(parser) {
        Some(string) => IDHash(string),
        None => match consume_ident_string_rest(parser) {
            ~"" => Delim('#'),
            string => Hash(string),
        }
    }
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
    Ok(consume_numeric_end(parser, NumericValue::new(string, false)))
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
            '\x00'..'\x08' | '\x0B' | '\x0E'..'\x1F' | '\x7F'  // non-printable
                | '"' | '\'' | '(' => return consume_bad_url(parser),
            '\\' => match parser.next_n_chars(1) {
                ['\n'] => return consume_bad_url(parser),
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
// to not be a newline.
fn consume_escape(parser: &mut Parser) -> char {
    if parser.is_eof() { return '\uFFFD' }  // Escaped EOF
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


#[cfg(test)]
pub fn component_value_list_to_json(values: ~[ComponentValue]) -> ~[json::Json] {
    use JList = extra::json::List;
    use JString = extra::json::String;
    use JNumber = extra::json::Number;

    fn numeric(NumericValue{representation: r, value: v, int_value: i}: NumericValue)
               -> ~[json::Json] {
        ~[JString(r), JNumber(v as float), JString(
            match i { Some(_) => ~"integer", _ => ~"number" })]
    }

    let mut results = ~[];
    do vec::consume(values) |_, value| {
        let json = match value {
            Ident(value) => JList(~[JString(~"ident"), JString(value)]),
            AtKeyword(value) => JList(~[JString(~"at-keyword"), JString(value)]),
            Hash(value) => JList(~[JString(~"hash"), JString(value), JString(~"unrestricted")]),
            IDHash(value) => JList(~[JString(~"hash"), JString(value), JString(~"id")]),
            String(value) => JList(~[JString(~"string"), JString(value)]),
            URL(value) => JList(~[JString(~"url"), JString(value)]),
            Delim('\\') => { results.push(JList(~[JString(~"error"), JString(~"bad-escape")]));
                             JString(~"\\") },
            Delim(value) => JString(str::from_char(value)),

            Number(value) => JList(~[JString(~"number")] + numeric(value)),
            Percentage(value) => JList(~[JString(~"percentage")] + numeric(value)),
            Dimension(value, unit)
            => JList(~[JString(~"dimension")] + numeric(value) + [JString(unit)]),

            // TODO:
            UnicodeRange(_start, _end) => fail!(),
            EmptyUnicodeRange => fail!(),

            WhiteSpace => JString(~" "),
            Colon => JString(~":"),
            Semicolon => JString(~";"),
            IncludeMath => JString(~"~="),
            DashMatch => JString(~"|="),
            PrefixMatch => JString(~"^="),
            SuffixMatch => JString(~"$="),
            SubstringMatch => JString(~"*="),
            Column => JString(~"||"),
            CDO => JString(~"<!--"),
            CDC => JString(~"-->"),

            Function(name, arguments)
            => JList(~[JString(~"function"), JString(name)]
                     + component_value_list_to_json(arguments)),
            ParenthesisBlock(content)
            => JList(~[JString(~"()")] + component_value_list_to_json(content)),
            SquareBraketBlock(content)
            => JList(~[JString(~"[]")] + component_value_list_to_json(content)),
            CurlyBraketBlock(content)
            => JList(~[JString(~"{}")] + component_value_list_to_json(content)),

            BadURL => JList(~[JString(~"error"), JString(~"bad-url")]),
            BadString => JList(~[JString(~"error"), JString(~"bad-string")]),
            CloseParenthesis => JList(~[JString(~"error"), JString(~")")]),
            CloseSquareBraket => JList(~[JString(~"error"), JString(~"]")]),
            CloseCurlyBraket => JList(~[JString(~"error"), JString(~"}")]),
        };
        results.push(json)
    }
    results
}


#[test]
fn test_component_value_list_json() {
    let items = match json::from_str(include_str!(
            // https://github.com/SimonSapin/tinycss2/tree/master/tinycss2/tests
            // TODO: use git subtree or something to have the JSON files in this repository.
            "../tinycss2/tinycss2/tests/component_value_list.json")) {
        Ok(json::List(items)) => items,
        _ => fail!("Invalid JSON in component_value_list.json")
    };
    assert!(items.len() % 2 == 0);
    let mut input: Option<~str> = None;
    do vec::consume(items) |_, item| {
        match (&input, item) {
            (&None, json::String(string)) => input = Some(string),
            (&Some(_), json::List(expected)) => {
                let input = input.swap_unwrap();
                let mut parser = Parser::from_str(input);
                let results = component_value_list_to_json(consume_component_values_list(parser));
                assert_vec_equals(results, expected, input);
            }
            _ => fail!()
        };
    }
}
