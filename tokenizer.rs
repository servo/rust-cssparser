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

extern mod core;
use super::utils::*;
use super::ast::*;


pub struct SyntaxError {
    message: ~str,
//    source_location: ~str,
    source_line: uint,
    source_column: uint,
}


pub struct InputStream {
    priv data: ~str,
    priv length: uint,  // All counted in bytes, not characters
    priv position: uint,  // All counted in bytes, not characters
    priv line: uint,
    priv column: uint,  // All counted in bytes, not characters
    priv errors: ~[SyntaxError],
}


pub impl InputStream {
    fn from_str(input: &str) -> ~InputStream {
        let data = preprocess(input);
        ~InputStream {
            length: data.len(),
            data: data,
            position: 0,
            line: 1,
            column: 1,
            errors: ~[],
        }
    }
}


pub fn consume_component_value(input: &mut InputStream) -> Option<ComponentValue> {
    consume_comments(input);
    if input.is_eof() { return None }
    let c = input.current_char();
    Some(match c {
        '-' => {
            if input.starts_with(~"-->") {
                input.position += 3;
                CDC
            }
            else if next_is_namestart_or_escape(input) {
                consume_ident(input)
            } else {
                consume_numeric(input)
            }
        },
        '<' => {
            if input.starts_with(~"<!--") {
                input.position += 4;
                CDO
            } else {
                input.position += 1;
                Delim('<')
            }
        },
        '0'..'9' | '.' | '+' => consume_numeric(input),
        'u' | 'U' => consume_unicode_range(input),
        'a'..'z' | 'A'..'Z' | '_' | '\\' => consume_ident(input),
        _ if c >= '\x80' => consume_ident(input),  // Non-ASCII
        _ => {
            match input.consume_char() {
                '\t' | '\n' | ' ' => {
                    while !input.is_eof() {
                        match input.current_char() {
                            '\t' | '\n' | ' '
                                => input.position += 1,
                            _ => break,
                        }
                    }
                    WhiteSpace
                },
                '"' => consume_quoted_string(input, false),
                '#' => consume_hash(input),
                '\'' => consume_quoted_string(input, true),
                '(' => ParenthesisBlock(consume_block(input, CloseParenthesis)),
                ')' => CloseParenthesis,
                ':' => Colon,
                ';' => Semicolon,
                '@' => consume_at_keyword(input),
                '[' => SquareBraketBlock(consume_block(input, CloseSquareBraket)),
                ']' => CloseSquareBraket,
                '{' => CurlyBraketBlock(consume_block(input, CloseCurlyBraket)),
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


impl InputStream {
    fn error(&mut self, message: ~str) {
        self.errors.push(SyntaxError{
            message: message, source_line: self.line, source_column: self.column })
    }

    #[inline]
    fn is_eof(&self) -> bool { self.position >= self.length }

    // Assumes non-EOF
    #[inline]
    fn current_char(&self) -> char { str::char_at(self.data, self.position) }

    #[inline]
    fn consume_char(&mut self) -> char {
        let range = str::char_range_at(self.data, self.position);
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
            let range = str::char_range_at(self.data, position);
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
        let haystack: &str = self.data;
        for needle.each |c| { if haystack[i] != c { return false; } i += 1u; }
        return true;
    }
}


#[inline]
fn consume_comments(input: &mut InputStream) {
    while input.starts_with(~"/*") {
        input.position += 2;  // +2 to consume "/*"
        match str::find_str_from(input.data, "*/", input.position) {
            // +2 to consume "*/"
            Some(end_position) => input.position = end_position + 2,
            None => input.position = input.length  // EOF
        }
    }
}


fn consume_block(input: &mut InputStream, ending_token: ComponentValue) -> ~[ComponentValue] {
    let mut content = ~[];
    loop {
        match consume_component_value(input) {
            Some(component_value) => {
                if component_value == ending_token { return content }
                content.push(component_value)
            }
            None => return content
        }
    }
}


macro_rules! is_match(
    ($value:expr, $pattern:pat) => (
        match $value { $pattern => true, _ => false }
    );
)


#[inline]
fn is_invalid_escape(input: &mut InputStream) -> bool {
    match input.next_n_chars(2) {
        ['\\', '\n'] | ['\\'] => true,
        _ => false,
    }
}


#[inline]
fn is_namestart_or_escape(input: &mut InputStream) -> bool {
    match input.current_char() {
        'a'..'z' | 'A'..'Z' | '_' => true,
        '\\' => !is_invalid_escape(input),
        c => c >= '\x80',  // Non-ASCII
    }
}


#[inline]
fn next_is_namestart_or_escape(input: &mut InputStream) -> bool {
    input.position += 1;
    let result = !input.is_eof() && is_namestart_or_escape(input);
    input.position -= 1;
    result
}


fn consume_quoted_string(input: &mut InputStream, single_quote: bool) -> ComponentValue {
    let mut string: ~str = ~"";
    while !input.is_eof() {
        match input.consume_char() {
            '"' if !single_quote => break,
            '\'' if single_quote => break,
            '\n' => {
                input.error(~"Newline in quoted string");
                return BadString;
            },
            '\\' => {
                match input.next_n_chars(1) {
                    // Quoted newline
                    ['\n'] => input.position += 1,
                    [] => {
                        input.error(~"Escaped EOF");
                        return BadString
                    },
                    _ => string.push_char(consume_escape(input))
                }
            }
            c => string.push_char(c),
        }
    }
    String(string)
}


fn consume_hash(input: &mut InputStream) -> ComponentValue {
    let string = consume_ident_string_rest(input);
    if string == ~"" { Delim('#') } else { Hash(string) }
}


fn consume_at_keyword(input: &mut InputStream) -> ComponentValue {
    match consume_ident_string(input) {
        Some(string) => AtKeyword(string),
        None => Delim('@')
    }
}


fn consume_ident(input: &mut InputStream) -> ComponentValue {
    match consume_ident_string(input) {
        Some(string) => {
            if input.is_eof() { return Ident(string) }
            match input.current_char() {
                '(' => {
                    input.position += 1;
                    if ascii_lower(string) == ~"url" { consume_url(input) }
                    else { Function(string, consume_block(input, CloseParenthesis)) }
                },
                _ => Ident(string)
            }
        },
        None => match input.current_char() {
            '-' => {
                input.position += 1;
                Delim('-')
            },
            '\\' => {
                input.position += 1;
                input.error(~"Invalid escape");
                Delim('\\')
            },
            _ => fail!(),  // Should not have called consume_ident() here.
        }
    }
}

fn consume_ident_string(input: &mut InputStream) -> Option<~str> {
    match input.current_char() {
        '-' => if !next_is_namestart_or_escape(input) { None }
               else { Some(consume_ident_string_rest(input)) },
        '\\' if is_invalid_escape(input) => return None,
        _ if !is_namestart_or_escape(input) => return None,
        _ => Some(consume_ident_string_rest(input))
    }
}


fn consume_ident_string_rest(input: &mut InputStream) -> ~str {
    let mut string = ~"";
    while !input.is_eof() {
        let c = input.current_char();
        let next_char = match c {
            'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '-'  => {
                input.position += 1; c },
            _ if c >= '\x80' => input.consume_char(),  // Non-ASCII
            '\\' => {
                if is_invalid_escape(input) { break }
                input.position += 1;
                consume_escape(input)
            },
            _ => break
        };
        string.push_char(next_char)
    }
    string
}


fn consume_numeric(input: &mut InputStream) -> ComponentValue {
    let c = input.consume_char();
    match c {
        '-' | '+' => consume_numeric_sign(input, c),
        '.' => {
            if input.is_eof() { return Delim('.') }
            match input.current_char() {
                '0'..'9' => consume_numeric_fraction(input, ~"."),
                _ => Delim('.'),
            }
        },
        '0'..'9' => consume_numeric_rest(input, c),
        _ => fail!(),  // consume_numeric() should not have been called here.
    }
}


fn consume_numeric_sign(input: &mut InputStream, sign: char)
        -> ComponentValue {
    if input.is_eof() { return Delim(sign) }
    match input.current_char() {
        '.' => {
            input.position += 1;
            if !input.is_eof()
                    && is_match!(input.current_char(), '0'..'9') {
                consume_numeric_fraction(
                    input, str::from_char(sign) + ~".")
            } else {
                input.position -= 1;
                Delim(sign)
            }
        },
        '0'..'9' => consume_numeric_rest(input, sign),
        _ => Delim(sign)
    }
}


fn consume_numeric_rest(input: &mut InputStream, initial_char: char)
        -> ComponentValue {
    let mut string = str::from_char(initial_char);
    while !input.is_eof() {
        let c = input.current_char();
        match c {
            '0'..'9' => { string.push_char(c); input.position += 1 },
            '.' => {
                input.position += 1;
                if !input.is_eof()
                        && is_match!(input.current_char(), '0'..'9') {
                    string.push_char('.');
                    return consume_numeric_fraction(input, string);
                } else {
                    input.position -= 1; break
                }
            },
            _ => match consume_scientific_number(input, string) {
                Ok(token) => return token,
                Err(s) => { string = s; break }
            }
        }
    }
    consume_numeric_end(input, NumericValue::new(string, true))
}


fn consume_numeric_fraction(input: &mut InputStream, string: ~str)
        -> ComponentValue {
    let mut string: ~str = string;
    while !input.is_eof() {
        match input.current_char() {
            '0'..'9' => string.push_char(input.consume_char()),
            _ => match consume_scientific_number(input, string) {
                Ok(token) => return token,
                Err(s) => { string = s; break }
            }
        }
    }
    consume_numeric_end(input, NumericValue::new(string, false))
}


fn consume_numeric_end(input: &mut InputStream, value: NumericValue)
        -> ComponentValue {
    if input.is_eof() { return Number(value) }
    match input.current_char() {
        '%' => { input.position += 1; Percentage(value) },
        _ => {
            match consume_ident_string(input) {
                Some(unit) => Dimension(value, unit),
                None => Number(value),
            }
        },
    }
}


fn consume_scientific_number(input: &mut InputStream, string: ~str)
        -> Result<ComponentValue, ~str> {
    let next_3 = input.next_n_chars(3);
    let mut string: ~str = string;
    if (next_3.len() >= 2
        && (next_3[0] == 'e' || next_3[0] == 'E')
        && (is_match!(next_3[1], '0'..'9'))
    ) {
        string.push_char(next_3[0]);
        string.push_char(next_3[1]);
        input.position += 2;
    } else if (
        next_3.len() == 3
        && (next_3[0] == 'e' || next_3[0] == 'E')
        && (next_3[1] == '+' || next_3[1] == '-')
        && is_match!(next_3[2], '0'..'9')
    ) {
        string.push_char(next_3[0]);
        string.push_char(next_3[1]);
        string.push_char(next_3[2]);
        input.position += 3;
    } else {
        return Err(string)
    }
    while !input.is_eof() && is_match!(input.current_char(), '0'..'9') {
        string.push_char(input.consume_char())
    }
    Ok(Number(NumericValue::new(string, false)))
}


fn consume_url(input: &mut InputStream) -> ComponentValue {
    while !input.is_eof() {
        match input.current_char() {
            '\t' | '\n' | ' ' => input.position += 1,
            '"' => return consume_quoted_url(input, false),
            '\'' => return consume_quoted_url(input, true),
            ')' => { input.position += 1; break },
            _ => return consume_unquoted_url(input),
        }
    }
    URL(~"")
}


fn consume_quoted_url(input: &mut InputStream, single_quote: bool)
        -> ComponentValue {
    input.position += 1;  // The initial quote
    match consume_quoted_string(input, single_quote) {
        String(string) => consume_url_end(input, string),
        BadString => consume_bad_url(input),
        // consume_quoted_string() only returns String or BadString
        _ => fail!(),
    }
}


fn consume_url_end(input: &mut InputStream, string: ~str)
        -> ComponentValue {
    while !input.is_eof() {
        match input.consume_char() {
            '\t' | '\n' | ' ' => (),
            ')' => break,
            _ => return consume_bad_url(input)
        }
    }
    URL(string)
}


fn consume_unquoted_url(input: &mut InputStream) -> ComponentValue {
    let mut string = ~"";
    while !input.is_eof() {
        let next_char = match input.consume_char() {
            '\t' | '\n' | ' '
                => return consume_url_end(input, string),
            ')' => break,
            '\x00'..'\x08' | '\x0E'..'\x1F' | '\x7F'..'\x9F'  // non-printable
                | '"' | '\'' | '(' => return consume_bad_url(input),
            '\\' => match input.next_n_chars(1) {
                ['\n'] | [] => return consume_bad_url(input),
                _ => consume_escape(input)
            },
            c => c
        };
        string.push_char(next_char)
    }
    URL(string)
}


fn consume_bad_url(input: &mut InputStream) -> ComponentValue {
    // Consume up to the closing )
    while !input.is_eof() {
        match input.consume_char() {
            ')' => break,
            '\\' => input.position += 1, // Skip an escaped ) or \
            _ => ()
        }
    }
    input.error(~"Invalid URL syntax");
    BadURL
}


fn consume_unicode_range(input: &mut InputStream)
        -> ComponentValue {
    let next_3 = input.next_n_chars(3);
    // We got here with U or u
    assert!(next_3[0] == 'u' || next_3[0] == 'U');
    // Check if this is indeed an unicode range. Fallback on ident.
    if next_3.len() == 3 && next_3[1] == '+' {
        match next_3[2] {
            '0'..'9' | 'a'..'f' | 'A'..'F' => input.position += 2,
            _ => { return consume_ident(input) }
        }
    } else { return consume_ident(input) }

    let mut hex = ~[];
    while hex.len() < 6 && !input.is_eof() {
        let c = input.current_char();
        match c {
            '0'..'9' | 'A'..'F' | 'a'..'f' => {
                hex.push(c); input.position += 1 },
            _ => break
        }
    }
    assert!(hex.len() > 0);
    let max_question_marks = 6u - hex.len();
    let mut question_marks = 0u;
    while question_marks < max_question_marks && !input.is_eof()
            && input.current_char() == '?' {
        question_marks += 1;
        input.position += 1
    }
    let start: char, end: char;
    if question_marks > 0 {
        start = char_from_hex(hex + vec::from_elem(question_marks, '0'));
        end = char_from_hex(hex + vec::from_elem(question_marks, 'F'));
    } else {
        start = char_from_hex(hex);
        hex = ~[];
        if !input.is_eof() && input.current_char() == '-' {
            input.position += 1;
            while hex.len() < 6 && !input.is_eof() {
                let c = input.current_char();
                match c {
                    '0'..'9' | 'A'..'F' | 'a'..'f' => {
                        hex.push(c); input.position += 1 },
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
fn consume_escape(input: &mut InputStream) -> char {
    let c = input.consume_char();
    match c {
        '0'..'9' | 'A'..'F' | 'a'..'f' => {
            let mut hex = ~[c];
            while hex.len() < 6 && !input.is_eof() {
                let c = input.current_char();
                match c {
                    '0'..'9' | 'A'..'F' | 'a'..'f' => {
                        hex.push(c); input.position += 1 },
                    _ => break
                }
            }
            if !input.is_eof() {
                match input.current_char() {
                    '\t' | '\n' | ' ' => input.position += 1,
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
fn test_component_values() {
    fn assert_component_values(input: &str, expected_component_values: &[ComponentValue],
                               expected_errors: &[~str]) {
        let mut stream = InputStream::from_str(input);
        let mut result: ~[ComponentValue] = ~[];
        loop {
            match consume_component_value(stream) {
                Some(component_value) => result.push(component_value),
                None => break
            }
        }
        let errors = vec::map_consume(
            core::util::replace(&mut stream.errors, ~[]),
            |SyntaxError{message: m, source_line: _, source_column: _}| m);
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
