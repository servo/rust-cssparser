/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

// https://drafts.csswg.org/css-syntax/#tokenization

use std::ops::Range;
use std::cell::Cell;
use std::char;
use std::ascii::AsciiExt;
use std::borrow::{Cow, ToOwned};
use std::borrow::Cow::{Owned, Borrowed};
use std::i32;

use self::Token::*;


/// One of the pieces the CSS input is broken into.
///
/// Some components use `CowString` in order to borrow from the original input string
/// and avoid allocating/copying when possible.
#[derive(PartialEq, Debug, Clone)]
pub enum Token<'a> {

    /// A [`<ident-token>`](https://drafts.csswg.org/css-syntax/#ident-token-diagram)
    Ident(Cow<'a, str>),

    /// A [`<at-keyword-token>`](https://drafts.csswg.org/css-syntax/#at-keyword-token-diagram)
    ///
    /// The value does not include the `@` marker.
    AtKeyword(Cow<'a, str>),

    /// A [`<hash-token>`](https://drafts.csswg.org/css-syntax/#hash-token-diagram) with the type flag set to "unrestricted"
    ///
    /// The value does not include the `#` marker.
    Hash(Cow<'a, str>),

    /// A [`<hash-token>`](https://drafts.csswg.org/css-syntax/#hash-token-diagram) with the type flag set to "id"
    ///
    /// The value does not include the `#` marker.
    IDHash(Cow<'a, str>),  // Hash that is a valid ID selector.

    /// A [`<string-token>`](https://drafts.csswg.org/css-syntax/#string-token-diagram)
    ///
    /// The value does not include the quotes.
    QuotedString(Cow<'a, str>),

    /// A [`<url-token>`](https://drafts.csswg.org/css-syntax/#url-token-diagram) or `url( <string-token> )` function
    ///
    /// The value does not include the `url(` `)` markers or the quotes.
    UnquotedUrl(Cow<'a, str>),

    /// A `<delim-token>`
    Delim(char),

    /// A [`<number-token>`](https://drafts.csswg.org/css-syntax/#number-token-diagram)
    Number(NumericValue),

    /// A [`<percentage-token>`](https://drafts.csswg.org/css-syntax/#percentage-token-diagram)
    Percentage(PercentageValue),

    /// A [`<dimension-token>`](https://drafts.csswg.org/css-syntax/#dimension-token-diagram)
    Dimension(NumericValue, Cow<'a, str>),

    /// A [`<unicode-range-token>`](https://drafts.csswg.org/css-syntax/#unicode-range-token-diagram)
    ///
    /// Components are the start and end code points, respectively.
    ///
    /// The tokenizer only reads up to 6 hex digit (up to 0xFF_FFFF),
    /// but does not check that code points are within the range of Unicode (up to U+10_FFFF).
    UnicodeRange(u32, u32),

    /// A [`<whitespace-token>`](https://drafts.csswg.org/css-syntax/#whitespace-token-diagram)
    WhiteSpace(&'a str),

    /// A comment.
    ///
    /// The CSS Syntax spec does not generate tokens for comments,
    /// But we do, because we can (borrowed &str makes it cheap).
    ///
    /// The value does not include the `/*` `*/` markers.
    Comment(&'a str),

    /// A `:` `<colon-token>`
    Colon,  // :

    /// A `;` `<semicolon-token>`
    Semicolon,  // ;

    /// A `,` `<comma-token>`
    Comma,  // ,

    /// A `~=` [`<include-match-token>`](https://drafts.csswg.org/css-syntax/#include-match-token-diagram)
    IncludeMatch,

    /// A `|=` [`<dash-match-token>`](https://drafts.csswg.org/css-syntax/#dash-match-token-diagram)
    DashMatch,

    /// A `^=` [`<prefix-match-token>`](https://drafts.csswg.org/css-syntax/#prefix-match-token-diagram)
    PrefixMatch,

    /// A `$=` [`<suffix-match-token>`](https://drafts.csswg.org/css-syntax/#suffix-match-token-diagram)
    SuffixMatch,

    /// A `*=` [`<substring-match-token>`](https://drafts.csswg.org/css-syntax/#substring-match-token-diagram)
    SubstringMatch,

    /// A `||` [`<column-token>`](https://drafts.csswg.org/css-syntax/#column-token-diagram)
    Column,

    /// A `<!--` [`<CDO-token>`](https://drafts.csswg.org/css-syntax/#CDO-token-diagram)
    CDO,

    /// A `-->` [`<CDC-token>`](https://drafts.csswg.org/css-syntax/#CDC-token-diagram)
    CDC,

    /// A [`<function-token>`](https://drafts.csswg.org/css-syntax/#function-token-diagram)
    ///
    /// The value (name) does not include the `(` marker.
    Function(Cow<'a, str>),

    /// A `<(-token>`
    ParenthesisBlock,

    /// A `<[-token>`
    SquareBracketBlock,

    /// A `<{-token>`
    CurlyBracketBlock,

    /// A `<bad-url-token>`
    ///
    /// This token always indicates a parse error.
    BadUrl,

    /// A `<bad-string-token>`
    ///
    /// This token always indicates a parse error.
    BadString,

    /// A `<)-token>`
    ///
    /// When obtained from one of the `Parser::next*` methods,
    /// this token is always unmatched and indicates a parse error.
    CloseParenthesis,

    /// A `<]-token>`
    ///
    /// When obtained from one of the `Parser::next*` methods,
    /// this token is always unmatched and indicates a parse error.
    CloseSquareBracket,

    /// A `<}-token>`
    ///
    /// When obtained from one of the `Parser::next*` methods,
    /// this token is always unmatched and indicates a parse error.
    CloseCurlyBracket,
}


impl<'a> Token<'a> {
    /// Return whether this token represents a parse error.
    ///
    /// `BadUrl` and `BadString` are tokenizer-level parse errors.
    ///
    /// `CloseParenthesis`, `CloseSquareBracket`, and `CloseCurlyBracket` are *unmatched*
    /// and therefore parse errors when returned by one of the `Parser::next*` methods.
    pub fn is_parse_error(&self) -> bool {
        matches!(
            *self,
            BadUrl | BadString | CloseParenthesis | CloseSquareBracket | CloseCurlyBracket
        )
    }
}


/// The numeric value of `Number` and `Dimension` tokens.
#[derive(PartialEq, Debug, Copy, Clone)]
pub struct NumericValue {
    /// The value as a float
    pub value: f32,

    /// If the origin source did not include a fractional part, the value as an integer.
    pub int_value: Option<i32>,

    /// Whether the number had a `+` or `-` sign.
    ///
    /// This is used is some cases like the <An+B> micro syntax. (See the `parse_nth` function.)
    pub has_sign: bool,
}


/// The numeric value of `Percentage` tokens.
#[derive(PartialEq, Debug, Copy, Clone)]
pub struct PercentageValue {
    /// The value as a float, divided by 100 so that the nominal range is 0.0 to 1.0.
    pub unit_value: f32,

    /// If the origin source did not include a fractional part, the value as an integer. It is **not** divided by 100.
    pub int_value: Option<i32>,

    /// Whether the number had a `+` or `-` sign.
    pub has_sign: bool,
}


#[derive(Clone)]
pub struct Tokenizer<'a> {
    input: &'a str,
    /// Counted in bytes, not code points. From 0.
    position: usize,
    /// Cache for `source_location()`
    last_known_source_location: Cell<(SourcePosition, SourceLocation)>,
    var_functions: SeenStatus,
    viewport_percentages: SeenStatus,
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum SeenStatus {
    DontCare,
    LookingForThem,
    SeenAtLeastOne,
}


impl<'a> Tokenizer<'a> {
    #[inline]
    pub fn new(input: &str) -> Tokenizer {
        Tokenizer {
            input: input,
            position: 0,
            last_known_source_location: Cell::new((SourcePosition(0),
                                                   SourceLocation { line: 1, column: 1 })),
            var_functions: SeenStatus::DontCare,
            viewport_percentages: SeenStatus::DontCare,
        }
    }

    #[inline]
    pub fn look_for_var_functions(&mut self) {
        self.var_functions = SeenStatus::LookingForThem;
    }

    #[inline]
    pub fn seen_var_functions(&mut self) -> bool {
        let seen = self.var_functions == SeenStatus::SeenAtLeastOne;
        self.var_functions = SeenStatus::DontCare;
        seen
    }

    #[inline]
    pub fn look_for_viewport_percentages(&mut self) {
        self.viewport_percentages = SeenStatus::LookingForThem;
    }

    #[inline]
    pub fn seen_viewport_percentages(&mut self) -> bool {
        let seen = self.viewport_percentages == SeenStatus::SeenAtLeastOne;
        self.viewport_percentages = SeenStatus::DontCare;
        seen
    }

    #[inline]
    pub fn next(&mut self) -> Result<Token<'a>, ()> {
        next_token(self).ok_or(())
    }

    #[inline]
    pub fn position(&self) -> SourcePosition {
        SourcePosition(self.position)
    }

    #[inline]
    pub fn reset(&mut self, new_position: SourcePosition) {
        self.position = new_position.0;
    }

    #[inline]
    pub fn slice_from(&self, start_pos: SourcePosition) -> &'a str {
        &self.input[start_pos.0..self.position]
    }

    #[inline]
    pub fn slice(&self, range: Range<SourcePosition>) -> &'a str {
        &self.input[range.start.0..range.end.0]
    }

    #[inline]
    pub fn current_source_location(&self) -> SourceLocation {
        let position = SourcePosition(self.position);
        self.source_location(position)
    }

    pub fn source_location(&self, position: SourcePosition) -> SourceLocation {
        let target = position.0;
        let mut location;
        let mut position;
        let (SourcePosition(last_known_position), last_known_location) =
            self.last_known_source_location.get();
        if target >= last_known_position {
            position = last_known_position;
            location = last_known_location;
        } else {
            // For now we’re only traversing the source *forwards* to count newlines.
            // So if the requested position is before the last known one,
            // start over from the beginning.
            position = 0;
            location = SourceLocation { line: 1, column: 1 };
        }
        let mut source = &self.input[position..target];
        while let Some(newline_position) = source.find(|c| matches!(c, '\n' | '\r' | '\x0C')) {
            let offset = newline_position +
                if source[newline_position..].starts_with("\r\n") { 2 } else { 1 };
            source = &source[offset..];
            position += offset;
            location.line += 1;
            location.column = 1;
        }
        debug_assert!(position <= target);
        location.column += target - position;
        self.last_known_source_location.set((SourcePosition(target), location));
        location
    }

    #[inline]
    pub fn next_byte(&self) -> Option<u8> {
        if self.is_eof() {
            None
        } else {
            Some(self.input.as_bytes()[self.position])
        }
    }

    // If false, `tokenizer.next_char()` will not panic.
    #[inline]
    fn is_eof(&self) -> bool { !self.has_at_least(0) }

    // If true, the input has at least `n` bytes left *after* the current one.
    // That is, `tokenizer.char_at(n)` will not panic.
    #[inline]
    fn has_at_least(&self, n: usize) -> bool { self.position + n < self.input.len() }

    #[inline]
    pub fn advance(&mut self, n: usize) { self.position += n }

    // Assumes non-EOF
    #[inline]
    fn next_char(&self) -> char { self.char_at(0) }

    #[inline]
    fn char_at(&self, offset: usize) -> char {
        self.input[self.position + offset..].chars().next().unwrap()
    }

    #[inline]
    fn has_newline_at(&self, offset: usize) -> bool {
        self.position + offset < self.input.len() &&
        matches!(self.char_at(offset), '\n' | '\r' | '\x0C')
    }

    #[inline]
    fn consume_char(&mut self) -> char {
        let c = self.next_char();
        self.position += c.len_utf8();
        c
    }

    #[inline]
    fn starts_with(&self, needle: &str) -> bool {
        self.input[self.position..].starts_with(needle)
    }
}


#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct SourcePosition(usize);


/// The line and column number for a given position within the input.
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct SourceLocation {
    /// The line number, starting at 1 for the first line.
    pub line: usize,

    /// The column number within a line, starting at 1 for first the character of the line.
    pub column: usize,
}


fn next_token<'a>(tokenizer: &mut Tokenizer<'a>) -> Option<Token<'a>> {
    if tokenizer.is_eof() {
        return None
    }
    let c = tokenizer.next_char();
    let token = match c {
        '\t' | '\n' | ' ' | '\r' | '\x0C' => {
            let start_position = tokenizer.position();
            tokenizer.advance(1);
            while !tokenizer.is_eof() {
                match tokenizer.next_char() {
                    ' ' | '\t' | '\n' | '\r' | '\x0C' => tokenizer.advance(1),
                    _ => break,
                }
            }
            WhiteSpace(tokenizer.slice_from(start_position))
        },
        '"' => consume_string(tokenizer, false),
        '#' => {
            tokenizer.advance(1);
            if is_ident_start(tokenizer) { IDHash(consume_name(tokenizer)) }
            else if !tokenizer.is_eof() && match tokenizer.next_char() {
                'a'...'z' | 'A'...'Z' | '0'...'9' | '-' | '_' => true,
                '\\' => !tokenizer.has_newline_at(1),
                _ => c > '\x7F',  // Non-ASCII
            } { Hash(consume_name(tokenizer)) }
            else { Delim(c) }
        },
        '$' => {
            if tokenizer.starts_with("$=") { tokenizer.advance(2); SuffixMatch }
            else { tokenizer.advance(1); Delim(c) }
        },
        '\'' => consume_string(tokenizer, true),
        '(' => { tokenizer.advance(1); ParenthesisBlock },
        ')' => { tokenizer.advance(1); CloseParenthesis },
        '*' => {
            if tokenizer.starts_with("*=") { tokenizer.advance(2); SubstringMatch }
            else { tokenizer.advance(1); Delim(c) }
        },
        '+' => {
            if (
                tokenizer.has_at_least(1)
                && matches!(tokenizer.char_at(1), '0'...'9')
            ) || (
                tokenizer.has_at_least(2)
                && tokenizer.char_at(1) == '.'
                && matches!(tokenizer.char_at(2), '0'...'9')
            ) {
                consume_numeric(tokenizer)
            } else {
                tokenizer.advance(1);
                Delim(c)
            }
        },
        ',' => { tokenizer.advance(1); Comma },
        '-' => {
            if (
                tokenizer.has_at_least(1)
                && matches!(tokenizer.char_at(1), '0'...'9')
            ) || (
                tokenizer.has_at_least(2)
                && tokenizer.char_at(1) == '.'
                && matches!(tokenizer.char_at(2), '0'...'9')
            ) {
                consume_numeric(tokenizer)
            } else if tokenizer.starts_with("-->") {
                tokenizer.advance(3);
                CDC
            } else if is_ident_start(tokenizer) {
                consume_ident_like(tokenizer)
            } else {
                tokenizer.advance(1);
                Delim(c)
            }
        },
        '.' => {
            if tokenizer.has_at_least(1)
                && matches!(tokenizer.char_at(1), '0'...'9'
            ) {
                consume_numeric(tokenizer)
            } else {
                tokenizer.advance(1);
                Delim(c)
            }
        }
        '/' if tokenizer.starts_with("/*") => {
            tokenizer.advance(2);  // consume "/*"
            let start_position = tokenizer.position();
            let content;
            match tokenizer.input[tokenizer.position..].find("*/") {
                Some(offset) => {
                    tokenizer.advance(offset);
                    content = tokenizer.slice_from(start_position);
                    tokenizer.advance(2);
                }
                None => {
                    tokenizer.position = tokenizer.input.len();
                    content = tokenizer.slice_from(start_position);
                }
            }
            Comment(content)
        }
        '0'...'9' => consume_numeric(tokenizer),
        ':' => { tokenizer.advance(1); Colon },
        ';' => { tokenizer.advance(1); Semicolon },
        '<' => {
            if tokenizer.starts_with("<!--") {
                tokenizer.advance(4);
                CDO
            } else {
                tokenizer.advance(1);
                Delim(c)
            }
        },
        '@' => {
            tokenizer.advance(1);
            if is_ident_start(tokenizer) { AtKeyword(consume_name(tokenizer)) }
            else { Delim(c) }
        },
        'u' | 'U' => {
            if tokenizer.has_at_least(2)
               && tokenizer.char_at(1) == '+'
               && matches!(tokenizer.char_at(2), '0'...'9' | 'a'...'f' | 'A'...'F' | '?')
            { consume_unicode_range(tokenizer) }
            else { consume_ident_like(tokenizer) }
        },
        'a'...'z' | 'A'...'Z' | '_' | '\0' => consume_ident_like(tokenizer),
        '[' => { tokenizer.advance(1); SquareBracketBlock },
        '\\' => {
            if !tokenizer.has_newline_at(1) { consume_ident_like(tokenizer) }
            else { tokenizer.advance(1); Delim(c) }
        },
        ']' => { tokenizer.advance(1); CloseSquareBracket },
        '^' => {
            if tokenizer.starts_with("^=") { tokenizer.advance(2); PrefixMatch }
            else { tokenizer.advance(1); Delim(c) }
        },
        '{' => { tokenizer.advance(1); CurlyBracketBlock },
        '|' => {
            if tokenizer.starts_with("|=") { tokenizer.advance(2); DashMatch }
            else if tokenizer.starts_with("||") { tokenizer.advance(2); Column }
            else { tokenizer.advance(1); Delim(c) }
        },
        '}' => { tokenizer.advance(1); CloseCurlyBracket },
        '~' => {
            if tokenizer.starts_with("~=") { tokenizer.advance(2); IncludeMatch }
            else { tokenizer.advance(1); Delim(c) }
        },
        _ => {
            if c > '\x7F' {  // Non-ASCII
                consume_ident_like(tokenizer)
            } else {
                tokenizer.advance(1);
                Delim(c)
            }
        },
    };
    Some(token)
}


fn consume_string<'a>(tokenizer: &mut Tokenizer<'a>, single_quote: bool) -> Token<'a> {
    match consume_quoted_string(tokenizer, single_quote) {
        Ok(value) => QuotedString(value),
        Err(()) => BadString
    }
}


/// Return `Err(())` on syntax error (ie. unescaped newline)
fn consume_quoted_string<'a>(tokenizer: &mut Tokenizer<'a>, single_quote: bool)
                             -> Result<Cow<'a, str>, ()> {
    tokenizer.advance(1);  // Skip the initial quote
    let start_pos = tokenizer.position();
    let mut string;
    loop {
        if tokenizer.is_eof() {
            return Ok(Borrowed(tokenizer.slice_from(start_pos)))
        }
        match tokenizer.next_char() {
            '"' if !single_quote => {
                let value = tokenizer.slice_from(start_pos);
                tokenizer.advance(1);
                return Ok(Borrowed(value))
            }
            '\'' if single_quote => {
                let value = tokenizer.slice_from(start_pos);
                tokenizer.advance(1);
                return Ok(Borrowed(value))
            }
            '\\' | '\0' => {
                string = tokenizer.slice_from(start_pos).to_owned();
                break
            }
            '\n' | '\r' | '\x0C' => return Err(()),
            _ => {
                tokenizer.consume_char();
            }
        }
    }

    while !tokenizer.is_eof() {
        if matches!(tokenizer.next_char(), '\n' | '\r' | '\x0C') {
            return Err(());
        }
        match tokenizer.consume_char() {
            '"' if !single_quote => break,
            '\'' if single_quote => break,
            '\\' => {
                if !tokenizer.is_eof() {
                    match tokenizer.next_char() {
                        // Escaped newline
                        '\n' | '\x0C' => tokenizer.advance(1),
                        '\r' => {
                            tokenizer.advance(1);
                            if !tokenizer.is_eof() && tokenizer.next_char() == '\n' {
                                tokenizer.advance(1);
                            }
                        }
                        _ => string.push(consume_escape(tokenizer))
                    }
                }
                // else: escaped EOF, do nothing.
            }
            '\0' => string.push('\u{FFFD}'),
            c => string.push(c),
        }
    }
    Ok(Owned(string))
}


#[inline]
fn is_ident_start(tokenizer: &mut Tokenizer) -> bool {
    !tokenizer.is_eof() && match tokenizer.next_char() {
        'a'...'z' | 'A'...'Z' | '_' | '\0' => true,
        '-' => tokenizer.has_at_least(1) && match tokenizer.char_at(1) {
            'a'...'z' | 'A'...'Z' | '-' | '_' | '\0' => true,
            '\\' => !tokenizer.has_newline_at(1),
            c => c > '\x7F',  // Non-ASCII
        },
        '\\' => !tokenizer.has_newline_at(1),
        c => c > '\x7F',  // Non-ASCII
    }
}


fn consume_ident_like<'a>(tokenizer: &mut Tokenizer<'a>) -> Token<'a> {
    let value = consume_name(tokenizer);
    if !tokenizer.is_eof() && tokenizer.next_char() == '(' {
        tokenizer.advance(1);
        if value.eq_ignore_ascii_case("url") {
            consume_unquoted_url(tokenizer).unwrap_or(Function(value))
        } else {
            if tokenizer.var_functions == SeenStatus::LookingForThem &&
                value.eq_ignore_ascii_case("var") {
                tokenizer.var_functions = SeenStatus::SeenAtLeastOne;
            }
            Function(value)
        }
    } else {
        Ident(value)
    }
}

fn consume_name<'a>(tokenizer: &mut Tokenizer<'a>) -> Cow<'a, str> {
    let start_pos = tokenizer.position();
    let mut value;
    loop {
        if tokenizer.is_eof() {
            return Borrowed(tokenizer.slice_from(start_pos))
        }
        match tokenizer.next_char() {
            'a'...'z' | 'A'...'Z' | '0'...'9' | '_' | '-'  => tokenizer.advance(1),
            '\\' | '\0' => {
                value = tokenizer.slice_from(start_pos).to_owned();
                break
            }
            c if c.is_ascii() => return Borrowed(tokenizer.slice_from(start_pos)),
            _ => {
                tokenizer.consume_char();
            }
        }
    }

    while !tokenizer.is_eof() {
        let c = tokenizer.next_char();
        value.push(match c {
            'a'...'z' | 'A'...'Z' | '0'...'9' | '_' | '-'  => {
                tokenizer.advance(1);
                c
            }
            '\\' => {
                if tokenizer.has_newline_at(1) { break }
                tokenizer.advance(1);
                consume_escape(tokenizer)
            }
            '\0' => { tokenizer.advance(1); '\u{FFFD}' },
            c if c.is_ascii() => break,
            _ => tokenizer.consume_char(),
        })
    }
    Owned(value)
}


fn consume_numeric<'a>(tokenizer: &mut Tokenizer<'a>) -> Token<'a> {
    // Parse [+-]?\d*(\.\d+)?([eE][+-]?\d+)?
    // But this is always called so that there is at least one digit in \d*(\.\d+)?

    // Do all the math in f64 so that large numbers overflow to +/-inf
    // and i32::{MIN, MAX} are within range.

    let (has_sign, sign) = match tokenizer.next_char() {
        '-' => (true, -1.),
        '+' => (true, 1.),
        _ => (false, 1.),
    };
    if has_sign {
        tokenizer.advance(1);
    }

    let mut integral_part: f64 = 0.;
    while let Some(digit) = tokenizer.next_char().to_digit(10) {
        integral_part = integral_part * 10. + digit as f64;
        tokenizer.advance(1);
        if tokenizer.is_eof() {
            break
        }
    }

    let mut is_integer = true;

    let mut fractional_part: f64 = 0.;
    if tokenizer.has_at_least(1) && tokenizer.next_char() == '.'
            && matches!(tokenizer.char_at(1), '0'...'9') {
        is_integer = false;
        tokenizer.advance(1);  // Consume '.'
        let mut factor = 0.1;
        while let Some(digit) = tokenizer.next_char().to_digit(10) {
            fractional_part += digit as f64 * factor;
            factor *= 0.1;
            tokenizer.advance(1);
            if tokenizer.is_eof() {
                break
            }
        }
    }

    let mut value = sign * (integral_part + fractional_part);

    if (
        tokenizer.has_at_least(1)
        && matches!(tokenizer.next_char(), 'e' | 'E')
        && matches!(tokenizer.char_at(1), '0'...'9')
    ) || (
        tokenizer.has_at_least(2)
        && matches!(tokenizer.next_char(), 'e' | 'E')
        && matches!(tokenizer.char_at(1), '+' | '-')
        && matches!(tokenizer.char_at(2), '0'...'9')
    ) {
        is_integer = false;
        tokenizer.advance(1);
        let (has_sign, sign) = match tokenizer.next_char() {
            '-' => (true, -1.),
            '+' => (true, 1.),
            _ => (false, 1.),
        };
        if has_sign {
            tokenizer.advance(1);
        }
        let mut exponent: f64 = 0.;
        while let Some(digit) = tokenizer.next_char().to_digit(10) {
            exponent = exponent * 10. + digit as f64;
            tokenizer.advance(1);
            if tokenizer.is_eof() {
                break
            }
        }
        value *= f64::powf(10., sign * exponent);
    }

    let int_value = if is_integer {
        Some(if value >= i32::MAX as f64 {
            i32::MAX
        } else if value <= i32::MIN as f64 {
            i32::MIN
        } else {
            value as i32
        })
    } else {
        None
    };

    if !tokenizer.is_eof() && tokenizer.next_char() == '%' {
        tokenizer.advance(1);
        return Percentage(PercentageValue {
            unit_value: value as f32 / 100.,
            int_value: int_value,
            has_sign: has_sign,
        })
    }
    let value = NumericValue {
        value: value as f32,
        int_value: int_value,
        has_sign: has_sign,
    };
    if is_ident_start(tokenizer) {
        let name = consume_name(tokenizer);
        if tokenizer.viewport_percentages == SeenStatus::LookingForThem {
            if name.eq_ignore_ascii_case("vh") ||
               name.eq_ignore_ascii_case("vw") ||
               name.eq_ignore_ascii_case("vmin") ||
               name.eq_ignore_ascii_case("vmax") {
                   tokenizer.viewport_percentages = SeenStatus::SeenAtLeastOne;
           }
        }
        Dimension(value, name)
    } else {
        Number(value)
    }
}


fn consume_unquoted_url<'a>(tokenizer: &mut Tokenizer<'a>) -> Result<Token<'a>, ()> {
    for (offset, c) in tokenizer.input[tokenizer.position..].char_indices() {
        match c {
            ' ' | '\t' | '\n' | '\r' | '\x0C' => {},
            '"' | '\'' => return Err(()),  // Do not advance
            ')' => {
                tokenizer.advance(offset + 1);
                return Ok(UnquotedUrl(Borrowed("")));
            }
            _ => {
                tokenizer.advance(offset);
                return Ok(consume_unquoted_url(tokenizer))
            }
        }
    }
    tokenizer.position = tokenizer.input.len();
    return Ok(UnquotedUrl(Borrowed("")));

    fn consume_unquoted_url<'a>(tokenizer: &mut Tokenizer<'a>) -> Token<'a> {
        let start_pos = tokenizer.position();
        let mut string;
        loop {
            if tokenizer.is_eof() {
                return UnquotedUrl(Borrowed(tokenizer.slice_from(start_pos)))
            }
            match tokenizer.next_char() {
                ' ' | '\t' | '\n' | '\r' | '\x0C' => {
                    let value = tokenizer.slice_from(start_pos);
                    tokenizer.advance(1);
                    return consume_url_end(tokenizer, Borrowed(value))
                }
                ')' => {
                    let value = tokenizer.slice_from(start_pos);
                    tokenizer.advance(1);
                    return UnquotedUrl(Borrowed(value))
                }
                '\x01'...'\x08' | '\x0B' | '\x0E'...'\x1F' | '\x7F'  // non-printable
                    | '"' | '\'' | '(' => {
                    tokenizer.advance(1);
                    return consume_bad_url(tokenizer)
                },
                '\\' | '\0' => {
                    string = tokenizer.slice_from(start_pos).to_owned();
                    break
                }
                _ => {
                    tokenizer.consume_char();
                }
            }
        }
        while !tokenizer.is_eof() {
            let next_char = match tokenizer.consume_char() {
                ' ' | '\t' | '\n' | '\r' | '\x0C' => {
                    return consume_url_end(tokenizer, Owned(string))
                }
                ')' => break,
                '\x01'...'\x08' | '\x0B' | '\x0E'...'\x1F' | '\x7F'  // non-printable
                    | '"' | '\'' | '(' => return consume_bad_url(tokenizer),
                '\\' => {
                    if tokenizer.has_newline_at(0) {
                        return consume_bad_url(tokenizer)
                    }
                    consume_escape(tokenizer)
                },
                '\0' => '\u{FFFD}',
                c => c
            };
            string.push(next_char)
        }
        UnquotedUrl(Owned(string))
    }

    fn consume_url_end<'a>(tokenizer: &mut Tokenizer<'a>, string: Cow<'a, str>) -> Token<'a> {
        while !tokenizer.is_eof() {
            match tokenizer.consume_char() {
                ' ' | '\t' | '\n' | '\r' | '\x0C' => (),
                ')' => break,
                _ => return consume_bad_url(tokenizer)
            }
        }
        UnquotedUrl(string)
    }

    fn consume_bad_url<'a>(tokenizer: &mut Tokenizer<'a>) -> Token<'a> {
        // Consume up to the closing )
        while !tokenizer.is_eof() {
            match tokenizer.consume_char() {
                ')' => break,
                '\\' => tokenizer.advance(1), // Skip an escaped ')' or '\'
                _ => ()
            }
        }
        BadUrl
    }
}



fn consume_unicode_range<'a>(tokenizer: &mut Tokenizer<'a>) -> Token<'a> {
    tokenizer.advance(2);  // Skip U+
    let (hex_value, hex_digits) = consume_hex_digits(tokenizer);
    let max_question_marks = 6 - hex_digits;
    let mut question_marks = 0;
    while question_marks < max_question_marks && !tokenizer.is_eof()
            && tokenizer.next_char() == '?' {
        question_marks += 1;
        tokenizer.advance(1)
    }
    let start;
    let end;
    if question_marks > 0 {
        start = hex_value << (question_marks * 4);
        end = ((hex_value + 1) << (question_marks * 4)) - 1;
    } else {
        start = hex_value;
        if tokenizer.has_at_least(1) &&
           tokenizer.next_char() == '-' &&
           matches!(tokenizer.char_at(1), '0'...'9' | 'A'...'F' | 'a'...'f') {
            tokenizer.advance(1);
            let (hex_value, _) = consume_hex_digits(tokenizer);
            end = hex_value;
        } else {
            end = start;
        }
    }
    UnicodeRange(start, end)
}


// (value, number of digits up to 6)
fn consume_hex_digits<'a>(tokenizer: &mut Tokenizer<'a>) -> (u32, u32) {
    let mut value = 0;
    let mut digits = 0;
    while digits < 6 && !tokenizer.is_eof() {
        match tokenizer.next_char().to_digit(16) {
            Some(digit) => {
                value = value * 16 + digit;
                digits += 1;
                tokenizer.advance(1);
            }
            None => break
        }
    }
    (value, digits)
}


// Assumes that the U+005C REVERSE SOLIDUS (\) has already been consumed
// and that the next input character has already been verified
// to not be a newline.
fn consume_escape(tokenizer: &mut Tokenizer) -> char {
    if tokenizer.is_eof() { return '\u{FFFD}' }  // Escaped EOF
    match tokenizer.next_char() {
        '0'...'9' | 'A'...'F' | 'a'...'f' => {
            let (c, _) = consume_hex_digits(tokenizer);
            if !tokenizer.is_eof() {
                match tokenizer.next_char() {
                    ' ' | '\t' | '\n' | '\x0C' => tokenizer.advance(1),
                    '\r' => {
                        tokenizer.advance(1);
                        if !tokenizer.is_eof() && tokenizer.next_char() == '\n' {
                            tokenizer.advance(1);
                        }
                    }
                    _ => ()
                }
            }
            static REPLACEMENT_CHAR: char = '\u{FFFD}';
            if c != 0 {
                let c = char::from_u32(c);
                c.unwrap_or(REPLACEMENT_CHAR)
            } else {
                REPLACEMENT_CHAR
            }
        },
        '\0' => {
            tokenizer.advance(1);
            '\u{FFFD}'
        }
        _ => tokenizer.consume_char()
    }
}
