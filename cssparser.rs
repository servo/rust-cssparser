extern mod std;
use str;


// TODO http://dev.w3.org/csswg/css3-syntax/#the-input-byte-stream
// fn decode(input: &[u8], protocol_encoding: &str, link_encoding: &str,
//           document_encoding: &str) -> ~str
// Use empty strings for “no such encoding information”?


// http://dev.w3.org/csswg/css3-syntax/#preprocessing-the-input-stream
fn preprocess(input: &str) -> ~str {
    // TODO: Is this faster if done in one pass?
    str::replace(str::replace(str::replace(input,
    "\r\n", "\n"),
    "\r", "\n"),
    "\x00", "\uFFFD")
}


#[test]
fn test_preprocess() {
    assert preprocess("") == ~"";
    assert preprocess("Lorem\r\n\t\x00ipusm\ndoror\uFFFD\r")
        == ~"Lorem\n\t\uFFFDipusm\ndoror\uFFFD\n";
}


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

enum NumericValue {
    Integer(int),
    // The spec calls this "number".
    // Use "float" instead to reduce term overloading with "number token".
    Float(float),
}
// TODO: add a NumberValue.as_float() method.

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
