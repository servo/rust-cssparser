/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use std::borrow::Cow::Borrowed;
use std::fs::File;
use std::io::{self, Write};
use std::path::Path;
use std::process::Command;
use std::mem;
use rustc_serialize::json::{self, Json, ToJson};
use tempdir::TempDir;

use encoding::label::encoding_from_whatwg_label;

use super::{Parser, Delimiter, Token, NumericValue, PercentageValue, SourceLocation,
            DeclarationListParser, DeclarationParser, RuleListParser,
            AtRuleType, AtRuleParser, QualifiedRuleParser,
            parse_one_declaration, parse_one_rule, parse_important,
            decode_stylesheet_bytes,
            Color, RGBA, parse_nth, ToCss};


macro_rules! JArray {
    ($($e: expr,)*) => { JArray![ $( $e ),* ] };
    ($($e: expr),*) => { Json::Array(vec!( $( $e.to_json() ),* )) }
}


fn write_whole_file(path: &Path, data: &str) -> io::Result<()> {
    (try!(File::create(path))).write_all(data.as_bytes())
}


fn print_json_diff(results: &Json, expected: &Json) -> io::Result<()> {
    let temp = try!(TempDir::new("rust-cssparser-tests"));
    let results = results.pretty().to_string() + "\n";
    let expected = expected.pretty().to_string() + "\n";
    let result_path = temp.path().join("results.json");
    let expected_path = temp.path().join("expected.json");
    try!(write_whole_file(&result_path, &results));
    try!(write_whole_file(&expected_path, &expected));
    try!(Command::new("colordiff")
        .arg("-u1000")
        .arg(&result_path)
        .arg(&expected_path)
        .status());
    Ok(())
}


fn almost_equals(a: &Json, b: &Json) -> bool {
    match (a, b) {
        (&Json::I64(a), _) => almost_equals(&Json::F64(a as f64), b),
        (&Json::U64(a), _) => almost_equals(&Json::F64(a as f64), b),
        (_, &Json::I64(b)) => almost_equals(a, &Json::F64(b as f64)),
        (_, &Json::U64(b)) => almost_equals(a, &Json::F64(b as f64)),

        (&Json::F64(a), &Json::F64(b)) => (a - b).abs() < 1e-6,

        (&Json::Boolean(a), &Json::Boolean(b)) => a == b,
        (&Json::String(ref a), &Json::String(ref b)) => a == b,
        (&Json::Array(ref a), &Json::Array(ref b)) => {
            a.len() == b.len() &&
            a.iter().zip(b.iter()).all(|(ref a, ref b)| almost_equals(*a, *b))
        },
        (&Json::Object(_), &Json::Object(_)) => panic!("Not implemented"),
        (&Json::Null, &Json::Null) => true,
        _ => false,
    }
}

fn normalize(json: &mut Json) {
    match *json {
        Json::Array(ref mut list) => {
            match find_url(list) {
                Some(Ok(url)) => *list = vec!["url".to_json(), Json::String(url)],
                Some(Err(())) => *list = vec!["error".to_json(), "bad-url".to_json()],
                None => {
                    for item in list.iter_mut() {
                        normalize(item)
                    }
                }
            }
        }
        Json::String(ref mut s) => {
            if *s == "extra-input" || *s == "empty" {
                *s = "invalid".to_string()
            }
        }
        _ => {}
    }
}

fn find_url(list: &mut [Json]) -> Option<Result<String, ()>> {
    if list.len() < 2 ||
        list[0].as_string() != Some("function") ||
        list[1].as_string() != Some("url") {
        return None
    }

    let mut args = list[2..].iter_mut().filter(|a| a.as_string() != Some(" "));
    if let (Some(&mut Json::Array(ref mut arg)), None) = (args.next(), args.next()) {
        if arg.len() == 2 && arg[0].as_string() == Some("string") {
            if let &mut Json::String(ref mut value) = &mut arg[1] {
                return Some(Ok(mem::replace(value, String::new())))
            }
        }
    }

    Some(Err(()))
}


fn assert_json_eq(results: json::Json, mut expected: json::Json, message: String) {
    normalize(&mut expected);
    if !almost_equals(&results, &expected) {
        print_json_diff(&results, &expected).unwrap();
        panic!(message)
    }
}


fn run_raw_json_tests<F: Fn(Json, Json) -> ()>(json_data: &str, run: F) {
    let items = match Json::from_str(json_data) {
        Ok(Json::Array(items)) => items,
        _ => panic!("Invalid JSON")
    };
    assert!(items.len() % 2 == 0);
    let mut input = None;
    for item in items.into_iter() {
        match (&input, item) {
            (&None, json_obj) => input = Some(json_obj),
            (&Some(_), expected) => {
                let input = input.take().unwrap();
                run(input, expected)
            },
        };
    }
}


fn run_json_tests<F: Fn(&mut Parser) -> Json>(json_data: &str, parse: F) {
    run_raw_json_tests(json_data, |input, expected| {
        match input {
            Json::String(input) => {
                let result = parse(&mut Parser::new(&input));
                assert_json_eq(result, expected, input);
            },
            _ => panic!("Unexpected JSON")
        }
    });
}


#[test]
fn component_value_list() {
    run_json_tests(include_str!("css-parsing-tests/component_value_list.json"), |input| {
        Json::Array(component_values_to_json(input))
    });
}


#[test]
fn one_component_value() {
    run_json_tests(include_str!("css-parsing-tests/one_component_value.json"), |input| {
        input.parse_entirely(|input| {
            Ok(one_component_value_to_json(try!(input.next()), input))
        }).unwrap_or(JArray!["error", "invalid"])
    });
}


#[test]
fn declaration_list() {
    run_json_tests(include_str!("css-parsing-tests/declaration_list.json"), |input| {
        Json::Array(DeclarationListParser::new(input, JsonParser).map(|result| {
            result.unwrap_or(JArray!["error", "invalid"])
        }).collect())
    });
}


#[test]
fn one_declaration() {
    run_json_tests(include_str!("css-parsing-tests/one_declaration.json"), |input| {
        parse_one_declaration(input, &mut JsonParser).unwrap_or(JArray!["error", "invalid"])
    });
}


#[test]
fn rule_list() {
    run_json_tests(include_str!("css-parsing-tests/rule_list.json"), |input| {
        Json::Array(RuleListParser::new_for_nested_rule(input, JsonParser).map(|result| {
            result.unwrap_or(JArray!["error", "invalid"])
        }).collect())
    });
}


#[test]
fn stylesheet() {
    run_json_tests(include_str!("css-parsing-tests/stylesheet.json"), |input| {
        Json::Array(RuleListParser::new_for_stylesheet(input, JsonParser).map(|result| {
            result.unwrap_or(JArray!["error", "invalid"])
        }).collect())
    });
}


#[test]
fn one_rule() {
    run_json_tests(include_str!("css-parsing-tests/one_rule.json"), |input| {
        parse_one_rule(input, &mut JsonParser).unwrap_or(JArray!["error", "invalid"])
    });
}


#[test]
fn stylesheet_from_bytes() {
    run_raw_json_tests(include_str!("css-parsing-tests/stylesheet_bytes.json"),
                       |input, expected| {
        let map = match input {
            Json::Object(map) => map,
            _ => panic!("Unexpected JSON")
        };

        let result = {
            let css = get_string(&map, "css_bytes").unwrap().chars().map(|c| {
                assert!(c as u32 <= 0xFF);
                c as u8
            }).collect::<Vec<u8>>();
            let protocol_encoding_label = get_string(&map, "protocol_encoding");
            let environment_encoding = get_string(&map, "environment_encoding")
                .and_then(encoding_from_whatwg_label);

            let (css_unicode, encoding) = decode_stylesheet_bytes(
                &css, protocol_encoding_label, environment_encoding);
            let input = &mut Parser::new(&css_unicode);
            let rules = RuleListParser::new_for_stylesheet(input, JsonParser)
                        .map(|result| result.unwrap_or(JArray!["error", "invalid"]))
                        .collect::<Vec<_>>();
            JArray![rules, encoding.name()]
        };
        assert_json_eq(result, expected, Json::Object(map).to_string());
    });

    fn get_string<'a>(map: &'a json::Object, key: &str) -> Option<&'a str> {
        match map.get(key) {
            Some(&Json::String(ref s)) => Some(s),
            Some(&Json::Null) => None,
            None => None,
            _ => panic!("Unexpected JSON"),
        }
    }
}


#[test]
fn expect_no_error_token() {
    assert!(Parser::new("foo 4px ( / { !bar }").expect_no_error_token().is_ok());
    assert!(Parser::new(")").expect_no_error_token().is_err());
    assert!(Parser::new("}").expect_no_error_token().is_err());
    assert!(Parser::new("(a){]").expect_no_error_token().is_err());
    assert!(Parser::new("'\n'").expect_no_error_token().is_err());
    assert!(Parser::new("url('\n'").expect_no_error_token().is_err());
    assert!(Parser::new("url(a b)").expect_no_error_token().is_err());
    assert!(Parser::new("url(\u{7F})").expect_no_error_token().is_err());
}


/// https://github.com/servo/rust-cssparser/issues/71
#[test]
fn outer_block_end_consumed() {
    let mut input = Parser::new("(calc(true))");
    assert!(input.expect_parenthesis_block().is_ok());
    assert!(input.parse_nested_block(|input| input.expect_function_matching("calc")).is_ok());
    println!("{:?}", input.position());
    assert_eq!(input.next(), Err(()));
}


fn run_color_tests<F: Fn(Result<Color, ()>) -> Json>(json_data: &str, to_json: F) {
    run_json_tests(json_data, |input| {
        to_json(input.parse_entirely(Color::parse))
    });
}


#[test]
fn color3() {
    run_color_tests(include_str!("css-parsing-tests/color3.json"), |c| c.ok().to_json())
}


#[test]
fn color3_hsl() {
    run_color_tests(include_str!("css-parsing-tests/color3_hsl.json"), |c| c.ok().to_json())
}


/// color3_keywords.json is different: R, G and B are in 0..255 rather than 0..1
#[test]
fn color3_keywords() {
    run_color_tests(include_str!("css-parsing-tests/color3_keywords.json"), |c| {
        match c {
            Ok(Color::RGBA(RGBA { red: r, green: g, blue: b, alpha: a }))
            => [r * 255., g * 255., b * 255., a].to_json(),
            Ok(Color::CurrentColor) => "currentColor".to_json(),
            Err(()) => Json::Null,
        }
    });
}


#[test]
fn nth() {
    run_json_tests(include_str!("css-parsing-tests/An+B.json"), |input| {
        input.parse_entirely(parse_nth).ok().to_json()
    });
}


#[test]
fn serializer() {
    run_json_tests(include_str!("css-parsing-tests/component_value_list.json"), |input| {
        fn write_to(input: &mut Parser, string: &mut String) {
            while let Ok(token) = input.next_including_whitespace_and_comments() {
                token.to_css(string).unwrap();
                let closing_token = match token {
                    Token::Function(_) | Token::ParenthesisBlock => Some(Token::CloseParenthesis),
                    Token::SquareBracketBlock => Some(Token::CloseSquareBracket),
                    Token::CurlyBracketBlock => Some(Token::CloseCurlyBracket),
                    _ => None
                };
                if let Some(closing_token) = closing_token {
                    input.parse_nested_block(|input| {
                        write_to(input, string);
                        Ok(())
                    }).unwrap();
                    closing_token.to_css(string).unwrap();
                }
            }
        }
        let mut serialized = String::new();
        write_to(input, &mut serialized);
        let parser = &mut Parser::new(&serialized);
        Json::Array(component_values_to_json(parser))
    });
}


#[test]
fn serialize_current_color() {
    let c = Color::CurrentColor;
    assert!(c.to_css_string() == "currentColor");
}


#[test]
fn serialize_rgb_full_alpha() {
    let c = Color::RGBA(RGBA { red: 1.0, green: 0.9, blue: 0.8, alpha: 1.0 });
    assert!(c.to_css_string() == "rgb(255, 230, 204)");
}


#[test]
fn serialize_rgba() {
    let c = Color::RGBA(RGBA { red: 0.1, green: 0.2, blue: 0.3, alpha: 0.5 });
    assert!(c.to_css_string() == "rgba(26, 51, 77, 0.5)");
}

#[test]
fn line_numbers() {
    let mut input = Parser::new("foo bar\nbaz\r\n\n\"a\\\r\nb\"");
    assert_eq!(input.current_source_location(), SourceLocation { line: 1, column: 1 });
    assert_eq!(input.next_including_whitespace(), Ok(Token::Ident(Borrowed("foo"))));
    assert_eq!(input.current_source_location(), SourceLocation { line: 1, column: 4 });
    assert_eq!(input.next_including_whitespace(), Ok(Token::WhiteSpace(" ")));
    assert_eq!(input.current_source_location(), SourceLocation { line: 1, column: 5 });
    assert_eq!(input.next_including_whitespace(), Ok(Token::Ident(Borrowed("bar"))));
    assert_eq!(input.current_source_location(), SourceLocation { line: 1, column: 8 });
    assert_eq!(input.next_including_whitespace(), Ok(Token::WhiteSpace("\n")));
    assert_eq!(input.current_source_location(), SourceLocation { line: 2, column: 1 });
    assert_eq!(input.next_including_whitespace(), Ok(Token::Ident(Borrowed("baz"))));
    assert_eq!(input.current_source_location(), SourceLocation { line: 2, column: 4 });
    let position = input.position();

    assert_eq!(input.next_including_whitespace(), Ok(Token::WhiteSpace("\r\n\n")));
    assert_eq!(input.current_source_location(), SourceLocation { line: 4, column: 1 });

    assert_eq!(input.source_location(position), SourceLocation { line: 2, column: 4 });

    assert_eq!(input.next_including_whitespace(), Ok(Token::QuotedString(Borrowed("ab"))));
    assert_eq!(input.current_source_location(), SourceLocation { line: 5, column: 3 });
    assert_eq!(input.next_including_whitespace(), Err(()));
}

#[test]
fn line_delimited() {
    let mut input = Parser::new(" { foo ; bar } baz;,");
    assert_eq!(input.next(), Ok(Token::CurlyBracketBlock));
    assert_eq!(input.parse_until_after(Delimiter::Semicolon, |_| Ok(42)), Err(()));
    assert_eq!(input.next(), Ok(Token::Comma));
    assert_eq!(input.next(), Err(()));
}

impl ToJson for Color {
    fn to_json(&self) -> json::Json {
        match *self {
            Color::RGBA(RGBA { red, green, blue, alpha }) => {
                [red, green, blue, alpha].to_json()
            },
            Color::CurrentColor => "currentColor".to_json(),
        }
    }
}


struct JsonParser;


impl DeclarationParser for JsonParser {
    type Declaration = Json;

    fn parse_value(&self, name: &str, input: &mut Parser) -> Result<Json, ()> {
        let mut value = vec![];
        let mut important = false;
        loop {
            let start_position = input.position();
            if let Ok(mut token) = input.next_including_whitespace() {
                // Hack to deal with css-parsing-tests assuming that
                // `!important` in the middle of a declaration value is OK.
                // This can never happen per spec
                // (even CSS Variables forbid top-level `!`)
                if token == Token::Delim('!') {
                    input.reset(start_position);
                    if parse_important(input).is_ok() {
                        if input.is_exhausted() {
                            important = true;
                            break
                        }
                    }
                    input.reset(start_position);
                    token = input.next_including_whitespace().unwrap();
                }
                value.push(one_component_value_to_json(token, input));
            } else {
                break
            }
        }
        Ok(JArray![
            "declaration",
            name,
            value,
            important,
        ])
    }
}

impl AtRuleParser for JsonParser {
    type Prelude = Vec<Json>;
    type AtRule = Json;

    fn parse_prelude(&self, name: &str, input: &mut Parser)
                     -> Result<AtRuleType<Vec<Json>, Json>, ()> {
        Ok(AtRuleType::OptionalBlock(vec![
            "at-rule".to_json(),
            name.to_json(),
            Json::Array(component_values_to_json(input)),
        ]))
    }

    fn parse_block(&self, mut prelude: Vec<Json>, input: &mut Parser) -> Result<Json, ()> {
        prelude.push(Json::Array(component_values_to_json(input)));
        Ok(Json::Array(prelude))
    }

    fn rule_without_block(&self, mut prelude: Vec<Json>) -> Json {
        prelude.push(Json::Null);
        Json::Array(prelude)
    }
}

impl QualifiedRuleParser for JsonParser {
    type Prelude = Vec<Json>;
    type QualifiedRule = Json;

    fn parse_prelude(&self, input: &mut Parser) -> Result<Vec<Json>, ()> {
        Ok(component_values_to_json(input))
    }

    fn parse_block(&self, prelude: Vec<Json>, input: &mut Parser) -> Result<Json, ()> {
        Ok(JArray![
            "qualified rule",
            prelude,
            component_values_to_json(input),
        ])
    }
}

fn component_values_to_json(input: &mut Parser) -> Vec<Json> {
    let mut values = vec![];
    while let Ok(token) = input.next_including_whitespace() {
        values.push(one_component_value_to_json(token, input));
    }
    values
}


fn one_component_value_to_json(token: Token, input: &mut Parser) -> Json {
    fn numeric(value: NumericValue) -> Vec<json::Json> {
        vec![
            Token::Number(value).to_css_string().to_json(),
            match value.int_value { Some(i) => i.to_json(), None => value.value.to_json() },
            match value.int_value { Some(_) => "integer", None => "number" }.to_json()
        ]
    }

    fn nested(input: &mut Parser) -> Vec<Json> {
        input.parse_nested_block(|input| Ok(component_values_to_json(input))).unwrap()
    }

    match token {
        Token::Ident(value) => JArray!["ident", value],
        Token::AtKeyword(value) => JArray!["at-keyword", value],
        Token::Hash(value) => JArray!["hash", value, "unrestricted"],
        Token::IDHash(value) => JArray!["hash", value, "id"],
        Token::QuotedString(value) => JArray!["string", value],
        Token::Url(value) => JArray!["url", value],
        Token::Delim('\\') => "\\".to_json(),
        Token::Delim(value) => value.to_string().to_json(),

        Token::Number(value) => Json::Array(vec!["number".to_json()] + &*numeric(value)),
        Token::Percentage(PercentageValue { unit_value, int_value, signed }) => Json::Array(
            vec!["percentage".to_json()] + &*numeric(NumericValue {
                value: unit_value * 100.,
                int_value: int_value,
                signed: signed,
            })),
        Token::Dimension(value, unit) => Json::Array(
            vec!["dimension".to_json()] + &*numeric(value) + &[unit.to_json()][..]),

        Token::UnicodeRange(start, end) => JArray!["unicode-range", start, end],

        Token::WhiteSpace(_) => " ".to_json(),
        Token::Comment(_) => "/**/".to_json(),
        Token::Colon => ":".to_json(),
        Token::Semicolon => ";".to_json(),
        Token::Comma => ",".to_json(),
        Token::IncludeMatch => "~=".to_json(),
        Token::DashMatch => "|=".to_json(),
        Token::PrefixMatch => "^=".to_json(),
        Token::SuffixMatch => "$=".to_json(),
        Token::SubstringMatch => "*=".to_json(),
        Token::Column => "||".to_json(),
        Token::CDO => "<!--".to_json(),
        Token::CDC => "-->".to_json(),

        Token::Function(name) => Json::Array(vec!["function".to_json(), name.to_json()] +
                                             &*nested(input)),
        Token::ParenthesisBlock => Json::Array(vec!["()".to_json()] + &*nested(input)),
        Token::SquareBracketBlock => Json::Array(vec!["[]".to_json()] + &*nested(input)),
        Token::CurlyBracketBlock => Json::Array(vec!["{}".to_json()] + &nested(input)),
        Token::BadUrl => JArray!["error", "bad-url"],
        Token::BadString => JArray!["error", "bad-string"],
        Token::CloseParenthesis => JArray!["error", ")"],
        Token::CloseSquareBracket => JArray!["error", "]"],
        Token::CloseCurlyBracket => JArray!["error", "}"],
    }
}
