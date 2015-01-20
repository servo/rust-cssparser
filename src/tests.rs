/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use std::borrow::Cow::Borrowed;
use std::io::{mod, File, Command, Writer, TempDir, IoResult};
use std::num::Float;
use std::mem;
use serialize::json::{mod, Json, ToJson};
use test;

use encoding::label::encoding_from_whatwg_label;

use super::{Parser, Token, NumericValue, PercentageValue, SourceLocation,
            DeclarationListParser, DeclarationParser, RuleListParser,
            AtRuleType, AtRuleParser, QualifiedRuleParser,
            parse_one_declaration, parse_one_rule, parse_important,
            parse_stylesheet_rules_from_bytes,
            Color, RGBA, parse_color_keyword, parse_nth, ToCss};


macro_rules! JArray {
    ($($e: expr,)*) => { JArray![ $( $e ),* ] };
    ($($e: expr),*) => { Json::Array(vec!( $( $e.to_json() ),* )) }
}


fn write_whole_file(path: &Path, data: &str) -> IoResult<()> {
    (try!(File::open_mode(path, io::Open, io::Write))).write(data.as_bytes())
}


fn print_json_diff(results: &Json, expected: &Json) -> IoResult<()> {
    use std::io::stdio::stdout;

    let temp = try!(TempDir::new("rust-cssparser-tests"));
    let results = results.to_pretty_str() + "\n";
    let expected = expected.to_pretty_str() + "\n";
    let mut result_path = temp.path().clone();
    result_path.push("results.json");
    let mut expected_path = temp.path().clone();
    expected_path.push("expected.json");
    try!(write_whole_file(&result_path, results.as_slice()));
    try!(write_whole_file(&expected_path, expected.as_slice()));
    stdout().write(try!(Command::new("colordiff")
        .arg("-u1000")
        .arg(result_path.display().to_string())
        .arg(expected_path.display().to_string())
        .output()
        .map_err(|_| io::standard_error(io::OtherIoError))
    ).output.as_slice())
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
            match find_url(list.as_mut_slice()) {
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
            if s.as_slice() == "extra-input" || s.as_slice() == "empty" {
                *s = "invalid".into_string()
            }
        }
        _ => {}
    }
}

fn find_url(list: &mut [Json]) -> Option<Result<String, ()>> {
    if let [Json::String(ref a1), Json::String(ref a2), ..] = list.as_mut_slice() {
        if !(a1.as_slice() == "function" && a2.as_slice() == "url") {
            return None
        }
    } else {
        return None
    };
    let args = list.slice_from_mut(2);

    let args = if !args.is_empty() && args[0] == " ".to_json() {
        args.slice_from_mut(1)
    } else {
        args.as_mut_slice()
    };

    if let [Json::Array(ref mut arg1), ref rest..] = args.as_mut_slice() {
        if let [Json::String(ref a11), Json::String(ref mut a12)] = arg1.as_mut_slice() {
            if a11.as_slice() == "string" && rest.iter().all(|a| a == &" ".to_json()) {
                return Some(Ok(mem::replace(a12, String::new())))
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


fn run_raw_json_tests(json_data: &str, run: |Json, Json|) {
    let items = match json::from_str(json_data) {
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


fn run_json_tests(json_data: &str, parse: |input: &mut Parser| -> Json) {
    run_raw_json_tests(json_data, |input, expected| {
        match input {
            Json::String(input) => {
                let result = parse(&mut Parser::new(input.as_slice()));
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

            parse_stylesheet_rules_from_bytes(
                css.as_slice(), protocol_encoding_label, environment_encoding,
                JsonParser, |encoding, rules| {
                    Json::Array(vec![
                        Json::Array(rules.map(|result| {
                            result.unwrap_or(JArray!["error", "invalid"])
                        }).collect()),
                        encoding.name().to_json()
                    ])
                })
        };
        assert_json_eq(result, expected, Json::Object(map).to_string());
    });

    fn get_string<'a>(map: &'a json::Object, key: &str) -> Option<&'a str> {
        match map.get(key) {
            Some(&Json::String(ref s)) => Some(s.as_slice()),
            Some(&Json::Null) => None,
            None => None,
            _ => panic!("Unexpected JSON"),
        }
    }
}


fn run_color_tests(json_data: &str, to_json: |result: Result<Color, ()>| -> Json) {
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


#[bench]
fn bench_color_lookup_red(b: &mut test::Bencher) {
    b.iter(|| {
        test::black_box(parse_color_keyword("red"))
    });
}


#[bench]
fn bench_color_lookup_lightgoldenrodyellow(b: &mut test::Bencher) {
    b.iter(|| {
        test::black_box(parse_color_keyword("lightgoldenrodyellow"))
    });
}


#[bench]
fn bench_color_lookup_fail(b: &mut test::Bencher) {
    b.iter(|| {
        test::black_box(parse_color_keyword("lightgoldenrodyellowbazinga"))
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
        fn flatten(input: &mut Parser, tokens: &mut Vec<Token<'static>>) {
            while let Ok(token) = input.next_including_whitespace() {
                let closing_token = match token {
                    Token::Function(_) | Token::ParenthesisBlock => Some(Token::CloseParenthesis),
                    Token::SquareBracketBlock => Some(Token::CloseSquareBracket),
                    Token::CurlyBracketBlock => Some(Token::CloseCurlyBracket),
                    _ => None
                };
                tokens.push(token.into_owned());
                if let Some(closing_token) = closing_token {
                    input.parse_nested_block(|input| {
                        flatten(input, tokens);
                        Ok(())
                    }).unwrap();
                    tokens.push(closing_token);
                }
            }
        }
        let mut tokens = vec![];
        flatten(input, &mut tokens);
        let serialized = tokens.to_css_string();
        let parser = &mut Parser::new(serialized.as_slice());
        Json::Array(component_values_to_json(parser))
    });
}


#[test]
fn serialize_current_color() {
    let c = Color::CurrentColor;
    assert!(c.to_css_string().as_slice() == "currentColor");
}


#[test]
fn serialize_rgb_full_alpha() {
    let c = Color::RGBA(RGBA { red: 1.0, green: 0.9, blue: 0.8, alpha: 1.0 });
    assert!(c.to_css_string().as_slice() == "rgb(255, 230, 204)");
}


#[test]
fn serialize_rgba() {
    let c = Color::RGBA(RGBA { red: 0.1, green: 0.2, blue: 0.3, alpha: 0.5 });
    assert!(c.to_css_string().as_slice() == "rgba(26, 51, 77, 0.5)");
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


impl DeclarationParser<Json> for JsonParser {
    fn parse_value(&mut self, name: &str, input: &mut Parser) -> Result<Json, ()> {
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

impl AtRuleParser<Vec<Json>, Json> for JsonParser {
    fn parse_prelude(&mut self, name: &str, input: &mut Parser)
                     -> Result<AtRuleType<Vec<Json>, Json>, ()> {
        Ok(AtRuleType::OptionalBlock(vec![
            "at-rule".to_json(),
            name.to_json(),
            Json::Array(component_values_to_json(input)),
        ]))
    }

    fn parse_block(&mut self, mut prelude: Vec<Json>, input: &mut Parser) -> Result<Json, ()> {
        prelude.push(Json::Array(component_values_to_json(input)));
        Ok(Json::Array(prelude))
    }

    fn rule_without_block(&mut self, mut prelude: Vec<Json>) -> Result<Json, ()> {
        prelude.push(Json::Null);
        Ok(Json::Array(prelude))
    }
}

impl QualifiedRuleParser<Vec<Json>, Json> for JsonParser {
    fn parse_prelude(&mut self, input: &mut Parser) -> Result<Vec<Json>, ()> {
        Ok(component_values_to_json(input))
    }

    fn parse_block(&mut self, prelude: Vec<Json>, input: &mut Parser) -> Result<Json, ()> {
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
        Token::Delim(value) => String::from_char(1, value).to_json(),

        Token::Number(value) => Json::Array(vec!["number".to_json()] + numeric(value)),
        Token::Percentage(PercentageValue { unit_value, int_value, signed }) => Json::Array(
            vec!["percentage".to_json()] + numeric(NumericValue {
                value: unit_value * 100.,
                int_value: int_value,
                signed: signed,
            })),
        Token::Dimension(value, unit) => Json::Array(
            vec!["dimension".to_json()] + numeric(value) + [unit.to_json()].as_slice()),

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
                                             nested(input)),
        Token::ParenthesisBlock => Json::Array(vec!["()".to_json()] + nested(input)),
        Token::SquareBracketBlock => Json::Array(vec!["[]".to_json()] + nested(input)),
        Token::CurlyBracketBlock => Json::Array(vec!["{}".to_json()] + nested(input)),
        Token::BadUrl => JArray!["error", "bad-url"],
        Token::BadString => JArray!["error", "bad-string"],
        Token::CloseParenthesis => JArray!["error", ")"],
        Token::CloseSquareBracket => JArray!["error", "]"],
        Token::CloseCurlyBracket => JArray!["error", "}"],
    }
}
