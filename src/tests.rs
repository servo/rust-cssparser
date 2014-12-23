/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use std::io::{mod, File, Command, Writer, TempDir, IoResult};
use std::num::Float;
use std::mem;
use serialize::json::{mod, Json, ToJson};
use test;

use encoding::label::encoding_from_whatwg_label;

use super::{Tokenizer, Parser, Token, NumericValue,
            DeclarationListParser, DeclarationParser, RuleListParser,
            AtRulePrelude, AtRuleParser, QualifiedRuleParser, Priority,
            parse_one_declaration, parse_one_rule, parse_important,
            parse_stylesheet_rules_from_bytes,
            Color, RGBA, parse_nth, ToCss};


macro_rules! JList {
    ($($e: expr,)*) => { JList![ $( $e ),* ] };
    ($($e: expr),*) => { json::List(vec![ $( $e.to_json() ),* ]) };
}


fn write_whole_file(path: &Path, data: &str) -> IoResult<()> {
    (try!(File::open_mode(path, io::Open, io::Write))).write(data.as_bytes())
}


fn print_json_diff(results: &json::Json, expected: &json::Json) -> IoResult<()> {
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


fn almost_equals(a: &json::Json, b: &json::Json) -> bool {
    match (a, b) {
        (&json::I64(a), _) => almost_equals(&json::F64(a as f64), b),
        (&json::U64(a), _) => almost_equals(&json::F64(a as f64), b),
        (_, &json::I64(b)) => almost_equals(a, &json::F64(b as f64)),
        (_, &json::U64(b)) => almost_equals(a, &json::F64(b as f64)),

        (&json::F64(a), &json::F64(b)) => (a - b).abs() < 1e-6,

        (&json::Boolean(a), &json::Boolean(b)) => a == b,
        (&json::String(ref a), &json::String(ref b)) => a == b,
        (&json::List(ref a), &json::List(ref b)) => {
            a.len() == b.len() &&
            a.iter().zip(b.iter()).all(|(ref a, ref b)| almost_equals(*a, *b))
        },
        (&json::Object(_), &json::Object(_)) => panic!("Not implemented"),
        (&json::Null, &json::Null) => true,
        _ => false,
    }
}

fn normalize(json: &mut Json) {
    match *json {
        Json::List(ref mut list) => {
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

    if let [Json::List(ref mut arg1), ref rest..] = args.as_mut_slice() {
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


fn run_raw_json_tests(json_data: &str, run: |json::Json, json::Json|) {
    let items = match json::from_str(json_data) {
        Ok(json::List(items)) => items,
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
            json::String(input) => {
                // FIXME: Use Parser::parse_str when unboxed closures permit.
                let result = parse(&mut Parser::new(&mut Tokenizer::new(input.as_slice())));
                assert_json_eq(result, expected, input);
            },
            _ => panic!("Unexpected JSON")
        }
    });
}


#[test]
fn component_value_list() {
    run_json_tests(include_str!("css-parsing-tests/component_value_list.json"), |input| {
        Json::List(component_values_to_json(input))
    });
}


#[test]
fn one_component_value() {
    run_json_tests(include_str!("css-parsing-tests/one_component_value.json"), |input| {
        input.parse_entirely(|input| {
            Ok(one_component_value_to_json(try!(input.next()), input))
        }).unwrap_or(JList!["error", "invalid"])
    });
}


#[test]
fn declaration_list() {
    run_json_tests(include_str!("css-parsing-tests/declaration_list.json"), |input| {
        Json::List(DeclarationListParser::new(input, JsonParser).map(|result| {
            result.unwrap_or(JList!["error", "invalid"])
        }).collect())
    });
}


#[test]
fn one_declaration() {
    run_json_tests(include_str!("css-parsing-tests/one_declaration.json"), |input| {
        parse_one_declaration(input, &mut JsonParser).unwrap_or(JList!["error", "invalid"])
    });
}


#[test]
fn rule_list() {
    run_json_tests(include_str!("css-parsing-tests/rule_list.json"), |input| {
        Json::List(RuleListParser::new_for_nested_rule(input, JsonParser).map(|result| {
            result.unwrap_or(JList!["error", "invalid"])
        }).collect())
    });
}


#[test]
fn stylesheet() {
    run_json_tests(include_str!("css-parsing-tests/stylesheet.json"), |input| {
        Json::List(RuleListParser::new_for_stylesheet(input, JsonParser).map(|result| {
            result.unwrap_or(JList!["error", "invalid"])
        }).collect())
    });
}


#[test]
fn one_rule() {
    run_json_tests(include_str!("css-parsing-tests/one_rule.json"), |input| {
        parse_one_rule(input, &mut JsonParser).unwrap_or(JList!["error", "invalid"])
    });
}


#[test]
fn stylesheet_from_bytes() {
    run_raw_json_tests(include_str!("css-parsing-tests/stylesheet_bytes.json"),
                       |input, expected| {
        let map = match input {
            json::Object(map) => map,
            _ => panic!("Unexpected JSON")
        };

        let result = {
            let css = get_string(&map, &"css_bytes".to_string()).unwrap().chars().map(|c| {
                assert!(c as u32 <= 0xFF);
                c as u8
            }).collect::<Vec<u8>>();
            let protocol_encoding_label = get_string(&map, &"protocol_encoding".to_string());
            let environment_encoding = get_string(&map, &"environment_encoding".to_string())
                .and_then(encoding_from_whatwg_label);

            parse_stylesheet_rules_from_bytes(
                css.as_slice(), protocol_encoding_label, environment_encoding,
                JsonParser, |encoding, rules| {
                    Json::List(vec![
                        Json::List(rules.map(|result| {
                            result.unwrap_or(JList!["error", "invalid"])
                        }).collect()),
                        encoding.name().to_json()
                    ])
                })
        };
        assert_json_eq(result, expected, json::Object(map).to_string());
    });

    fn get_string<'a>(map: &'a json::JsonObject, key: &String) -> Option<&'a str> {
        match map.get(key) {
            Some(&json::String(ref s)) => Some(s.as_slice()),
            Some(&json::Null) => None,
            None => None,
            _ => panic!("Unexpected JSON"),
        }
    }
}


fn run_color_tests(json_data: &str, to_json: |result: Result<Color, ()>| -> json::Json) {
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
            Err(()) => json::Null,
        }
    });
}


#[bench]
fn bench_color_lookup_red(b: &mut test::Bencher) {
    b.iter(|| {
        assert!(Parser::parse_str("red", Color::parse).is_ok())
    });
}


#[bench]
fn bench_color_lookup_lightgoldenrodyellow(b: &mut test::Bencher) {
    b.iter(|| {
        assert!(Parser::parse_str("lightgoldenrodyellow", Color::parse).is_ok())
    });
}


#[bench]
fn bench_color_lookup_fail(b: &mut test::Bencher) {
    b.iter(|| {
        assert!(Parser::parse_str("lightgoldenrodyellowbazinga", Color::parse).is_ok())
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
        fn flatten(input: &mut Parser, tokens: &mut Vec<Token>) {
            while let Ok(token) = input.next_including_whitespace() {
                let closing_token = match token {
                    Token::Function(_) | Token::ParenthesisBlock => Some(Token::CloseParenthesis),
                    Token::SquareBracketBlock => Some(Token::CloseSquareBracket),
                    Token::CurlyBracketBlock => Some(Token::CloseCurlyBracket),
                    _ => None
                };
                tokens.push(token);
                if let Some(closing_token) = closing_token {
                    flatten(&mut input.parse_nested_block(), tokens);
                    tokens.push(closing_token);
                }
            }
        }
        let mut tokens = vec![];
        flatten(input, &mut tokens);
        let serialized = tokens.to_css_string();
        Json::List(Parser::parse_str(serialized.as_slice(), component_values_to_json))
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
        while let Ok(mut token) = input.next_including_whitespace() {
            if token == Token::Delim('!') {
                input.push_back(token);
                match parse_important(input) {
                    Ok(Priority::Important) => {
                        if input.is_exhausted() {
                            important = true;
                            break
                        }
                        // Hack to deal with css-parsing-tests assuming that
                        // `!important` in the middle of a declaration value is OK.
                        // This can never happen per spec
                        // (even CSS Variables forbid top-level `!`)
                        value.push("!".to_json());
                        token = Token::Ident("important".into_string());
                    }
                    // More hacks
                    Ok(Priority::Normal) => {
                        token = input.next_including_whitespace().unwrap();
                        assert!(token == Token::Delim('!'));
                    }
                    Err(()) => token = Token::Delim('!')
                }
            }
            value.push(one_component_value_to_json(token, input));
        }
        Ok(JList![
            "declaration",
            name,
            value,
            important,
        ])
    }
}

impl AtRuleParser<Vec<Json>, Json> for JsonParser {
    fn parse_prelude(&mut self, name: &str, input: &mut Parser)
                     -> Result<AtRulePrelude<Vec<Json>, Json>, ()> {
        Ok(AtRulePrelude::OptionalBlock(vec![
            "at-rule".to_json(),
            name.to_json(),
            Json::List(component_values_to_json(input)),
        ]))
    }

    fn parse_block(&mut self, mut prelude: Vec<Json>, input: &mut Parser) -> Result<Json, ()> {
        prelude.push(Json::List(component_values_to_json(input)));
        Ok(Json::List(prelude))
    }

    fn rule_without_block(&mut self, mut prelude: Vec<Json>) -> Result<Json, ()> {
        prelude.push(Json::Null);
        Ok(Json::List(prelude))
    }
}

impl QualifiedRuleParser<Vec<Json>, Json> for JsonParser {
    fn parse_prelude(&mut self, input: &mut Parser) -> Result<Vec<Json>, ()> {
        Ok(component_values_to_json(input))
    }

    fn parse_block(&mut self, prelude: Vec<Json>, input: &mut Parser) -> Result<Json, ()> {
        Ok(JList![
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
        match value {
            NumericValue{representation: r, value: v, int_value: i}
            => vec![r.to_json(), v.to_json(),
                    match i { Some(_) => "integer", None => "number" }.to_json()]
        }
    }

    match token {
        Token::Ident(value) => JList!["ident", value],
        Token::AtKeyword(value) => JList!["at-keyword", value],
        Token::Hash(value) => JList!["hash", value, "unrestricted"],
        Token::IDHash(value) => JList!["hash", value, "id"],
        Token::QuotedString(value) => JList!["string", value],
        Token::Url(value) => JList!["url", value],
        Token::Delim('\\') => "\\".to_json(),
        Token::Delim(value) => String::from_char(1, value).to_json(),

        Token::Number(value) => json::List(vec!["number".to_json()] + numeric(value)),
        Token::Percentage(value) => json::List(
            vec!["percentage".to_json()] + numeric(value)),
        Token::Dimension(value, unit) => json::List(
            vec!["dimension".to_json()] + numeric(value) + [unit.to_json()].as_slice()),

        Token::UnicodeRange(start, end) => JList!["unicode-range", start, end],

        Token::WhiteSpace => " ".to_json(),
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

        Token::Function(name) => {
            json::List(vec!["function".to_json(), name.to_json()] +
                       component_values_to_json(&mut input.parse_nested_block()))
        }
        Token::ParenthesisBlock => {
            json::List(vec!["()".to_json()] +
                       component_values_to_json(&mut input.parse_nested_block()))
        }
        Token::SquareBracketBlock => {
            json::List(vec!["[]".to_json()] +
                       component_values_to_json(&mut input.parse_nested_block()))
        }
        Token::CurlyBracketBlock => {
            json::List(vec!["{}".to_json()] +
                       component_values_to_json(&mut input.parse_nested_block()))
        }
        Token::BadUrl => JList!["error", "bad-url"],
        Token::BadString => JList!["error", "bad-string"],
        Token::CloseParenthesis => JList!["error", ")"],
        Token::CloseSquareBracket => JList!["error", "]"],
        Token::CloseCurlyBracket => JList!["error", "}"],
    }
}
