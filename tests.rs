/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use std::{io, str, task};
use std::io::{File, Process, Writer, TempDir};
use serialize::{json};
use serialize::json::ToJson;
use test;

use encoding::label::encoding_from_whatwg_label;

use super::*;
use ast::*;


fn write_whole_file(path: &Path, data: &str) {
    match File::open_mode(path, io::Open, io::Write) {
        Ok(mut writer) => { writer.write(data.as_bytes()).unwrap(); },
        _ => fail!("could not open file"),
    }
}


fn almost_equals(a: &json::Json, b: &json::Json) -> bool {
    match (a, b) {
        (&json::Number(a), &json::Number(b)) => (a - b).abs() < 1e-6,
        (&json::String(ref a), &json::String(ref b)) => a == b,
        (&json::Boolean(a), &json::Boolean(b)) => a == b,
        (&json::List(ref a), &json::List(ref b))
            => a.iter().zip(b.iter()).all(|(ref a, ref b)| almost_equals(*a, *b)),
        (&json::Object(_), &json::Object(_)) => fail!(~"Not implemented"),
        (&json::Null, &json::Null) => true,
        _ => false,
    }
}


fn assert_json_eq(results: json::Json, expected: json::Json, message: ~str) {
    if !almost_equals(&results, &expected) {
        let temp = TempDir::new("rust-cssparser-tests").unwrap();
        let results = results.to_pretty_str() + "\n";
        let expected = expected.to_pretty_str() + "\n";
        task::try(proc() {
            let mut result_path = temp.path().clone();
            result_path.push("results.json");
            let mut expected_path = temp.path().clone();
            expected_path.push("expected.json");
            write_whole_file(&result_path, results);
            write_whole_file(&expected_path, expected);
            Process::status(
                "colordiff",
                [~"-u1000", result_path.display().to_str(), expected_path.display().to_str()]
            ).unwrap();
        }).unwrap();

        fail!(message)
    }
}


fn run_raw_json_tests(json_data: &str, run: |json::Json, json::Json|) {
    let items = match json::from_str(json_data) {
        Ok(json::List(items)) => items,
        _ => fail!("Invalid JSON")
    };
    assert!(items.len() % 2 == 0);
    let mut input = None;
    for item in items.move_iter() {
        match (&input, item) {
            (&None, json_obj) => input = Some(json_obj),
            (&Some(_), expected) => {
                let input = input.take_unwrap();
                run(input, expected)
            },
        };
    }
}


fn run_json_tests<T: ToJson>(json_data: &str, parse: |input: ~str| -> T) {
    run_raw_json_tests(json_data, |input, expected| {
        match input {
            json::String(input) => {
                let result = parse(input.to_owned()).to_json();
                assert_json_eq(result, expected, input);
            },
            _ => fail!("Unexpected JSON")
        }
    });
}


#[test]
fn component_value_list() {
    run_json_tests(include_str!("css-parsing-tests/component_value_list.json"), |input| {
        tokenize(input).map(|(c, _)| c).collect::<~[ComponentValue]>()
    });
}


#[test]
fn one_component_value() {
    run_json_tests(include_str!("css-parsing-tests/one_component_value.json"), |input| {
        parse_one_component_value(tokenize(input))
    });
}


#[test]
fn declaration_list() {
    run_json_tests(include_str!("css-parsing-tests/declaration_list.json"), |input| {
        parse_declaration_list(tokenize(input)).collect::<~[Result<DeclarationListItem, SyntaxError>]>()
    });
}


#[test]
fn one_declaration() {
    run_json_tests(include_str!("css-parsing-tests/one_declaration.json"), |input| {
        parse_one_declaration(tokenize(input))
    });
}


#[test]
fn rule_list() {
    run_json_tests(include_str!("css-parsing-tests/rule_list.json"), |input| {
        parse_rule_list(tokenize(input)).collect::<~[Result<Rule, SyntaxError>]>()
    });
}


#[test]
fn stylesheet() {
    run_json_tests(include_str!("css-parsing-tests/stylesheet.json"), |input| {
        parse_stylesheet_rules(tokenize(input)).collect::<~[Result<Rule, SyntaxError>]>()
    });
}


#[test]
fn one_rule() {
    run_json_tests(include_str!("css-parsing-tests/one_rule.json"), |input| {
        parse_one_rule(tokenize(input))
    });
}


#[test]
fn stylesheet_from_bytes() {
    run_raw_json_tests(include_str!("css-parsing-tests/stylesheet_bytes.json"),
    |input, expected| {
        let map = match input {
            json::Object(map) => map,
            _ => fail!("Unexpected JSON")
        };

        let result = {
            let css = get_string(map, &~"css_bytes").unwrap().chars().map(|c| {
                assert!(c as u32 <= 0xFF);
                c as u8
            }).collect::<~[u8]>();
            let protocol_encoding_label = get_string(map, &~"protocol_encoding");
            let environment_encoding = get_string(map, &~"environment_encoding")
                .and_then(encoding_from_whatwg_label);

            let (mut rules, used_encoding) = parse_stylesheet_rules_from_bytes(
                css, protocol_encoding_label, environment_encoding);

            (rules.collect::<~[Result<Rule, SyntaxError>]>(), used_encoding.name().to_owned()).to_json()
        };
        assert_json_eq(result, expected, json::Object(map).to_str());
    });

    fn get_string<'a>(map: &'a json::Object, key: &~str) -> Option<&'a str> {
        match map.find(key) {
            Some(&json::String(ref s)) => Some(s.as_slice()),
            Some(&json::Null) => None,
            None => None,
            _ => fail!("Unexpected JSON"),
        }
    }
}


fn run_color_tests(json_data: &str, to_json: |result: Option<Color>| -> json::Json) {
    run_json_tests(json_data, |input| {
        match parse_one_component_value(tokenize(input)) {
            Ok(component_value) => to_json(Color::parse(&component_value)),
            Err(_reason) => json::Null,
        }
    });
}


#[test]
fn color3() {
    run_color_tests(include_str!("css-parsing-tests/color3.json"), |c| c.to_json())
}


#[test]
fn color3_hsl() {
    run_color_tests(include_str!("css-parsing-tests/color3_hsl.json"), |c| c.to_json())
}


/// color3_keywords.json is different: R, G and B are in 0..255 rather than 0..1
#[test]
fn color3_keywords() {
    run_color_tests(include_str!("css-parsing-tests/color3_keywords.json"), |c| {
        match c {
            Some(RGBA(RGBA { red: r, green: g, blue: b, alpha: a }))
            => (~[r * 255., g * 255., b * 255., a]).to_json(),
            Some(CurrentColor) => json::String(~"currentColor"),
            None => json::Null,
        }
    });
}


#[bench]
fn bench_color_lookup_red(b: &mut test::BenchHarness) {
    let ident = parse_one_component_value(tokenize("red")).unwrap();
    b.iter(|| assert!(Color::parse(&ident).is_some()));
}


#[bench]
fn bench_color_lookup_lightgoldenrodyellow(b: &mut test::BenchHarness) {
    let ident = parse_one_component_value(tokenize("lightgoldenrodyellow")).unwrap();
    b.iter(|| assert!(Color::parse(&ident).is_some()));
}


#[bench]
fn bench_color_lookup_fail(b: &mut test::BenchHarness) {
    let ident = parse_one_component_value(tokenize("lightgoldenrodyellowbazinga")).unwrap();
    b.iter(|| assert!(Color::parse(&ident).is_none()));
}


#[test]
fn nth() {
    run_json_tests(include_str!("css-parsing-tests/An+B.json"), |input| {
        parse_nth(tokenize(input).map(|(c, _)| c).collect::<~[ComponentValue]>())
    });
}


#[test]
fn serializer() {
    run_json_tests(include_str!("css-parsing-tests/component_value_list.json"), |input| {
        let component_values = tokenize(input).map(|(c, _)| c).collect::<~[ComponentValue]>();
        let serialized = component_values.iter().to_css();
        tokenize(serialized).map(|(c, _)| c).collect::<~[ComponentValue]>()
    });
}


impl ToJson for Result<Rule, SyntaxError> {
    fn to_json(&self) -> json::Json {
        match *self {
            Ok(ref a) => a.to_json(),
            Err(ref b) => b.to_json(),
        }
    }
}


impl ToJson for Result<DeclarationListItem, SyntaxError> {
    fn to_json(&self) -> json::Json {
        match *self {
            Ok(ref a) => a.to_json(),
            Err(ref b) => b.to_json(),
        }
    }
}


impl ToJson for Result<Declaration, SyntaxError> {
    fn to_json(&self) -> json::Json {
        match *self {
            Ok(ref a) => a.to_json(),
            Err(ref b) => b.to_json(),
        }
    }
}


impl ToJson for Result<ComponentValue, SyntaxError> {
    fn to_json(&self) -> json::Json {
        match *self {
            Ok(ref a) => a.to_json(),
            Err(ref b) => b.to_json(),
        }
    }
}


impl ToJson for SyntaxError {
    fn to_json(&self) -> json::Json {
        json::List(~[json::String(~"error"), json::String(match self.reason {
            ErrEmptyInput => ~"empty",
            ErrExtraInput => ~"extra-input",
            _ => ~"invalid",
        })])
    }
}


impl ToJson for Color {
    fn to_json(&self) -> json::Json {
        match *self {
            RGBA(RGBA { red: r, green: g, blue: b, alpha: a }) => (~[r, g, b, a]).to_json(),
            CurrentColor => json::String(~"currentColor"),
        }
    }
}


impl ToJson for Rule {
    fn to_json(&self) -> json::Json {
        match *self {
            QualifiedRule(ref rule) => rule.to_json(),
            AtRule(ref rule) => rule.to_json(),
        }
    }
}


impl ToJson for DeclarationListItem {
    fn to_json(&self) -> json::Json {
        match *self {
            Declaration(ref declaration) => declaration.to_json(),
            DeclAtRule(ref at_rule) => at_rule.to_json(),
        }
    }
}


fn list_to_json(list: &~[(ComponentValue, SourceLocation)]) -> ~[json::Json] {
    list.iter().map(|tuple| {
        match *tuple {
            (ref c, _) => c.to_json()
        }
    }).collect()
}


impl ToJson for AtRule {
    fn to_json(&self) -> json::Json {
        match *self {
            AtRule{name: ref name, prelude: ref prelude, block: ref block, ..}
            => json::List(~[json::String(~"at-rule"), name.to_json(),
                            prelude.to_json(), block.as_ref().map(list_to_json).to_json()])
        }
    }
}


impl ToJson for QualifiedRule {
    fn to_json(&self) -> json::Json {
        match *self {
            QualifiedRule{prelude: ref prelude, block: ref block, ..}
            => json::List(~[json::String(~"qualified rule"),
                            prelude.to_json(), json::List(list_to_json(block))])
        }
    }
}


impl ToJson for Declaration {
    fn to_json(&self) -> json::Json {
        match *self {
            Declaration{name: ref name, value: ref value, important: ref important, ..}
            =>  json::List(~[json::String(~"declaration"), name.to_json(),
                             value.to_json(), important.to_json()])
        }
    }
}


impl ToJson for ComponentValue {
    fn to_json(&self) -> json::Json {
        use JList = serialize::json::List;
        use JString = serialize::json::String;

        fn numeric(value: &NumericValue) -> ~[json::Json] {
            match *value {
                NumericValue{representation: ref r, value: ref v, int_value: ref i}
                => ~[r.to_json(), v.to_json(),
                     JString(match *i { Some(_) => ~"integer", _ => ~"number" })]
            }
        }

        match *self {
            Ident(ref value) => JList(~[JString(~"ident"), value.to_json()]),
            AtKeyword(ref value) => JList(~[JString(~"at-keyword"), value.to_json()]),
            Hash(ref value) => JList(~[JString(~"hash"), value.to_json(),
                                       JString(~"unrestricted")]),
            IDHash(ref value) => JList(~[JString(~"hash"), value.to_json(), JString(~"id")]),
            String(ref value) => JList(~[JString(~"string"), value.to_json()]),
            URL(ref value) => JList(~[JString(~"url"), value.to_json()]),
            Delim('\\') => JString(~"\\"),
            Delim(value) => JString(str::from_char(value)),

            Number(ref value) => JList(~[JString(~"number")] + numeric(value)),
            Percentage(ref value) => JList(~[JString(~"percentage")] + numeric(value)),
            Dimension(ref value, ref unit)
            => JList(~[JString(~"dimension")] + numeric(value) + ~[unit.to_json()]),

            UnicodeRange(start, end)
            => JList(~[JString(~"unicode-range"), start.to_json(), end.to_json()]),

            WhiteSpace => JString(~" "),
            Colon => JString(~":"),
            Semicolon => JString(~";"),
            Comma => JString(~","),
            IncludeMatch => JString(~"~="),
            DashMatch => JString(~"|="),
            PrefixMatch => JString(~"^="),
            SuffixMatch => JString(~"$="),
            SubstringMatch => JString(~"*="),
            Column => JString(~"||"),
            CDO => JString(~"<!--"),
            CDC => JString(~"-->"),

            Function(ref name, ref arguments)
            => JList(~[JString(~"function"), name.to_json()] +
                     arguments.iter().map(|a| a.to_json()).collect::<~[json::Json]>()),
            ParenthesisBlock(ref content)
            => JList(~[JString(~"()")] + content.iter().map(|c| c.to_json()).collect::<~[json::Json]>()),
            SquareBracketBlock(ref content)
            => JList(~[JString(~"[]")] + content.iter().map(|c| c.to_json()).collect::<~[json::Json]>()),
            CurlyBracketBlock(ref content)
            => JList(~[JString(~"{}")] + list_to_json(content)),

            BadURL => JList(~[JString(~"error"), JString(~"bad-url")]),
            BadString => JList(~[JString(~"error"), JString(~"bad-string")]),
            CloseParenthesis => JList(~[JString(~"error"), JString(~")")]),
            CloseSquareBracket => JList(~[JString(~"error"), JString(~"]")]),
            CloseCurlyBracket => JList(~[JString(~"error"), JString(~"}")]),
        }
    }
}
