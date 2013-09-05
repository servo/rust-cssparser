/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use std::{io, os, str, run, task};
use extra::{tempfile, json};
use extra::json::ToJson;

use super::*;


fn write_whole_file(path: &Path, data: &str) {
    match io::file_writer(path, [io::Create]) {
        Ok(writer) => writer.write_str(data),
        Err(message) => fail!(message),
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
        let temp = tempfile::mkdtemp(&os::tmpdir(), "rust-cssparser-tests").unwrap();
        let temp_ = temp.clone();
        let results = results.to_pretty_str() + "\n";
        let expected = expected.to_pretty_str() + "\n";
        do task::try {
            let result_path = temp.push("results.json");
            let expected_path = temp.push("expected.json");
            write_whole_file(&result_path, results);
            write_whole_file(&expected_path, expected);
            run::process_status("colordiff", [~"-u1000", result_path.to_str(),
                                              expected_path.to_str()]);
        };
        os::remove_dir_recursive(&temp_);

        fail!(message)
    }
}


fn run_json_tests<T: ToJson>(json_data: &str, parse: &fn (input: ~str) -> T) {
    let items = match json::from_str(json_data) {
        Ok(json::List(items)) => items,
        _ => fail!("Invalid JSON")
    };
    assert!(items.len() % 2 == 0);
    let mut input: Option<~str> = None;
    for item in items.move_iter() {
        match (&input, item) {
            (&None, json::String(string)) => input = Some(string),
            (&Some(_), expected) => {
                let input = input.take_unwrap();
                let result = parse(input.to_owned()).to_json();
                assert_json_eq(result, expected, input);
            },
            _ => fail!("Unexpected JSON")
        };
    }
}


#[test]
fn component_value_list() {
    do run_json_tests(include_str!("css-parsing-tests/component_value_list.json")) |input| {
        tokenize(input).map(|(c, _)| c).to_owned_vec()
    }
}


#[test]
fn one_component_value() {
    do run_json_tests(include_str!("css-parsing-tests/one_component_value.json")) |input| {
        parse_one_component_value(tokenize(input))
    }
}


#[test]
fn declaration_list() {
    do run_json_tests(include_str!("css-parsing-tests/declaration_list.json")) |input| {
        parse_declaration_list(tokenize(input)).to_owned_vec()
    }
}


#[test]
fn one_declaration() {
    do run_json_tests(include_str!("css-parsing-tests/one_declaration.json")) |input| {
        parse_one_declaration(tokenize(input))
    }
}


#[test]
fn rule_list() {
    do run_json_tests(include_str!("css-parsing-tests/rule_list.json")) |input| {
        parse_rule_list(tokenize(input)).to_owned_vec()
    }
}


#[test]
fn stylesheet() {
    do run_json_tests(include_str!("css-parsing-tests/stylesheet.json")) |input| {
        parse_stylesheet_rules(tokenize(input)).to_owned_vec()
    }
}


#[test]
fn one_rule() {
    do run_json_tests(include_str!("css-parsing-tests/one_rule.json")) |input| {
        parse_one_rule(tokenize(input))
    }
}


fn run_color_tests(json_data: &str, to_json: &fn(result: Option<Color>) -> json::Json) {
    do run_json_tests(json_data) |input| {
        match parse_one_component_value(tokenize(input)) {
            Ok(component_value) => to_json(Color::parse(&component_value)),
            Err(_reason) => json::Null,
        }
    }
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
    do run_color_tests(include_str!("css-parsing-tests/color3_keywords.json")) |c| {
        match c {
            Some(RGBA(RGBA { red: r, green: g, blue: b, alpha: a }))
            => (~[r * 255., g * 255., b * 255., a]).to_json(),
            Some(CurrentColor) => json::String(~"currentColor"),
            None => json::Null,
        }
    }
}


#[test]
fn nth() {
    do run_json_tests(include_str!("css-parsing-tests/An+B.json")) |input| {
        parse_nth(tokenize(input).map(|(c, _)| c).to_owned_vec())
    }
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
            Decl_AtRule(ref at_rule) => at_rule.to_json(),
        }
    }
}


fn list_to_json(list: &~[(ComponentValue, SourceLocation)]) -> ~[json::Json] {
    list.map(|tuple| {
        match *tuple {
            (ref c, _) => c.to_json()
        }
    })
}


impl ToJson for AtRule {
    fn to_json(&self) -> json::Json {
        match *self {
            AtRule{name: ref name, prelude: ref prelude, block: ref block, _}
            => json::List(~[json::String(~"at-rule"), name.to_json(),
                            prelude.to_json(), block.map(list_to_json).to_json()])
        }
    }
}


impl ToJson for QualifiedRule {
    fn to_json(&self) -> json::Json {
        match *self {
            QualifiedRule{prelude: ref prelude, block: ref block, _}
            => json::List(~[json::String(~"qualified rule"),
                            prelude.to_json(), json::List(list_to_json(block))])
        }
    }
}


impl ToJson for Declaration {
    fn to_json(&self) -> json::Json {
        match *self {
            Declaration{name: ref name, value: ref value, important: ref important, _}
            =>  json::List(~[json::String(~"declaration"), name.to_json(),
                             value.to_json(), important.to_json()])
        }
    }
}


impl ToJson for ComponentValue {
    fn to_json(&self) -> json::Json {
        use JList = extra::json::List;
        use JString = extra::json::String;

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

            UnicodeRange { start: s, end: e }
            => JList(~[JString(~"unicode-range"), s.to_json(), e.to_json()]),

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
            => JList(~[JString(~"function"), name.to_json()] + arguments.map(|a| a.to_json())),
            ParenthesisBlock(ref content)
            => JList(~[JString(~"()")] + content.map(|c| c.to_json())),
            SquareBracketBlock(ref content)
            => JList(~[JString(~"[]")] + content.map(|c| c.to_json())),
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
