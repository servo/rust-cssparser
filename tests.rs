/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use std::{io, os, str, run, task};
use extra::{tempfile, json};
use extra::json::ToJson;

use ast::*;
use tokenizer::*;
use parser::*;
use color::*;


fn write_whole_file(path: &Path, data: &str) {
    match io::file_writer(path, [io::Create]) {
        Ok(writer) => writer.write_str(data),
        Err(message) => fail!(message),
    }
}


fn assert_json_eq(results: json::Json, expected: json::Json, message: ~str) {
    if results != expected {
        let temp = tempfile::mkdtemp(&os::tmpdir(), "rust-cssparser-tests").get();
        let temp_ = copy temp;
        let results = json::to_pretty_str(&results) + "\n";
        let expected = json::to_pretty_str(&expected) + "\n";
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


fn run_json_tests(json_data: &str, parse: &fn (input: ~str) -> json::Json) {
    let items = match json::from_str(json_data) {
        Ok(json::List(items)) => items,
        _ => fail!("Invalid JSON")
    };
    assert!(items.len() % 2 == 0);
    let mut input: Option<~str> = None;
    for items.consume_iter().advance |item| {
        match (&input, item) {
            (&None, json::String(string)) => input = Some(string),
            (&Some(_), expected) => {
                let input = input.swap_unwrap();
                let result = parse(input.to_owned());
                assert_json_eq(result, expected, input);
            },
            _ => fail!("Unexpected JSON")
        };
    }
}


#[test]
fn component_value_list() {
    do run_json_tests(include_str!("css-parsing-tests/component_value_list.json")) |input| {
        let parser = &mut Parser::from_str(input);
        let mut results = ~[];
        loop {
            match next_component_value(parser) {
                Some((c, _)) => results.push(c),
                None => break,
            }
        }
        results.to_json()
    }
}


#[test]
fn one_component_value() {
    do run_json_tests(include_str!("css-parsing-tests/one_component_value.json")) |input| {
        let iter = &mut ComponentValueIterator::from_str(input);
        result_to_json(parse_one_component_value(iter).chain(|(c, _)| Ok(c)))
    }
}


#[test]
fn declaration_list() {
    do run_json_tests(include_str!("css-parsing-tests/declaration_list.json")) |input| {
        let iter = &mut ComponentValueIterator::from_str(input);
        let mut declarations = ~[];
        loop {
            match parse_declaration_or_at_rule(iter) {
                None => break,
                Some(result) => declarations.push(result_to_json(result)),
            }
        }
        json::List(declarations)
    }
}


#[test]
fn one_declaration() {
    do run_json_tests(include_str!("css-parsing-tests/one_declaration.json")) |input| {
        result_to_json(parse_one_declaration(&mut ComponentValueIterator::from_str(input)))
    }
}


#[test]
fn rule_list() {
    do run_json_tests(include_str!("css-parsing-tests/rule_list.json")) |input| {
        let iter = &mut ComponentValueIterator::from_str(input);
        let mut rules = ~[];
        loop {
            match parse_rule(iter) {
                None => break,
                Some(result) => rules.push(result_to_json(result)),
            }
        }
        json::List(rules)
    }
}


#[test]
fn one_rule() {
    do run_json_tests(include_str!("css-parsing-tests/one_rule.json")) |input| {
        result_to_json(parse_one_rule(&mut ComponentValueIterator::from_str(input)))
    }
}


fn run_color_tests(json_data: &str, to_json: &fn(result: Option<Color>) -> json::Json) {
    do run_json_tests(json_data) |input| {
        match parse_one_component_value(&mut ComponentValueIterator::from_str(input)) {
            Ok((component_value, _location)) => to_json(parse_color(component_value)),
            Err(_reason) => json::Null,
        }
    }
}


//#[test]
//fn color3() {
//    run_color_tests(include_str!("css-parsing-tests/color3.json"), |c| c.to_json())
//}


//#[test]
//fn color3_hsl() {
//    run_color_tests(include_str!("css-parsing-tests/color3_hsl.json"), |c| c.to_json())
//}


#[test]
fn color3_keywords() {
    do run_color_tests(include_str!("css-parsing-tests/color3_keywords.json")) |c| {
        let m = 255 as ColorFloat;
        match c {
            Some(RGBA(r, g, b, a)) => (~[r * m, g * m, b * m, a]).to_json(),
            Some(CurrentColor) => json::String(~"currentColor"),
            None => json::Null,
        }
    }
}


fn result_to_json<A:ToJson, B:ToJson>(result: Result<A, B>) -> json::Json {
    match result {
        Ok(ref a) => a.to_json(),
        Err(ref b) => b.to_json(),
    }
}


impl ToJson for Color {
    fn to_json(&self) -> json::Json {
        match *self {
            RGBA(r, g, b, a) => (~[r, g, b, a]).to_json(),
            CurrentColor => json::String(~"currentColor"),
        }
    }
}


impl ToJson for ErrorReason {
    fn to_json(&self) -> json::Json {
        json::List(~[json::String(~"error"), json::String(match *self {
            ErrEmptyInput => ~"empty",
            ErrExtraInput => ~"extra-input",
            _ => ~"invalid",
        })])
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
                            json::List(list_to_json(prelude)),
                            block.map(list_to_json).to_json()])
        }
    }
}


impl ToJson for QualifiedRule {
    fn to_json(&self) -> json::Json {
        match *self {
            QualifiedRule{prelude: ref prelude, block: ref block, _}
            => json::List(~[json::String(~"qualified rule"),
                            json::List(list_to_json(prelude)), json::List(list_to_json(block))])
        }
    }
}


impl ToJson for Declaration {
    fn to_json(&self) -> json::Json {
        match *self {
            Declaration{name: ref name, value: ref value, important: ref important, _}
            =>  json::List(~[json::String(~"declaration"), name.to_json(),
                             json::List(list_to_json(value)), important.to_json()])
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

            Function(ref name, ref arguments)
            => JList(~[JString(~"function"), name.to_json()] + list_to_json(arguments)),
            ParenthesisBlock(ref content)
            => JList(~[JString(~"()")] + list_to_json(content)),
            SquareBraketBlock(ref content)
            => JList(~[JString(~"[]")] + list_to_json(content)),
            CurlyBraketBlock(ref content)
            => JList(~[JString(~"{}")] + list_to_json(content)),

            BadURL => JList(~[JString(~"error"), JString(~"bad-url")]),
            BadString => JList(~[JString(~"error"), JString(~"bad-string")]),
            CloseParenthesis => JList(~[JString(~"error"), JString(~")")]),
            CloseSquareBraket => JList(~[JString(~"error"), JString(~"]")]),
            CloseCurlyBraket => JList(~[JString(~"error"), JString(~"}")]),
        }
    }
}
