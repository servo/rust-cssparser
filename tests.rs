use std::{io, os, str, vec, run, task};
use extra::{tempfile, json};
use extra::json::ToJson;

use ast::*;
use tokenizer::*;
use parser::*;


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


fn run_json_tests(json_data: &str, parse: &fn (input: ~str) -> json::Json)
                      -> bool {
    let items = match json::from_str(json_data) {
        Ok(json::List(items)) => items,
        _ => fail!("Invalid JSON")
    };
    assert!(items.len() % 2 == 0);
    let mut input: Option<~str> = None;
    do vec::consume(items) |_, item| {
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
    true
}


#[test]
fn component_value_list() {
    do run_json_tests(include_str!(
            // https://github.com/SimonSapin/tinycss2/tree/master/tinycss2/tests
            // TODO: use git subtree or something to have the JSON files in this repository.
            "../tinycss2/tinycss2/tests/component_value_list.json"
    )) |input| {
        let mut parser = Parser::from_str(input);
        let mut results = ~[];
        loop {
            match next_component_value(&mut parser) {
                Some(c) => results.push(c),
                None => break,
            }
        }
        results.to_json()
    };
}


#[test]
fn one_declaration() {
    do run_json_tests(include_str!(
            // https://github.com/SimonSapin/tinycss2/tree/master/tinycss2/tests
            // TODO: use git subtree or something to have the JSON files in this repository.
            "../tinycss2/tinycss2/tests/one_declaration.json"
    )) |input| {
        match parse_one_declaration(&mut ComponentValueIterator::from_str(input)) {
            Ok(declaration) => declaration.to_json(),
            Err(_) => json::List(~[json::String(~"error"), json::String(~"invalid")]),
        }
    };
}


impl ToJson for Declaration {
    fn to_json(&self) -> json::Json {
        let Declaration{name: name, value: value, important: important} = copy *self;
        json::List(~[json::String(~"declaration"), json::String(name),
                     value.to_json(), json::Boolean(important)])
    }
}


impl ToJson for ComponentValue {
    fn to_json(&self) -> json::Json {
        use JList = extra::json::List;
        use JString = extra::json::String;
        use JNumber = extra::json::Number;

        fn numeric(NumericValue{representation: r, value: v, int_value: i}: NumericValue)
                   -> ~[json::Json] {
            ~[JString(r), JNumber(v as float), JString(
                match i { Some(_) => ~"integer", _ => ~"number" })]
        }

        match copy *self {
            Ident(value) => JList(~[JString(~"ident"), JString(value)]),
            AtKeyword(value) => JList(~[JString(~"at-keyword"), JString(value)]),
            Hash(value) => JList(~[JString(~"hash"), JString(value), JString(~"unrestricted")]),
            IDHash(value) => JList(~[JString(~"hash"), JString(value), JString(~"id")]),
            String(value) => JList(~[JString(~"string"), JString(value)]),
            URL(value) => JList(~[JString(~"url"), JString(value)]),
            Delim('\\') => JString(~"\\"),
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
            => JList(~[JString(~"function"), JString(name)] + vec::map(arguments, |a| a.to_json())),
            ParenthesisBlock(content)
            => JList(~[JString(~"()")] + vec::map(content, |v| v.to_json())),
            SquareBraketBlock(content)
            => JList(~[JString(~"[]")] + vec::map(content, |v| v.to_json())),
            CurlyBraketBlock(content)
            => JList(~[JString(~"{}")] + vec::map(content, |v| v.to_json())),

            BadURL => JList(~[JString(~"error"), JString(~"bad-url")]),
            BadString => JList(~[JString(~"error"), JString(~"bad-string")]),
            CloseParenthesis => JList(~[JString(~"error"), JString(~")")]),
            CloseSquareBraket => JList(~[JString(~"error"), JString(~"]")]),
            CloseCurlyBraket => JList(~[JString(~"error"), JString(~"}")]),
        }
    }
}
