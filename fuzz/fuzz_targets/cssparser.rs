#![no_main]

use cssparser::*;

const DEBUG: bool = false;

fn parse_and_serialize(input: &str, preserving_comments: bool) -> String {
    let mut input = ParserInput::new(input);
    let mut parser = Parser::new(&mut input);
    let mut serialization = String::new();
    let result = do_parse_and_serialize(
        &mut parser,
        preserving_comments,
        TokenSerializationType::nothing(),
        &mut serialization,
        0,
    );
    if result.is_err() {
        return String::new();
    }
    serialization
}

fn do_parse_and_serialize<'i>(
    input: &mut Parser<'i, '_>,
    preserving_comments: bool,
    mut previous_token_type: TokenSerializationType,
    serialization: &mut String,
    indent_level: usize,
) -> Result<(), ParseError<'i, ()>> {
    loop {
        let token = if preserving_comments {
            input.next_including_whitespace_and_comments()
        } else {
            input.next_including_whitespace()
        };
        let token = match token {
            Ok(token) => token,
            Err(..) => break,
        };
        if DEBUG {
            for _ in 0..indent_level {
                print!(" ");
            }
            println!("{:?}", token);
        }
        if token.is_parse_error() {
            let token = token.clone();
            return Err(input.new_unexpected_token_error(token))
        }
        let token_type = token.serialization_type();
        if previous_token_type.needs_separator_when_before(token_type) {
            serialization.push_str("/**/");
        }
        previous_token_type = token_type;
        token.to_css(serialization).unwrap();
        let closing_token = match token {
            Token::Function(_) | Token::ParenthesisBlock => Token::CloseParenthesis,
            Token::SquareBracketBlock => Token::CloseSquareBracket,
            Token::CurlyBracketBlock => Token::CloseCurlyBracket,
            _ => continue,
        };

        input.parse_nested_block(|input| -> Result<_, ParseError<()>> {
            do_parse_and_serialize(input, preserving_comments, previous_token_type, serialization, indent_level + 1)
        })?;

        closing_token.to_css(serialization).unwrap();
    }
    Ok(())
}

fn fuzz(data: &str, preserving_comments: bool) {
    let serialization = parse_and_serialize(data, preserving_comments);
    let reserialization = parse_and_serialize(&serialization, preserving_comments);
    if DEBUG {
        println!("IN: {:?}", serialization);
        println!("OUT: {:?}", reserialization);
    }
    // TODO: This should ideally pass, but it doesn't for numbers near our
    // precision limits, so parsing e.g., 9999995e-45 generates a serialization
    // of 10e-39 because of dtoa rounding, and parsing _that_ generates a
    // serialization of 1e-38.
    //
    // assert_eq!(
    //     serialization, reserialization,
    //     "Serialization should be idempotent"
    // );
}

libfuzzer_sys::fuzz_target!(|data: &str| {
    fuzz(data, false);
    // TODO(emilio): Serialization when preserving comments is not idempotent.
    // But in browsers we never preserve comments so that's ok...
    // fuzz(data, true);
});
