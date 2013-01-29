// http://dev.w3.org/csswg/css3-syntax/#tree-construction
//
// The input to the tree construction stage is a sequence of tokens
// from the tokenization stage.
// The output is a tree of items with a stylesheet at the root
// and all other nodes being at-rules, style rules, or declarations.


use tokens;


#[deriving_eq]
pub enum Primitive {
    // Preserved tokens. Same as in the tokenizer.
    Ident(~str),
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
    UnicodeRange(char, char),  // start, end
    EmptyUnicodeRange,
    WhiteSpace,
    CDO,  // <!--
    CDC,  // -->
    Colon,  // :
    Semicolon,  // ;
    CloseParenthesis, // )
    CloseSquareBraket, // ]
    CloseCurlyBraket, // }

    // Function
    Function(~str, ~[~[Primitive]]),  // name, arguments

    // Simple block
    ParenthesisBlock(~[Primitive]),  // (…)
    SquareBraketBlock(~[Primitive]),  // […]
    CurlyBraketBlock(~[Primitive]),  // {…}
}


#[deriving_eq]
pub struct Declaration {
    name: ~str,
    value: ~[Primitive],
    important: bool,
}

#[deriving_eq]
pub struct StyleRule {
    selector: ~[Primitive],
    value: ~[DeclarationBlockItem],
}

#[deriving_eq]
pub struct AtRule {
    name: ~str,
    selector: ~[Primitive],
    value: AtRuleValue,
}


#[deriving_eq]
pub enum AtRuleValue {
    NotFilled,  // @foo…;
    DeclarationFilled(~[DeclarationBlockItem]),
    RuleFilled(~[Rule]),
}


#[deriving_eq]
pub enum DeclarationBlockItem {
    Declaration(Declaration),
    // A better idea for a name that means "at-rule" but is not "AtRule"?
    Decl_AtRule(AtRule),
}

#[deriving_eq]
pub enum Rule {
    StyleRule(StyleRule),
    AtRule(AtRule),
}


pub struct Parser {
    priv tokenizer: ~tokens::Tokenizer,
    priv quirks_mode: bool,
    priv mut current_token: Option<tokens::Token>,
    priv mut errors: ~[~str],
}


impl Parser {
    static fn from_tokenizer(
            tokenizer: ~tokens::Tokenizer, quirks_mode: bool) -> ~Parser {
        ~Parser {
            tokenizer: tokenizer,
            quirks_mode: quirks_mode,
            current_token: None,
            errors: ~[],
        }
    }
    static fn from_str(input: &str, transform_function_whitespace: bool,
                      quirks_mode: bool) -> ~Parser {
        Parser::from_tokenizer(
            tokens::Tokenizer::from_str(input, transform_function_whitespace),
            quirks_mode)
    }

    // Consume the whole input and return a list of primitives.
    // Could be used for parsing eg. a stand-alone media query.
    // This is similar to consume_simple_block(), but there is no ending token.
    fn parse_primitives(&self) -> ~[Primitive] {
        let mut primitives: ~[Primitive] = ~[];
        for self.each_token |token| {
            primitives.push(consume_primitive(self, token))
        }
        primitives
    }

    fn parse_declarations(&self) -> ~[DeclarationBlockItem] {
        consume_declaration_block(self, /* is_nested= */false)
    }
}




//  ***********  End of public API  ***********


impl Parser {
    priv fn consume_token(&self) -> tokens::Token {
        let mut current_token = None;
        current_token <-> self.current_token;
        match current_token {
            Some(token) => token,
            None => {
                let (token, err) = self.tokenizer.next_token();
                match err {
                    Some(ParseError{message: message}) =>
                        // TODO more contextual error handling?
                        self.errors.push(message),
                    None => ()
                }
                token
            }
        }
    }

    priv fn each_token(&self, it: fn(token: tokens::Token) -> bool) {
        loop {
            match self.consume_token() {
                tokens::EOF => break,
                token => if !it(token) { break },
            }
        }
    }

    // Fail if the is already a "current token".
    // Call it at most once per iteration of .each_token()
    priv fn reconsume_token(&self, token: tokens::Token) {
        assert self.current_token.is_none();
        self.current_token = Some(token)
    }
}


// 5.3.5. Declaration-block mode
fn consume_declaration_block(parser: &Parser, is_nested: bool)
        -> ~[DeclarationBlockItem] {
    let mut items: ~[DeclarationBlockItem] = ~[];
    for parser.each_token |token| {
        match token {
            tokens::WhiteSpace | tokens::Semicolon => (),
            tokens::CloseCurlyBraket if is_nested => break,
//            tokens::AtKeyword(name)
//                => items.push(consume_at_rule(parser, name)),
            tokens::Ident(name)
            => match consume_declaration(parser, is_nested, name) {
                Some(declaration) => items.push(Declaration(declaration)),
                None => ()
            },
            token => {
                parser.reconsume_token(token);
                consume_declaration_error(parser, is_nested)
            },
        }
    }
    items
}


// 5.3.6. After-declaration-name mode
fn consume_declaration(parser: &Parser, is_nested: bool, name: ~str)
        -> Option<Declaration> {
    let mut name = name;  // XXX see <-> below
    for parser.each_token |token| {
        match token {
            tokens::WhiteSpace => (),
            tokens::Colon => {
                // XXX https://github.com/mozilla/rust/issues/4654
                let mut name_ = ~"";
                name_ <-> name;
                return consume_declaration_value(parser, is_nested, name_)
            }
            _ => { parser.reconsume_token(token); break }
        }
    }
    consume_declaration_error(parser, is_nested);
    None
}


// 5.3.7. Declaration-value mode
fn consume_declaration_value(parser: &Parser, is_nested: bool, name: ~str)
        -> Option<Declaration> {
    let mut name = name;  // XXX see <-> below
    let mut value: ~[Primitive] = ~[];
    for parser.each_token |token| {
        match token {
            tokens::CloseCurlyBraket if is_nested => {
                parser.reconsume_token(token); break
            }
            tokens::Semicolon => break,
            tokens::Delim('!') => {
                // XXX https://github.com/mozilla/rust/issues/4654
                let mut name_ = ~"";
                let mut value_: ~[Primitive] = ~[];
                name_ <-> name;
                value_ <-> value;
                return consume_declaration_important(
                    parser, is_nested, name_, value_)
            },
            _ => value.push(consume_value_primitive(parser, name, token)),
        }
    }
    Some(Declaration{name: name, value: value, important: false})
}


// 5.3.8. Declaration-important mode
fn consume_declaration_important(parser: &Parser, is_nested: bool,
                                 name: ~str, value: ~[Primitive])
        -> Option<Declaration> {
    let mut important = false;
    for parser.each_token |token| {
        match token {
            tokens::WhiteSpace => (),
            tokens::Ident(priority) => {
                important = ascii_lower(priority) == ~"important";
                break
            },
            token => { parser.reconsume_token(token); break }
        }
    }
    if !important {
        consume_declaration_error(parser, is_nested);
        return None
    }
    // 5.3.9. Declaration-end mode
    for parser.each_token |token| {
        match token {
            tokens::WhiteSpace => (),
            tokens::CloseCurlyBraket if is_nested
                => { parser.reconsume_token(token); break }
            tokens::Semicolon => break,
            _ => { consume_declaration_error(parser, is_nested); return None }
        }
    }
    Some(Declaration {name: name, value: value, important: true})
}


// 5.3.11. Next-declaration error mode
fn consume_declaration_error(parser: &Parser, is_nested: bool) {
    parser.errors.push(~"Invalid declaration");
    for parser.each_token |token| {
        match token {
            tokens::Semicolon => break,
            tokens::CloseCurlyBraket if is_nested => {
                parser.reconsume_token(token);
                break
            },
            _ => consume_primitive(parser, token)  // Ignore the return value
        };
    }
}


// 5.4. Consume a primitive
fn consume_primitive(parser: &Parser, first_token: tokens::Token)
        -> Primitive {
    match first_token {
        // Preserved tokens
        tokens::Ident(string) => Ident(string),
        tokens::AtKeyword(string) => AtKeyword(string),
        tokens::Hash(string) => Hash(string),
        tokens::String(string) => String(string),
        tokens::BadString => BadString,
        tokens::URL(string) => URL(string),
        tokens::BadURL => BadURL,
        tokens::Delim(ch) => Delim(ch),
        tokens::Number(value, repr) => Number(value, repr),
        tokens::Percentage(value, repr) => Percentage(value, repr),
        tokens::Dimension(value, repr, unit) => Dimension(value, repr, unit),
        tokens::UnicodeRange(start, end) => UnicodeRange(start, end),
        tokens::EmptyUnicodeRange => EmptyUnicodeRange,
        tokens::WhiteSpace => WhiteSpace,
        tokens::Colon => Colon,
        tokens::Semicolon => Semicolon,
        tokens::CDO => CDO,
        tokens::CDC => CDC,
        tokens::CloseSquareBraket => CloseSquareBraket,
        tokens::CloseParenthesis => CloseParenthesis,
        tokens::CloseCurlyBraket => CloseCurlyBraket,

        tokens::OpenSquareBraket => SquareBraketBlock(
            consume_simple_block(parser, tokens::CloseSquareBraket)),
        tokens::OpenParenthesis => ParenthesisBlock(
            consume_simple_block(parser, tokens::CloseParenthesis)),
        tokens::OpenCurlyBraket => CurlyBraketBlock(
            consume_simple_block(parser, tokens::CloseCurlyBraket)),

        tokens::Function(string) => consume_function(parser, string),

        // Getting here is a  programming error.
        tokens::EOF => fail,
    }
}


// 5.5. Consume a primitive with the hashless color quirk
const HASHLESS_COLOR_QUIRK: &[&str] = &[
    &"background-color",
    &"border-color",
    &"border-top-color",
    &"border-right-color",
    &"border-bottom-color",
    &"border-left-color",
    &"color",
];

// 5.6. Consume a primitive with the unitless length quirk
const UNITLESS_LENGTH_QUIRK: &[&str] = &[
    &"border-top-width",
    &"border-right-width",
    &"border-bottom-width",
    &"border-left-width",
    &"border-width",
    &"bottom",
    &"font-size",
    &"height",
    &"left",
    &"letter-spacing",
    &"margin",
    &"margin-right",
    &"margin-left",
    &"margin-top",
    &"margin-bottom",
    &"padding",
    &"padding-top",
    &"padding-right",
    &"padding-bottom",
    &"padding-left",
    &"right",
    &"top",
    &"width",
    &"word-spacing",
];

fn consume_value_primitive(parser: &Parser, name: &str, token: tokens::Token)
        -> Primitive {
    if !parser.quirks_mode {
        return consume_primitive(parser, token)
    }
    #[inline(always)]
    fn contains(haystack: &[&str], needle: &(&str)) -> bool {
        // TODO: This is a O(n) linear search. We can probably do better.
        vec::contains(haystack, needle)
    }
    #[inline(always)]
    fn is_hex_color(value: &str) -> bool {
        match value.len() {
            3 | 6 => (),
            _ => return false
        }
        for value.each_char |ch| {
            match ch {
                '0'..'9' | 'a'..'f' | 'A'..'F' => (),
                _ => return false
            }
        }
        true
    }
    match token {
        tokens::Number(value, repr) => {
            let name: &str = ascii_lower(name);
            if is_hex_color(repr) && contains(HASHLESS_COLOR_QUIRK, &name) {
                Hash(repr)
            } else if contains(UNITLESS_LENGTH_QUIRK, &name) {
                Dimension(value, repr, ~"px")
            } else {
                Number(value, repr)
            }
        }
        tokens::Dimension(value, repr, unit) => {
            let name: &str = ascii_lower(name);
            let color = repr + unit;
            if is_hex_color(color) && contains(HASHLESS_COLOR_QUIRK, &name) {
                Hash(color)
            } else {
                Dimension(value, repr, unit)
            }
        }
        token => consume_primitive(parser, token)
    }
}

// 5.7. Consume a simple block  (kind of)
fn consume_simple_block(parser: &Parser, ending_token: tokens::Token)
        -> ~[Primitive] {
    let mut value: ~[Primitive] = ~[];
    for parser.each_token |token| {
        if token == ending_token { break }
        else { value.push(consume_primitive(parser, token)) }
    }
    value
}


// 5.8. Consume a function
fn consume_function(parser: &Parser, name: ~str)
        -> Primitive {
    let mut current_argument: ~[Primitive] = ~[];
    let mut arguments: ~[~[Primitive]] = ~[];
    for parser.each_token |token| {
        match token {
            tokens::CloseParenthesis => break,
            tokens::Delim(',') => {
                // XXX https://github.com/mozilla/rust/issues/4654
                let mut arg: ~[Primitive] = ~[];
                arg <-> current_argument;
                arguments.push(arg);
            },
            tokens::Number(value, repr) => current_argument.push(
                if parser.quirks_mode && ascii_lower(name) == ~"rect" {
                    // 5.6. Consume a primitive
                    // with the unitless length quirk
                    Dimension(value, repr, ~"px")
                } else {
                    Number(value, repr)
                }
            ),
            token => current_argument.push(consume_primitive(parser, token)),
        }
    }
    arguments.push(current_argument);
    Function(name, arguments)
}


#[test]
fn test_primitives() {
    fn assert_primitives(
            input: &str, quirks_mode: bool,
            expected_primitives: &[Primitive], expected_errors: &[~str]) {
        let parser = Parser::from_str(input, false, quirks_mode);
        tests::check_results(
            parser.parse_primitives(), expected_primitives,
            parser.errors, expected_errors);
    }

    assert_primitives("", false, [], []);
    assert_primitives("42 foo([aa ()b], -){\n  }", false, [
        Number(Integer(42), ~"42"), WhiteSpace,
        Function(~"foo", ~[
            ~[SquareBraketBlock(~[
                Ident(~"aa"), WhiteSpace, ParenthesisBlock(~[]), Ident(~"b")])],
            ~[WhiteSpace, Delim('-')]]),
        CurlyBraketBlock(~[
            WhiteSpace])], []);
    assert_primitives(
        "'foo", false, [String(~"foo")], [~"EOF in quoted string"]);
    // Without quirks mode, for reference
    assert_primitives("rgba(1,2%)rect(1,2%)Rect(1,2%)Réct(1,2%)", false, [
        Function(~"rgba", ~[~[Number(Integer(1), ~"1")],
                            ~[Percentage(Integer(2), ~"2")]]),
        Function(~"rect", ~[~[Number(Integer(1), ~"1")],
                            ~[Percentage(Integer(2), ~"2")]]),
        Function(~"Rect", ~[~[Number(Integer(1), ~"1")],
                            ~[Percentage(Integer(2), ~"2")]]),
        Function(~"Réct", ~[~[Number(Integer(1), ~"1")],
                            ~[Percentage(Integer(2), ~"2")]]),
    ], []);
    // With quirks mode
    assert_primitives("rgba(1,2%)rect(1,2%)Rect(1,2%)Réct(1,2%)", true, [
        Function(~"rgba", ~[~[Number(Integer(1), ~"1")],
                            ~[Percentage(Integer(2), ~"2")]]),
        Function(~"rect", ~[~[Dimension(Integer(1), ~"1", ~"px")],
                            ~[Percentage(Integer(2), ~"2")]]),
        Function(~"Rect", ~[~[Dimension(Integer(1), ~"1", ~"px")],
                            ~[Percentage(Integer(2), ~"2")]]),
        Function(~"Réct", ~[~[Number(Integer(1), ~"1")],
                            ~[Percentage(Integer(2), ~"2")]]),
    ], []);
}


#[test]
fn test_declarations() {
    fn assert_declarations(
            input: &str, quirks_mode: bool,
            expected_declarations: &[DeclarationBlockItem],
            expected_errors: &[~str]) {
        let parser = Parser::from_str(input, false, quirks_mode);
        tests::check_results(
            parser.parse_declarations(), expected_declarations,
            parser.errors, expected_errors);
    }
    fn decl(name: ~str, value: ~[Primitive]) -> DeclarationBlockItem {
        Declaration(Declaration{name: name, value: value, important: false})
    }
    fn important(name: ~str, value: ~[Primitive]) -> DeclarationBlockItem {
        Declaration(Declaration{name: name, value: value, important: true})
    }

    assert_declarations("", false, [], []);
    assert_declarations("", true, [], []);
    assert_declarations(" \n  ;; ", false, [], []);
    assert_declarations("color", false, [], [~"Invalid declaration"]);
    assert_declarations("color/ red", false, [], [~"Invalid declaration"]);
    assert_declarations("/**/ color:", false, [decl(~"color", ~[])], []);
    assert_declarations("a /{(;);}b; c:d", false,
        [decl(~"c", ~[Ident(~"d")])], [~"Invalid declaration"]);
    assert_declarations("color /**/: black,red; ;", false,
        [decl(~"color", ~[
            WhiteSpace, Ident(~"black"), Delim(','), Ident(~"red")
        ])], []);

    // !important
    assert_declarations("a:!important /**/; b:c!IMPORTant", false,
        [important(~"a", ~[]), important(~"b", ~[Ident(~"c")])], []);
    assert_declarations("a:!important b:c", false,
        [], [~"Invalid declaration"]);
    assert_declarations("a:!important} b:c", false,
        [], [~"Invalid declaration"]);
    assert_declarations("a:!stuff; a:!!;a:!", false, [], [
        ~"Invalid declaration", ~"Invalid declaration", ~"Invalid declaration"
    ]);
    // XXX See http://lists.w3.org/Archives/Public/www-style/2013Jan/0491.html
    assert_declarations(
        "a:! important; a:!/**/important;a:!/**/ important", false,
        [important(~"a", ~[]), important(~"a", ~[]), important(~"a", ~[])],
        []);

    // Without quirks mode, for reference
    assert_declarations("TOP:12; bottom:1.5; size:42", false, [
        decl(~"TOP", ~[Number(Integer(12), ~"12")]),
        decl(~"bottom", ~[Number(Float(1.5), ~"1.5")]),
        decl(~"size", ~[Number(Integer(42), ~"42")]),
    ], []);
    assert_declarations("color:012;Color:0f2;bORDER-color:123456", false, [
        decl(~"color", ~[Number(Integer(12), ~"012")]),
        decl(~"Color", ~[Dimension(Integer(0), ~"0", ~"f2")]),
        decl(~"bORDER-color", ~[Number(Integer(123456), ~"123456")]),
    ], []);
    assert_declarations("color:0123;color:0g2", false, [  // Not 3 or 6 hex
        decl(~"color", ~[Number(Integer(123), ~"0123")]),
        decl(~"color", ~[Dimension(Integer(0), ~"0", ~"g2")]),
    ], []);
    assert_declarations("fill:123", false,  // Not in the list
        [decl(~"fill", ~[Number(Integer(123), ~"123")])], []);
    // Quirks mode
    assert_declarations("TOP:12; bottom:1.5; size:42", true, [
        decl(~"TOP", ~[Dimension(Integer(12), ~"12", ~"px")]),
        decl(~"bottom", ~[Dimension(Float(1.5), ~"1.5", ~"px")]),
        decl(~"size", ~[Number(Integer(42), ~"42")]),
    ], []);
    assert_declarations("color:012;Color:0f2;bORDER-color:123456", true, [
        decl(~"color", ~[Hash(~"012")]),
        decl(~"Color", ~[Hash(~"0f2")]),
        decl(~"bORDER-color", ~[Hash(~"123456")]),
    ], []);
    assert_declarations("color:0123;color:0g2", true, [  // Not 3 or 6 hex
        decl(~"color", ~[Number(Integer(123), ~"0123")]),
        decl(~"color", ~[Dimension(Integer(0), ~"0", ~"g2")]),
    ], []);
    assert_declarations("fill:123", true,  // Not in the list
        [decl(~"fill", ~[Number(Integer(123), ~"123")])], []);
}
