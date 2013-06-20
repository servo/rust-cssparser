// http://dev.w3.org/csswg/css3-syntax/#tree-construction
//
// The input to the tree construction stage is a sequence of tokens
// from the tokenization stage.
// The output is a tree of items with a stylesheet at the root
// and all other nodes being at-rules, style rules, or declarations.


extern mod core;
use core::util::{swap, replace};
use super::utils::*;
use super::tokens;
//mod tokens;
//mod utils;


// When reporting these errors to the user, the application is expected
// to show the source filename/URL/location in addition to line and column.
pub struct ParseError {
    message: ~str,
    // TODO: add these:
//    source_line: uint,
//    source_column: uint,
}


#[deriving(Eq)]
pub enum NumericValue {
    Integer(int),
    // The spec calls this "number".
    // Use "float" instead to reduce term overloading with "number token".
    Float(float),
}


impl NumericValue {
    fn to_float(&self) -> float {
        match *self {
            Integer(value) => value as float,
            Float(value) => value,
        }
    }
}


#[deriving(Eq)]
pub enum Primitive {
    // Preserved tokens. Same as in the tokenizer.
    Ident(~str),
    AtKeyword(~str),
    Hash(~str),
    String(~str),
    URL(~str),
    Delim(char),
    Number(NumericValue, ~str),  // value, representation
    Percentage(NumericValue, ~str),  // value, representation
    Dimension(NumericValue, ~str, ~str),  // value, representation, unit
    UnicodeRange(char, char),  // start, end
    EmptyUnicodeRange,
    WhiteSpace,
    Colon,  // :
    Semicolon,  // ;

    // Function
    Function(~str, ~[~[Primitive]]),  // name, arguments

    // Simple block
    ParenthesisBlock(~[Primitive]),  // (…)
    SquareBraketBlock(~[Primitive]),  // […]
    CurlyBraketBlock(~[Primitive]),  // {…}

    // These are alwas invalid
    BadURL,
    BadString,
    CDO,  // <!--
    CDC,  // -->
    CloseParenthesis, // )
    CloseSquareBraket, // ]
    CloseCurlyBraket, // }
}


#[deriving(Eq)]
pub struct Declaration {
    name: ~str,
    value: ~[Primitive],
    important: bool,
}

#[deriving(Eq)]
pub struct StyleRule {
    selector: ~[Primitive],
    value: ~[DeclarationBlockItem],
}

#[deriving(Eq)]
pub struct AtRule {
    name: ~str,
    prelude: ~[Primitive],
    value: AtRuleValue,
}


#[deriving(Eq)]
pub enum AtRuleValue {
    NotFilled,  // @foo…;
    DeclarationFilled(~[DeclarationBlockItem]),
    RuleFilled(~[Rule]),
}


#[deriving(Eq)]
pub enum DeclarationBlockItem {
    Declaration(Declaration),
    // A better idea for a name that means "at-rule" but is not "AtRule"?
    Decl_AtRule(AtRule),
}

#[deriving(Eq)]
pub enum Rule {
    StyleRule(StyleRule),
    AtRule(AtRule),
}


pub struct Parser {
    priv tokenizer: ~tokens::Tokenizer,
    priv quirks_mode: bool,
    priv current_token: Option<tokens::Token>,
    priv errors: ~[~str],
    priv rule_filled_at_rules: ~[~str],
    priv declaration_filled_at_rules: ~[~str],
}


impl Parser {
    fn from_tokenizer(
            tokenizer: ~tokens::Tokenizer, quirks_mode: bool) -> ~Parser {
        ~Parser {
            tokenizer: tokenizer,
            quirks_mode: quirks_mode,
            current_token: None,
            errors: ~[],
            rule_filled_at_rules: ~[~"media"],
            declaration_filled_at_rules: ~[~"page"],
        }
    }
    fn from_str(input: &str, transform_function_whitespace: bool,
                      quirks_mode: bool) -> ~Parser {
        Parser::from_tokenizer(
            tokens::Tokenizer::from_str(input, transform_function_whitespace),
            quirks_mode)
    }

    // Consume the whole input and return a list of primitives.
    // Could be used for parsing eg. a stand-alone media query.
    // This is similar to consume_simple_block(), but there is no ending token.
    fn parse_primitives(&mut self) -> ~[Primitive] {
        let mut primitives: ~[Primitive] = ~[];
        for self.each_token |parser, token| {
            primitives.push(consume_primitive(parser, token))
        }
        primitives
    }

    fn parse_declarations(&mut self) -> ~[DeclarationBlockItem] {
        consume_declaration_block(self, /* is_nested= */false)
    }

    fn parse_stylesheet(&mut self) -> ~[Rule] {
        consume_top_level_rules(self)
    }
}


//  ***********  End of public API  ***********


impl Parser {
    priv fn consume_token(&mut self) -> tokens::Token {
        let mut current_token = None;
        swap(&mut current_token, &mut self.current_token);
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

    priv fn each_token(
        &mut self,
        it: &fn(parser: &mut Parser, token: tokens::Token) -> bool
    ) -> bool {
        loop {
            match self.consume_token() {
                tokens::EOF => return true,
                token => if !it(self, token) { return false },
            }
        }
    }

    // Fail if the is already a "current token".
    // Call it at most once per iteration of .each_token()
    priv fn reconsume_token(&mut self, token: tokens::Token) {
        assert!(self.current_token.is_none());
        self.current_token = Some(token)
    }

    priv fn is_rule_filled(&mut self, name: &(~str)) -> bool {
        vec::contains(self.rule_filled_at_rules, name)
    }

    priv fn is_declaration_filled(&mut self, name: &(~str)) -> bool {
        vec::contains(self.declaration_filled_at_rules, name)
    }
}


// 5.3.1. Top-level mode
fn consume_top_level_rules(parser: &mut Parser) -> ~[Rule] {
    let mut rules: ~[Rule] = ~[];
    for parser.each_token |parser, token| {
        match token {
            tokens::WhiteSpace | tokens::CDO | tokens::CDC => (),
            tokens::AtKeyword(name)
            => match consume_at_rule(parser, true, name) {
                Some(at_rule) => rules.push(AtRule(at_rule)),
                None => ()
            },
            token => {
                parser.reconsume_token(token);
                match consume_style_rule(parser, true) {
                    Some(style_rule) => rules.push(StyleRule(style_rule)),
                    None => (),
                }
            }
        }
    }
    rules
}


// 5.3.2. At-rule-prelude mode
fn consume_at_rule(parser: &mut Parser, is_nested: bool, name: ~str)
        -> Option<AtRule> {
    let mut prelude: ~[Primitive] = ~[];
    let mut value: AtRuleValue = NotFilled;
    for parser.each_token |parser, token| {
        match token {
            tokens::OpenCurlyBraket => {
                value = if parser.is_rule_filled(&name) {
                    RuleFilled(~[])
                } else if parser.is_declaration_filled(&name) {
                    DeclarationFilled(~[])
                } else {
                    parser.errors.push(
                        fmt!("Unexpected {} block for @%s rule", name));
                    consume_primitive(parser, token);  // Ignore the block.
                    return None
                }
            },
            tokens::Semicolon => break,
            tokens::CloseCurlyBraket if is_nested
                => { parser.reconsume_token(token); break }
            _ => prelude.push(consume_primitive(parser, token)),
        }
    }
    match value {
        NotFilled => {
            if parser.is_rule_filled(&name) ||
                    !parser.is_declaration_filled(&name)  {
                parser.errors.push(
                    fmt!("Missing {} block for @%s rule", name));
                None
            } else {
                Some(AtRule {name: name, prelude: prelude, value: NotFilled})
            }
        }
        RuleFilled(_) => Some(AtRule {
            name: name, prelude: prelude, value: RuleFilled(
                consume_rule_block(parser))}),
        DeclarationFilled(_) => Some(AtRule {
            name: name, prelude: prelude, value: DeclarationFilled(
                consume_declaration_block(parser, true))}),
    }
}


// 5.3.3. Rule-block mode
fn consume_rule_block(parser: &mut Parser) -> ~[Rule] {
    let mut rules: ~[Rule] = ~[];
    for parser.each_token |parser, token| {
        match token {
            tokens::WhiteSpace => (),
            tokens::CloseCurlyBraket => break,
            tokens::AtKeyword(name)
            => match consume_at_rule(parser, true, name) {
                Some(at_rule) => rules.push(AtRule(at_rule)),
                None => ()
            },
            token => {
                parser.reconsume_token(token);
                match consume_style_rule(parser, true) {
                    Some(style_rule) => rules.push(StyleRule(style_rule)),
                    None => (),
                }
            }
        }
    }
    rules
}


// 5.3.4. Selector mode
fn consume_style_rule(parser: &mut Parser, is_nested: bool) -> Option<StyleRule> {
    let mut selector: ~[Primitive] = ~[];
    for parser.each_token |parser, token| {
        match token {
            tokens::OpenCurlyBraket => {
                return Some(StyleRule{
                    selector: replace(&mut selector, ~[]),
                    value: consume_declaration_block(parser, true)})
            },
            tokens::CloseCurlyBraket if is_nested
                => { parser.reconsume_token(token); break }
            _ => selector.push(consume_primitive(parser, token)),
        }
    }
    parser.errors.push(~"Missing {} block for style rule");
    None
}


// 5.3.5. Declaration-block mode
fn consume_declaration_block(parser: &mut Parser, is_nested: bool)
        -> ~[DeclarationBlockItem] {
    let mut items: ~[DeclarationBlockItem] = ~[];
    for parser.each_token |parser, token| {
        match token {
            tokens::WhiteSpace | tokens::Semicolon => (),
            tokens::CloseCurlyBraket if is_nested => break,
            tokens::AtKeyword(name)
            => match consume_at_rule(parser, true, name) {
                Some(at_rule) => items.push(Decl_AtRule(at_rule)),
                None => ()
            },
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
fn consume_declaration(parser: &mut Parser, is_nested: bool, name: ~str)
        -> Option<Declaration> {
    let mut name = name;  // XXX see replace() below
    for parser.each_token |parser, token| {
        match token {
            tokens::WhiteSpace => (),
            tokens::Colon => {
                return consume_declaration_value(
                    parser, is_nested, replace(&mut name, ~""))
            }
            _ => { parser.reconsume_token(token); break }
        }
    }
    consume_declaration_error(parser, is_nested);
    None
}


// 5.3.7. Declaration-value mode
fn consume_declaration_value(parser: &mut Parser, is_nested: bool, name: ~str)
        -> Option<Declaration> {
    let mut name = name;  // XXX see <-> below
    let mut value: ~[Primitive] = ~[];
    for parser.each_token |parser, token| {
        match token {
            tokens::CloseCurlyBraket if is_nested
                => { parser.reconsume_token(token); break }
            tokens::Semicolon => break,
            tokens::Delim('!') => {
                return consume_declaration_important(
                    parser, is_nested, replace(&mut name, ~""),
                    replace(&mut value, ~[]))
            },
            _ => value.push(consume_value_primitive(parser, name, token)),
        }
    }
    Some(Declaration{name: name, value: value, important: false})
}


// 5.3.8. Declaration-important mode
fn consume_declaration_important(parser: &mut Parser, is_nested: bool,
                                 name: ~str, value: ~[Primitive])
        -> Option<Declaration> {
    let mut important = false;
    for parser.each_token |parser, token| {
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
    for parser.each_token |parser, token| {
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
fn consume_declaration_error(parser: &mut Parser, is_nested: bool) {
    parser.errors.push(~"Invalid declaration");
    for parser.each_token |parser, token| {
        match token {
            tokens::Semicolon => break,
            tokens::CloseCurlyBraket if is_nested
                => { parser.reconsume_token(token); break }
            _ => consume_primitive(parser, token)  // Ignore the return value
        };
    }
}


// 5.4. Consume a primitive
fn consume_primitive(parser: &mut Parser, first_token: tokens::Token)
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
        tokens::EOF => fail!(),
    }
}


// 5.5. Consume a primitive with the hashless color quirk
static HASHLESS_COLOR_QUIRK: &'static[&'static str] = &[
    &"background-color",
    &"border-color",
    &"border-top-color",
    &"border-right-color",
    &"border-bottom-color",
    &"border-left-color",
    &"color",
];

// 5.6. Consume a primitive with the unitless length quirk
static UNITLESS_LENGTH_QUIRK: &'static[&'static str] = &[
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

fn consume_value_primitive(parser: &mut Parser, name: &str, token: tokens::Token)
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
        for str::each_char(value) |ch| {
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
fn consume_simple_block(parser: &mut Parser, ending_token: tokens::Token)
        -> ~[Primitive] {
    let mut value: ~[Primitive] = ~[];
    for parser.each_token |parser, token| {
        if token == ending_token { break }
        else { value.push(consume_primitive(parser, token)) }
    }
    value
}


// 5.8. Consume a function
fn consume_function(parser: &mut Parser, name: ~str)
        -> Primitive {
    let mut current_argument: ~[Primitive] = ~[];
    let mut arguments: ~[~[Primitive]] = ~[];
    for parser.each_token |parser, token| {
        match token {
            tokens::CloseParenthesis => break,
            tokens::Delim(',') => {
                arguments.push(replace(&mut current_argument, ~[]));
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
        let mut parser = Parser::from_str(input, false, quirks_mode);
        check_results(
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
        let mut parser = Parser::from_str(input, false, quirks_mode);
        check_results(
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
