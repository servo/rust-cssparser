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
    URL(~str),
    Delim(char),
    Number(NumericValue, ~str),  // value, representation
    Percentage(NumericValue, ~str),  // value, representation
    Dimension(NumericValue, ~str, ~str),  // value, representation, unit
    UnicodeRange(char, char),  // start, end
    EmptyUnicodeRange,
    WhiteSpace,
    Comment,
    CDO,  // <!--
    CDC,  // -->
    Colon,  // :
    Semicolon,  // ;
    CloseBraket, // ]
    CloseParen, // )
    CloseBrace, // }

    // Function
    Function(~str, ~[~[Primitive]]),  // name, arguments

    // Simple block
    BraketBlock(~[Primitive]),  // […]
    ParenBlock(~[Primitive]),  // (…)
    BraceBlock(~[Primitive]),  // {…}
}


pub struct Declaration {
    name: ~str,
    value: ~[Primitive],
    important: bool,
}

pub struct StyleRule {
    selector: ~[Primitive],
    value: ~[DeclarationBlockItem],
}

pub struct AtRule {
    name: ~str,
    selector: ~[Primitive],
    value: AtRuleValue,
}


pub enum AtRuleValue {
    EmptyAtRule,  // @foo…;
    DeclarationFilled(~[DeclarationBlockItem]),
    RuleFilled(~[Rule]),
}

pub enum DeclarationBlockItem {
    Declaration(Declaration),
    InvalidDeclaration(~[Primitive]),
    // A better idea for a name that means "at-rule" but is not "AtRule"?
    Decl_AtRule(AtRule),
}

pub enum Rule {
    StyleRule(StyleRule),
    AtRule(AtRule),
    InvalidRule(~[Primitive]),
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
}


// Consume the whole input and return a list of primitives.
// Could be used for parsing eg. a stand-alone media query.
// This is similar to consume_simple_block(), but there is no ending token.
pub fn consume_primitive_list(parser: &Parser) -> ~[Primitive] {
    let mut primitives: ~[Primitive] = ~[];
    for parser.each_token |token| {
        primitives.push(consume_primitive(parser, token))
    }
    primitives
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

    // Skips comments
    priv fn each_token(&self, it: fn(token: tokens::Token) -> bool) {
        loop {
            match self.consume_token() {
                tokens::Comment => (),
                tokens::EOF => break,
                token => if !it(token) { break },
            }
        }
    }

    // Fail if the is already a "current token".
    // The easiest way to ensure this does not fail it to call it only once
    // just after calling parser.consume_token()
    priv fn reconsume_token(&self, token: tokens::Token) {
        assert self.current_token.is_none();
        self.current_token = Some(token)
    }
}


// 3.5.7. Declaration-block mode
fn consume_declaration_block(parser: &Parser, is_nested: bool)
        -> ~[DeclarationBlockItem] {
    let mut items: ~[DeclarationBlockItem] = ~[];
    for parser.each_token |token| {
        match token {
            tokens::WhiteSpace | tokens::Semicolon => (),
            tokens::CloseBrace if is_nested => break,
//            tokens::AtKeyword(name)
//                => items.push(consume_at_rule(parser, name)),
            tokens::Ident(name)
                => items.push(consume_declaration(parser, is_nested, name)),
            token => {
                parser.reconsume_token(token);
                items.push(consume_declaration_error(parser, is_nested, ~[]))
            },
        }
    }
    items
}


// 3.5.8. After-declaration-name mode
fn consume_declaration(parser: &Parser, is_nested: bool, name: ~str)
        -> DeclarationBlockItem {
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
    consume_declaration_error(parser, is_nested, ~[Ident(name)])
}


// 3.5.9. Declaration-value mode
fn consume_declaration_value(parser: &Parser, is_nested: bool, name: ~str)
        -> DeclarationBlockItem {
    let mut name = name;  // XXX see <-> below
    let mut value: ~[Primitive] = ~[];
    for parser.each_token |token| {
        match token {
            tokens::CloseBrace if is_nested => {
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
    Declaration(Declaration{name: name, value: value, important: false})
}


// 3.5.10. Declaration-important mode
fn consume_declaration_important(parser: &Parser, is_nested: bool,
                                 name: ~str, value: ~[Primitive])
        -> DeclarationBlockItem {
    // XXX Consume whitespace and comments here?
    // http://lists.w3.org/Archives/Public/www-style/2013Jan/0491.html
    let token = parser.consume_token();
    let (is_important, token) = match token {
        // TODO: consider "can be made important"?
        tokens::Ident(value) =>
            (ascii_lower(value) == ~"important", tokens::Ident(value)),
        token => (false, token)
    };
    if is_important {
        Declaration(Declaration {name: name, value: value, important: true})
    } else {
        parser.reconsume_token(token);
        let mut invalid = ~[Ident(name), Colon];
        let mut value = value;
        vec::push_all_move(&mut invalid, value);
        consume_declaration_error(parser, is_nested, invalid)
    }
}


// 3.5.13. Next-declaration error mode
fn consume_declaration_error(parser: &Parser, is_nested: bool,
                             invalid: ~[Primitive]) -> DeclarationBlockItem {
    let mut invalid = invalid;
    parser.errors.push(~"Invalid declaration");
    for parser.each_token |token| {
        match token {
            tokens::Semicolon => break,
            tokens::CloseBrace if is_nested => {
                parser.reconsume_token(token);
                break
            },
            _ => invalid.push(consume_primitive(parser, token))
        }
    }
    InvalidDeclaration(invalid)
}


// 3.5.15. Consume a primitive
fn consume_primitive(parser: &Parser, first_token: tokens::Token)
        -> Primitive {
    match first_token {
        // Preserved tokens
        tokens::Ident(string) => Ident(string),
        tokens::AtKeyword(string) => AtKeyword(string),
        tokens::Hash(string) => Hash(string),
        tokens::String(string) => String(string),
        tokens::URL(string) => URL(string),
        tokens::Delim(ch) => Delim(ch),
        tokens::Number(value, repr) => Number(value, repr),
        tokens::Percentage(value, repr) => Percentage(value, repr),
        tokens::Dimension(value, repr, unit) => Dimension(value, repr, unit),
        tokens::UnicodeRange(start, end) => UnicodeRange(start, end),
        tokens::EmptyUnicodeRange => EmptyUnicodeRange,
        tokens::WhiteSpace => WhiteSpace,
        tokens::Colon => Colon,
        tokens::Semicolon => Semicolon,

        // Simple blocks
        tokens::OpenBraket =>
            BraketBlock(consume_simple_block(parser, tokens::CloseBraket)),
        tokens::OpenParen =>
            ParenBlock(consume_simple_block(parser, tokens::CloseParen)),
        tokens::OpenBrace =>
            BraceBlock(consume_simple_block(parser, tokens::CloseBrace)),

        // Functions
        tokens::Function(string) => consume_function(parser, string),

        // Non-preserved tokens
        // TODO These still need to be dealt with somehow.
        tokens::BadString | tokens::BadURL | tokens::CDO | tokens::CDC
            | tokens::CloseBraket | tokens::CloseParen | tokens::CloseBrace
            => fail,

        // Getting here is a  programming error.
        tokens::Comment | tokens::EOF => fail,
    }
}


// 3.5.16. Consume a primitive with the hashless color quirk
const HASHLESS_COLOR_QUIRK: &[&str] = &[
    &"background-color",
    &"border-color",
    &"border-top-color",
    &"border-right-color",
    &"border-bottom-color",
    &"border-left-color",
    &"color",
];

// 3.5.17. Consume a primitive with the unitless length quirk
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
            if is_hex_color(repr) && contains(HASHLESS_COLOR_QUIRK, &name) {
                Hash(repr)
            } else if contains(UNITLESS_LENGTH_QUIRK, &name) {
                Dimension(value, repr, ~"px")
            } else {
                Number(value, repr)
            }
        }
        tokens::Dimension(value, repr, unit) => {
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

// 3.5.18. Consume a simple block  (kind of)
fn consume_simple_block(parser: &Parser, ending_token: tokens::Token)
        -> ~[Primitive] {
    let mut value: ~[Primitive] = ~[];
    for parser.each_token |token| {
        if token == ending_token { break }
        else { value.push(consume_primitive(parser, token)) }
    }
    value
}


// 3.5.19. Consume a function
fn consume_function(parser: &Parser, name: ~str)
        -> Primitive {
    let mut current_argument: ~[Primitive] = ~[];
    let mut arguments: ~[~[Primitive]] = ~[];
    for parser.each_token |token| {
        match token {
            tokens::CloseParen => break,
            tokens::Delim(',') => {
                // XXX https://github.com/mozilla/rust/issues/4654
                let mut arg: ~[Primitive] = ~[];
                arg <-> current_argument;
                arguments.push(arg);
            },
            tokens::Number(value, repr) => current_argument.push(
                if parser.quirks_mode && ascii_lower(name) == ~"rect" {
                    // 3.5.17. Consume a primitive
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
            consume_primitive_list(parser), expected_primitives,
            parser.errors, expected_errors);
    }

    assert_primitives("", false, [], []);
    assert_primitives("42 foo([aa ()b], -){\n  }", false, [
        Number(Integer(42), ~"42"), WhiteSpace,
        Function(~"foo", ~[
            ~[BraketBlock(~[
                Ident(~"aa"), WhiteSpace, ParenBlock(~[]), Ident(~"b")])],
            ~[WhiteSpace, Delim('-')]]),
        BraceBlock(~[
            WhiteSpace])], [])
}
