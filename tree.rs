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
    fn assert_primitives(input: &str, expected_primitives: &[Primitive],
                         expected_errors: &[~str]) {
        assert_primitives_flags(
            input, expected_primitives, expected_errors, false)
    }
    fn assert_primitives_flags(input: &str, expected_primitives: &[Primitive],
                               expected_errors: &[~str], quirks_mode: bool) {
        let parser = Parser::from_str(input, false, quirks_mode);
        let primitives: &[Primitive] = consume_primitive_list(parser);
        let errors: &[~str] = parser.errors;
        if primitives != expected_primitives {
            fail fmt!("%?\n!=\n%?", primitives, expected_primitives);
        }
        if errors != expected_errors {
            fail fmt!("%?\n!=\n%?", errors, expected_errors);
        }
    }

    assert_primitives("", [], []);
    assert_primitives("42 foo([aa ()b], -){\n  }", [
        Number(Integer(42), ~"42"), WhiteSpace,
        Function(~"foo", ~[
            ~[BraketBlock(~[
                Ident(~"aa"), WhiteSpace, ParenBlock(~[]), Ident(~"b")])],
            ~[WhiteSpace, Delim('-')]]),
        BraceBlock(~[
            WhiteSpace])], [])
}
