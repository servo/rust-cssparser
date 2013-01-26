// http://dev.w3.org/csswg/css3-syntax/#tree-construction
//
// The input to the tree construction stage is a sequence of tokens
// from the tokenization stage.
// The output is a tree of items with a stylesheet at the root
// and all other nodes being at-rules, style rules, or declarations.


use tokens;


#[deriving_eq]
enum Primitive {
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


struct Declaration {
    name: ~str,
    value: ~[Primitive],
    important: bool,
}

struct StyleRule {
    selector: ~[Primitive],
    value: ~[DeclarationListItem],
}

struct AtRule {
    name: ~str,
    selector: ~[Primitive],
    value: AtRuleValue,
}


enum AtRuleValue {
    EmptyAtRule,  // @foo…;
    DeclarationFilled(~[DeclarationListItem]),
    RuleFilled(~[RuleListItem]),
}

enum DeclarationListItem {
    Declaration(Declaration),
    // A better idea for a name that means "at-rule" but is not "AtRule"?
    Decl_AtRule(AtRule),
}

enum RuleListItem {
    StyleRule(StyleRule),
    AtRule(AtRule),
}


struct State {
    tokenizer: ~tokens::Tokenizer,
    quirks_mode: bool,
    mut current_token: Option<tokens::Token>,
}
