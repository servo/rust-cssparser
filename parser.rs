// http://dev.w3.org/csswg/css-syntax/#parsing
//
// The input to the tree construction stage is a sequence of tokens
// from the tokenization stage.
// The output is a tree of items with a stylesheet at the root
// and all other nodes being at-rules, style rules, or declarations.


use std::iterator::Iterator;
use std::util::replace;
use std::vec;

use ast::*;
use tokenizer::*;


// TODO: Use a trait?
enum ComponentValueIterator {
    ParserIter(~Parser),
    VectorIter(~[ComponentValue]),
}


impl ComponentValueIterator {
    #[inline]
    pub fn from_str(input: ~str) -> ComponentValueIterator {
        ComponentValueIterator::from_parser(~Parser::from_str(input))
    }

    #[inline]
    pub fn from_parser(parser: ~Parser) -> ComponentValueIterator {
        ParserIter(parser)
    }

    #[inline]
    pub fn from_vector(mut values: ~[ComponentValue]) -> ComponentValueIterator {
        // TODO: find a way to have parse_iter() or something instead of reverse() + pop()
        vec::reverse(values);
        VectorIter(values)
    }

    #[inline]
    pub fn next_non_whitespace(&mut self) -> Option<ComponentValue> {
        for self.advance |component_value| {
            if component_value != WhiteSpace { return Some(component_value) }
        }
        None
    }
}


impl Iterator<ComponentValue> for ComponentValueIterator {
    fn next(&mut self) -> Option<ComponentValue> {
        match self {
            &ParserIter(ref mut parser) => next_component_value(*parser),
            &VectorIter(ref mut reversed_vector)
            => if reversed_vector.is_empty() { None } else { Some(reversed_vector.pop()) }
        }
    }
}


// Work around "error: cannot borrow `*iter` as mutable more than once at a time"
// when using a normal for loop.
macro_rules! for_iter(
    ($iter: expr, $pattern: pat, $loop_body: expr) => (
        loop {
            match $iter.next() { None => break, Some($pattern) => $loop_body }
        }
    );
)



pub fn parse_stylesheet_rule(iter: &mut ComponentValueIterator) -> Option<Result<Rule, ~str>> {
    for_iter!(iter, component_value, {
        match component_value {
            WhiteSpace | CDO | CDC => (),
            AtKeyword(name) => return Some(Ok(AtRule(parse_at_rule(iter, name)))),
            component_value => return Some(match parse_qualified_rule(iter, component_value) {
                Ok(rule) => Ok(QualifiedRule(rule)),
                Err(reason) => Err(reason),
            }),
        }
    })
    None
}


/// Same as parse_stylesheet() except for the handling of top-level CDO and CDC
/// Used eg. for @media
pub fn parse_rule(iter: &mut ComponentValueIterator) -> Option<Result<Rule, ~str>> {
    for_iter!(iter, component_value, {
        match component_value {
            WhiteSpace => (),
            AtKeyword(name) => return Some(Ok(AtRule(parse_at_rule(iter, name)))),
            component_value => return Some(match parse_qualified_rule(iter, component_value) {
                Ok(rule) => Ok(QualifiedRule(rule)),
                Err(reason) => Err(reason),
            }),
        }
    })
    None
}


/// Used eg. for CSSRuleList.insertRule()
pub fn parse_one_rule(iter: &mut ComponentValueIterator) -> Result<Rule, ~str> {
    match parse_rule(iter) {
        None => Err(~"Input is empty"),
        Some(result) => match iter.next_non_whitespace() {
            // The only possible error in `result` is EOF in a qualified rule without a {} block,
            // which would not trigger Unexpected token.
            Some(_) => Err(~"Unexpected token after parsed rule."),
            None => result,
        }
    }
}


/// @page in CSS 2.1, all declaration lists in level 3
pub fn parse_declaration_or_at_rule(iter: &mut ComponentValueIterator)
                                      -> Option<Result<DeclarationListItem, ~str>> {
    match iter.next_non_whitespace() {
        None => None,
        Some(AtKeyword(name)) => Some(Ok(Decl_AtRule(parse_at_rule(iter, name)))),
        Some(component_value) => match parse_declaration(iter, component_value) {
            Ok(declaration) => Some(Ok(Declaration(declaration))),
            Err(reason) => {
                // Find the end of the declaration
                for iter.advance |v| { if v == Semicolon { break } }
                Some(Err(reason))
            }
        },
    }
}


/// Used eg. in @supports
pub fn parse_one_declaration(iter: &mut ComponentValueIterator) -> Result<Declaration, ~str> {
    match iter.next_non_whitespace() {
        None => Err(~"Input is empty"),
        Some(component_value) => {
            let result = parse_declaration(iter, component_value);
            match result {
                Err(_) => result,
                Ok(_) => match iter.next_non_whitespace() {
                    Some(_) => Err(~"Unexpected token after parsed rule."),
                    None => result,
                }
            }
        }
    }
}


////////////   End of public API.


fn parse_at_rule(iter: &mut ComponentValueIterator, name: ~str) -> AtRule {
    let mut prelude = ~[];
    let mut block = None;
    for_iter!(iter, component_value, {
        match component_value {
            CurlyBraketBlock(content) => { block = Some(content); break },
            Semicolon => break,
            component_value => prelude.push(component_value),
        }
    })
    AtRule {name: name, prelude: prelude, block: block}
}


fn parse_qualified_rule(iter: &mut ComponentValueIterator, first: ComponentValue)
                          -> Result<QualifiedRule, ~str> {
    let mut prelude = ~[first];
    for_iter!(iter, component_value, {
        match component_value {
            CurlyBraketBlock(content)
            => return Ok(QualifiedRule {prelude: replace(&mut prelude, ~[]), block: content}),
            component_value => prelude.push(component_value),
        }
    })
    Err(~"Missing {} block for qualified rule")
}


fn parse_declaration(iter: &mut ComponentValueIterator, first: ComponentValue)
                       -> Result<Declaration, ~str> {
    let name = match first {
        Ident(name) => name,
        _ => return Err(~"Expected an identifier")
    };
    if iter.next_non_whitespace() != Some(Colon) {
        return Err(~"Expected a colon")
    }
    let mut value = ~[];
    let mut important = false;
    for_iter!(iter, component_value, {
        match component_value {
            Semicolon => break,
            Delim('!') => if parse_declaration_important(iter) {
                important = true;
                break
            } else {
                return Err(~"Expected !important, got an invalid ! value")
            },
            component_value => value.push(component_value),
        }
    })
    Ok(Declaration{name: name, value: value, important: important})
}


#[inline]
fn parse_declaration_important(iter: &mut ComponentValueIterator) -> bool {
    let ident_value = match iter.next_non_whitespace() {
        Some(Ident(value)) => value,
        _ => return false,
    };
    if ascii_lower(ident_value) != ~"important" { return false }
    match iter.next_non_whitespace() {
        Some(Semicolon) => true,
        None => true,
        _ => false
    }
}


//#[test]
//fn test_declarations() {
//    fn assert_declarations(
//            input: &str, expected_declarations: &[DeclarationBlockItem],
//            expected_errors: &[~str]) {
//        let mut parser = Parser::from_str(input);
//        check_results(
//            input, parser.parse_declarations(), expected_declarations,
//            parser.errors, expected_errors);
//    }
//    fn decl(name: ~str, value: ~[ComponentValue]) -> DeclarationBlockItem {
//        Declaration(Declaration{name: name, value: value, important: false})
//    }
//    fn important(name: ~str, value: ~[ComponentValue]) -> DeclarationBlockItem {
//        Declaration(Declaration{name: name, value: value, important: true})
//    }

//    assert_declarations("", [], []);
//    assert_declarations(" \n  ;; ", [], []);
//    assert_declarations("color", [], [~"Invalid declaration"]);
//    assert_declarations("color/ red", [], [~"Invalid declaration"]);
//    assert_declarations("/**/ color:", [decl(~"color", ~[])], []);
//    assert_declarations("a /{(;);}b; c:d",
//        [decl(~"c", ~[Ident(~"d")])], [~"Invalid declaration"]);
//    assert_declarations("color /**/: black,red; ;",
//        [decl(~"color", ~[
//            WhiteSpace, Ident(~"black"), Delim(','), Ident(~"red")
//        ])], []);

//    // !important
//    assert_declarations("a:!important /**/; b:c!IMPORTant",
//        [important(~"a", ~[]), important(~"b", ~[Ident(~"c")])], []);
//    assert_declarations("a:!important b:c",
//        [], [~"Invalid declaration"]);
//    assert_declarations("a:!important} b:c",
//        [], [~"Invalid declaration"]);
//    assert_declarations("a:!stuff; a:!!;a:!", [], [
//        ~"Invalid declaration", ~"Invalid declaration", ~"Invalid declaration"
//    ]);
//    // XXX See http://lists.w3.org/Archives/Public/www-style/2013Jan/0491.html
//    assert_declarations(
//        "a:! important; a:!/**/important;a:!/**/ important",
//        [important(~"a", ~[]), important(~"a", ~[]), important(~"a", ~[])],
//        []);
//}
