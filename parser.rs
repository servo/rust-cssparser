// http://dev.w3.org/csswg/css-syntax/#parsing
//
// The input to the tree construction stage is a sequence of tokens
// from the tokenization stage.
// The output is a tree of items with a stylesheet at the root
// and all other nodes being at-rules, style rules, or declarations.


use std::iterator::Iterator;
use std::util::replace;
use std::vec;
use extra::json;

use utils::*;
use ast::*;
use tokenizer::*;


// TODO: Use a trait?
enum ComponentValueIterator {
    ParserIter(~Parser),
    VectorIter(~[ComponentValue]),
}


impl ComponentValueIterator {
    #[inline]
    fn from_str(input: ~str) -> ComponentValueIterator {
        ComponentValueIterator::from_parser(~Parser::from_str(input))
    }

    #[inline]
    fn from_parser(parser: ~Parser) -> ComponentValueIterator {
        ParserIter(parser)
    }

    #[inline]
    fn from_vector(mut values: ~[ComponentValue]) -> ComponentValueIterator {
        // TODO: find a way to have consume_iter() or something instead of reverse() + pop()
        vec::reverse(values);
        VectorIter(values)
    }

    #[inline]
    fn next_non_whitespace(&mut self) -> Option<ComponentValue> {
        for self.advance |component_value| {
            if component_value != WhiteSpace { return Some(component_value) }
        }
        None
    }
}


impl Iterator<ComponentValue> for ComponentValueIterator {
    fn next(&mut self) -> Option<ComponentValue> {
        match self {
            &ParserIter(ref mut parser) => consume_component_value(*parser),
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



pub fn consume_stylesheet_rule(iter: &mut ComponentValueIterator) -> Option<Result<Rule, ~str>> {
    for_iter!(iter, component_value, {
        match component_value {
            WhiteSpace | CDO | CDC => (),
            AtKeyword(name) => return Some(Ok(AtRule(consume_at_rule(iter, name)))),
            component_value => return Some(match consume_qualified_rule(iter, component_value) {
                Ok(rule) => Ok(QualifiedRule(rule)),
                Err(reason) => Err(reason),
            }),
        }
    })
    None
}


/// Same as consume_stylesheet() except for the handling of top-level CDO and CDC
/// Used eg. for @media
pub fn consume_rule(iter: &mut ComponentValueIterator) -> Option<Result<Rule, ~str>> {
    for_iter!(iter, component_value, {
        match component_value {
            WhiteSpace => (),
            AtKeyword(name) => return Some(Ok(AtRule(consume_at_rule(iter, name)))),
            component_value => return Some(match consume_qualified_rule(iter, component_value) {
                Ok(rule) => Ok(QualifiedRule(rule)),
                Err(reason) => Err(reason),
            }),
        }
    })
    None
}


/// Used eg. for CSSRuleList.insertRule()
pub fn consume_one_rule(iter: &mut ComponentValueIterator) -> Result<Rule, ~str> {
    match consume_rule(iter) {
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
pub fn consume_declaration_or_at_rule(iter: &mut ComponentValueIterator)
                                      -> Option<Result<DeclarationListItem, ~str>> {
    match iter.next_non_whitespace() {
        None => None,
        Some(AtKeyword(name)) => Some(Ok(Decl_AtRule(consume_at_rule(iter, name)))),
        Some(component_value) => match consume_declaration(iter, component_value) {
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
pub fn consume_one_declaration(iter: &mut ComponentValueIterator) -> Result<Declaration, ~str> {
    match iter.next_non_whitespace() {
        None => Err(~"Input is empty"),
        Some(component_value) => {
            let result = consume_declaration(iter, component_value);
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


fn consume_at_rule(iter: &mut ComponentValueIterator, name: ~str) -> AtRule {
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


fn consume_qualified_rule(iter: &mut ComponentValueIterator, first: ComponentValue)
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


fn consume_declaration(iter: &mut ComponentValueIterator, first: ComponentValue)
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
            Delim('!') => if consume_declaration_important(iter) {
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
fn consume_declaration_important(iter: &mut ComponentValueIterator) -> bool {
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


#[cfg(test)]
pub fn declaration_to_json(declaration: Declaration) -> ~[json::Json] {
    let Declaration{name: name, value: value, important: important} = declaration;
    ~[json::String(~"declaration"), json::String(name),
      json::List(component_value_list_to_json(value)), json::Boolean(important)]
}


#[test]
fn test_one_declaration_json() {
    for each_json_test(include_str!(
            // https://github.com/SimonSapin/tinycss2/tree/master/tinycss2/tests
            // TODO: use git subtree or something to have the JSON files in this repository.
            "../tinycss2/tinycss2/tests/one_declaration.json"
    )) |input, expected| {
        let parser = ~Parser::from_str(input);
        let mut iter = ComponentValueIterator::from_parser(parser);
        let result = match consume_one_declaration(&mut iter) {
            Ok(declaration) => declaration_to_json(declaration),
            Err(_) => ~[json::String(~"error"), json::String(~"invalid")],
        };
        assert_vec_equals(result, expected, input);
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
