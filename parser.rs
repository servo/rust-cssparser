/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

// http://dev.w3.org/csswg/css-syntax/#parsing
//
// The input to the tree construction stage is a sequence of tokens
// from the tokenization stage.
// The output is a tree of items with a stylesheet at the root
// and all other nodes being at-rules, style rules, or declarations.


use std::iterator::Iterator;
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


/// Call repeatedly for the top-level of a CSS stylesheet
pub fn parse_stylesheet_rule(iter: &mut ComponentValueIterator) -> Option<Result<Rule, ErrorReason>> {
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


/// Call repeatedly for a non-top level list of rules eg. the content of an @media rule.
/// Same as parse_stylesheet() except for the handling of top-level CDO and CDC
pub fn parse_rule(iter: &mut ComponentValueIterator) -> Option<Result<Rule, ErrorReason>> {
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
pub fn parse_one_rule(iter: &mut ComponentValueIterator) -> Result<Rule, ErrorReason> {
    match parse_rule(iter) {
        None => Err(ErrEmptyInput),
        Some(result) => if result.is_err() || iter.next_non_whitespace().is_none() { result }
                        else { Err(ErrExtraInput) }
    }
}


/// Call repeatedly of a list of declarations.
/// @page in CSS 2.1, all declaration lists in level 3
pub fn parse_declaration_or_at_rule(iter: &mut ComponentValueIterator)
                                      -> Option<Result<DeclarationListItem, ErrorReason>> {
    for_iter!(iter, component_value, {
        match component_value {
            WhiteSpace | Semicolon => (),
            AtKeyword(name) => return Some(Ok(Decl_AtRule(parse_at_rule(iter, name)))),
            component_value => return Some(match parse_declaration(iter, component_value) {
                Ok(declaration) => Ok(Declaration(declaration)),
                Err(reason) => {
                    // Find the end of the declaration
                    for iter.advance |v| { if v == Semicolon { break } }
                    Err(reason)
                }
            }),
        }
    })
    None
}


/// Used eg. in @supports
pub fn parse_one_declaration(iter: &mut ComponentValueIterator) -> Result<Declaration, ErrorReason> {
    match iter.next_non_whitespace() {
        None => Err(ErrEmptyInput),
        Some(component_value) => {
            let result = parse_declaration(iter, component_value);
            if result.is_err() || iter.next_non_whitespace().is_none() { result }
            else { Err(ErrExtraInput) }
        }
    }
}


//  ***********  End of public API  ***********


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
                          -> Result<QualifiedRule, ErrorReason> {
    match first {
        CurlyBraketBlock(content) => return Ok(QualifiedRule { prelude: ~[], block: content}),
        _ => (),
    }
    let mut prelude = ~[first];
    for_iter!(iter, component_value, {
        match component_value {
            CurlyBraketBlock(content)
            => return Ok(QualifiedRule {prelude: prelude, block: content}),
            component_value => prelude.push(component_value),
        }
    })
    Err(ErrMissingQualifiedRuleBlock)
}


fn parse_declaration(iter: &mut ComponentValueIterator, first: ComponentValue)
                       -> Result<Declaration, ErrorReason> {
    let name = match first {
        Ident(name) => name,
        _ => return Err(ErrInvalidDeclarationSyntax)
    };
    if iter.next_non_whitespace() != Some(Colon) {
        return Err(ErrInvalidDeclarationSyntax)
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
                return Err(ErrInvalidBangImportantSyntax)
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
