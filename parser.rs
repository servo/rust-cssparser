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
use super::eq_ascii_lower;


// TODO: Use a trait?
enum ComponentValueIterator {
    ParserIter(~Parser),
    VectorIter(vec::ConsumeIterator<(ComponentValue, SourceLocation)>),
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
    pub fn from_vector(values: ~[(ComponentValue, SourceLocation)]) -> ComponentValueIterator {
        VectorIter(values.consume_iter())
    }

    #[inline]
    pub fn next_non_whitespace(&mut self) -> Option<(ComponentValue, SourceLocation)> {
        for (component_value, location) in *self {
            if component_value != WhiteSpace { return Some((component_value, location)) }
        }
        None
    }
}


impl Iterator<(ComponentValue, SourceLocation)> for ComponentValueIterator {
    fn next(&mut self) -> Option<(ComponentValue, SourceLocation)> {
        match self {
            &ParserIter(ref mut parser) => next_component_value(*parser),
            &VectorIter(ref mut iter) => iter.next()
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
    for_iter!(iter, (component_value, location), {
        match component_value {
            WhiteSpace | CDO | CDC => (),
            AtKeyword(name) => return Some(Ok(AtRule(parse_at_rule(iter, name, location)))),
            _ => return Some(match parse_qualified_rule(iter, (component_value, location)) {
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
    for_iter!(iter, (component_value, location), {
        match component_value {
            WhiteSpace => (),
            AtKeyword(name) => return Some(Ok(AtRule(parse_at_rule(iter, name, location)))),
            _ => return Some(match parse_qualified_rule(iter, (component_value, location)) {
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
    for_iter!(iter, (component_value, location), {
        match component_value {
            WhiteSpace | Semicolon => (),
            AtKeyword(name) => return Some(Ok(Decl_AtRule(parse_at_rule(iter, name, location)))),
            _ => return Some(match parse_declaration(iter, (component_value, location)) {
                Ok(declaration) => Ok(Declaration(declaration)),
                Err(reason) => {
                    // Find the end of the declaration
                    for (v, _) in *iter { if v == Semicolon { break } }
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
        Some(item) => {
            let result = parse_declaration(iter, item);
            if result.is_err() || iter.next_non_whitespace().is_none() { result }
            else { Err(ErrExtraInput) }
        }
    }
}


/// Used eg. in attr(foo, color)
pub fn parse_one_component_value(iter: &mut ComponentValueIterator)
                                 -> Result<(ComponentValue, SourceLocation), ErrorReason> {
    match iter.next_non_whitespace() {
        None => Err(ErrEmptyInput),
        Some(item) => {
            if iter.next_non_whitespace().is_none() { Ok(item) }
            else { Err(ErrExtraInput) }
        }
    }
}


//  ***********  End of public API  ***********


fn parse_at_rule(iter: &mut ComponentValueIterator, name: ~str, location: SourceLocation)
                 -> AtRule {
    let mut prelude = ~[];
    let mut block = None;
    for_iter!(iter, (component_value, location), {
        match component_value {
            CurlyBracketBlock(content) => { block = Some(content); break },
            Semicolon => break,
            component_value => prelude.push((component_value, location)),
        }
    })
    AtRule {location: location, name: name, prelude: prelude, block: block}
}


fn parse_qualified_rule(iter: &mut ComponentValueIterator, first: (ComponentValue, SourceLocation))
                          -> Result<QualifiedRule, ErrorReason> {
    match first {
        (CurlyBracketBlock(content), location)
        => return Ok(QualifiedRule { location: location, prelude: ~[], block: content }),
        _ => (),
    }
    let mut prelude = ~[first];
    for_iter!(iter, (component_value, location), {
        match component_value {
            CurlyBracketBlock(content)
            => return Ok(QualifiedRule {location: location, prelude: prelude, block: content}),
            component_value => prelude.push((component_value, location)),
        }
    })
    Err(ErrMissingQualifiedRuleBlock)
}


fn parse_declaration(iter: &mut ComponentValueIterator, first: (ComponentValue, SourceLocation))
                       -> Result<Declaration, ErrorReason> {
    let (name, location) = match first {
        (Ident(name), location) => (name, location),
        _ => return Err(ErrInvalidDeclarationSyntax)
    };
    match iter.next_non_whitespace() {
        Some((Colon, _)) => (),
        _ => return Err(ErrInvalidDeclarationSyntax),
    }
    let mut value = ~[];
    let mut important = false;
    for_iter!(iter, (component_value, location), {
        match component_value {
            Semicolon => break,
            Delim('!') => if parse_declaration_important(iter) {
                important = true;
                break
            } else {
                return Err(ErrInvalidBangImportantSyntax)
            },
            component_value => value.push((component_value, location)),
        }
    })
    Ok(Declaration{location: location, name: name, value: value, important: important})
}


#[inline]
fn parse_declaration_important(iter: &mut ComponentValueIterator) -> bool {
    let ident_value = match iter.next_non_whitespace() {
        Some((Ident(value), _)) => value,
        _ => return false,
    };
    if !eq_ascii_lower(ident_value, "important") { return false }
    match iter.next_non_whitespace() {
        Some((Semicolon, _)) => true,
        None => true,
        _ => false
    }
}
