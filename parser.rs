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
use std::ascii::eq_ignore_ascii_case;

use ast::*;


// Work around "error: cannot borrow `*iter` as mutable more than once at a time"
// when using a normal for loop.
macro_rules! for_iter(
    ($iter: ident, $pattern: pat, $loop_body: expr) => (
        loop {
            match $iter.next() { None => break, Some($pattern) => $loop_body }
        }
    );
)


/// Call repeatedly for the top-level of a CSS stylesheet
pub fn parse_stylesheet_rule<T: Iterator<Node>>(iter: &mut T) -> Option<Result<Rule, ErrorReason>> {
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
pub fn parse_rule<T: Iterator<Node>>(iter: &mut T) -> Option<Result<Rule, ErrorReason>> {
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
pub fn parse_one_rule<T: Iterator<Node>>(iter: &mut T) -> Result<Rule, ErrorReason> {
    match parse_rule(iter) {
        None => Err(ErrEmptyInput),
        Some(result) => if result.is_err() || next_non_whitespace(iter).is_none() { result }
                        else { Err(ErrExtraInput) }
    }
}


/// Call repeatedly of a list of declarations.
/// @page in CSS 2.1, all declaration lists in level 3
pub fn parse_declaration_or_at_rule<T: Iterator<Node>>(iter: &mut T)
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
pub fn parse_one_declaration<T: Iterator<Node>>(iter: &mut T) -> Result<Declaration, ErrorReason> {
    match next_non_whitespace(iter) {
        None => Err(ErrEmptyInput),
        Some(item) => {
            let result = parse_declaration(iter, item);
            if result.is_err() || next_non_whitespace(iter).is_none() { result }
            else { Err(ErrExtraInput) }
        }
    }
}


/// Used eg. in attr(foo, color)
pub fn parse_one_component_value<T: Iterator<Node>>(iter: &mut T) -> Result<Node, ErrorReason> {
    match next_non_whitespace(iter) {
        None => Err(ErrEmptyInput),
        Some(item) => {
            if next_non_whitespace(iter).is_none() { Ok(item) }
            else { Err(ErrExtraInput) }
        }
    }
}


//  ***********  End of public API  ***********


fn parse_at_rule<T: Iterator<Node>>(iter: &mut T, name: ~str, location: SourceLocation)
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


fn parse_qualified_rule<T: Iterator<Node>>(iter: &mut T, first: Node)
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


fn parse_declaration<T: Iterator<Node>>(iter: &mut T, first: Node)
                    -> Result<Declaration, ErrorReason> {
    let (name, location) = match first {
        (Ident(name), location) => (name, location),
        _ => return Err(ErrInvalidDeclarationSyntax)
    };
    match next_non_whitespace(iter) {
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
fn parse_declaration_important<T: Iterator<Node>>(iter: &mut T) -> bool {
    let ident_value = match next_non_whitespace(iter) {
        Some((Ident(value), _)) => value,
        _ => return false,
    };
    if !eq_ignore_ascii_case(ident_value, "important") { return false }
    match next_non_whitespace(iter) {
        Some((Semicolon, _)) => true,
        None => true,
        _ => false
    }
}


#[inline]
fn next_non_whitespace<T: Iterator<Node>>(iter: &mut T) -> Option<Node> {
    for (component_value, location) in *iter {
        if component_value != WhiteSpace { return Some((component_value, location)) }
    }
    None
}
