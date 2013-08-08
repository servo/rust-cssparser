/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

// http://dev.w3.org/csswg/css-syntax/#parsing

/// The input to these functions needs to implement Iterator<(ComponentValue, SourceLocation)>.
/// The input is consumed to avoid doing a lot of copying.
/// A conforming input can be obtained:
///
/// * From a string in CSS syntax, with tokenize()
/// * From a ~[(ComponentValue, SourceLocation)] vector
///   (as found in "nested" component values such as CurlyBracketBlock),
///   with v.consume_iter()


use std::iterator::Iterator;
use std::ascii::eq_ignore_ascii_case;

use ast::*;


/// Parse top-level of a CSS stylesheet.
/// Return a Iterator<Result<Rule, ErrorReason>>
#[inline]
pub fn parse_stylesheet_rules<T: Iterator<Node>>(iter: T) -> StylesheetParser<T> {
    StylesheetParser(iter)
}


/// Parse a non-top level list of rules eg. the content of an @media rule.
/// Return a Iterator<Result<Rule, ErrorReason>>
#[inline]
pub fn parse_rule_list<T: Iterator<Node>>(iter: T) -> RuleListParser<T> {
    RuleListParser(iter)
}


/// Parse a list of declarations and at-rules,
/// like @page in CSS 2.1, all declaration lists in level 3
/// Return a Iterator<Result<DeclarationListItem, ErrorReason>>
#[inline]
pub fn parse_declaration_list<T: Iterator<Node>>(iter: T) -> DeclarationListParser<T> {
    DeclarationListParser(iter)
}


/// Parse a single rule.
/// Used eg. for CSSRuleList.insertRule()
pub fn parse_one_rule<T: Iterator<Node>>(iter: T) -> Result<Rule, ErrorReason> {
    let mut parser = RuleListParser(iter);
    match parser.next() {
        None => Err(ErrEmptyInput),
        Some(result) => {
            if result.is_err() || next_non_whitespace(&mut *parser).is_none() { result }
            else { Err(ErrExtraInput) }
        }
    }
}


/// Parse a single declaration (not an at-rule)
/// Used eg. in @supports
pub fn parse_one_declaration<T: Iterator<Node>>(mut iter: T) -> Result<Declaration, ErrorReason> {
    match next_non_whitespace(&mut iter) {
        None => Err(ErrEmptyInput),
        Some((component_value, location)) => {
            let result = parse_declaration(&mut iter, component_value, location);
            if result.is_err() || next_non_whitespace(&mut iter).is_none() { result }
            else { Err(ErrExtraInput) }
        }
    }
}


/// Parse a single component value.
/// Used eg. in attr(foo, color)
pub fn parse_one_component_value<T: Iterator<Node>>(mut iter: T)
                                -> Result<ComponentValue, ErrorReason> {
    match next_non_whitespace(&mut iter) {
        None => Err(ErrEmptyInput),
        Some((component_value, _location)) => {
            if next_non_whitespace(&mut iter).is_none() { Ok(component_value) }
            else { Err(ErrExtraInput) }
        }
    }
}


//  ***********  End of public API  ***********


struct StylesheetParser<T>(T);
struct RuleListParser<T>(T);
struct DeclarationListParser<T>(T);


// Work around "error: cannot borrow `*iter` as mutable more than once at a time"
// when using a normal for loop.
macro_rules! for_iter(
    ($iter: ident, $pattern: pat, $loop_body: expr) => (
        loop {
            match $iter.next() { None => break, Some($pattern) => $loop_body }
        }
    );
)


impl<T: Iterator<Node>> Iterator<Result<Rule, ErrorReason>> for StylesheetParser<T> {
    fn next(&mut self) -> Option<Result<Rule, ErrorReason>> {
        let iter = &mut **self;
        for_iter!(iter, (component_value, location), {
            match component_value {
                WhiteSpace | CDO | CDC => (),
                AtKeyword(name) => return Some(Ok(AtRule(parse_at_rule(iter, name, location)))),
                _ => return Some(match parse_qualified_rule(iter, component_value, location) {
                    Ok(rule) => Ok(QualifiedRule(rule)),
                    Err(reason) => Err(reason),
                }),
            }
        })
        None
    }
}


impl<T: Iterator<Node>> Iterator<Result<Rule, ErrorReason>> for RuleListParser<T> {
    fn next(&mut self) -> Option<Result<Rule, ErrorReason>> {
        let iter = &mut **self;
        for_iter!(iter, (component_value, location), {
            match component_value {
                WhiteSpace => (),
                AtKeyword(name) => return Some(Ok(AtRule(parse_at_rule(iter, name, location)))),
                _ => return Some(match parse_qualified_rule(iter, component_value, location) {
                    Ok(rule) => Ok(QualifiedRule(rule)),
                    Err(reason) => Err(reason),
                }),
            }
        })
        None
    }
}


impl<T: Iterator<Node>> Iterator<Result<DeclarationListItem, ErrorReason>>
for DeclarationListParser<T> {
    fn next(&mut self) -> Option<Result<DeclarationListItem, ErrorReason>> {
        let iter = &mut **self;
        for_iter!(iter, (component_value, location), {
            match component_value {
                WhiteSpace | Semicolon => (),
                AtKeyword(name)
                => return Some(Ok(Decl_AtRule(parse_at_rule(iter, name, location)))),
                _ => return Some(match parse_declaration(iter, component_value, location) {
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
}


fn parse_at_rule<T: Iterator<Node>>(iter: &mut T, name: ~str, location: SourceLocation)
                 -> AtRule {
    let mut prelude = ~[];
    let mut block = None;
    for_iter!(iter, (component_value, _location), {
        match component_value {
            CurlyBracketBlock(content) => { block = Some(content); break },
            Semicolon => break,
            component_value => prelude.push(component_value),
        }
    })
    AtRule {location: location, name: name, prelude: prelude, block: block}
}


fn parse_qualified_rule<T: Iterator<Node>>(iter: &mut T, first: ComponentValue,
                                           location: SourceLocation)
                                           -> Result<QualifiedRule, ErrorReason> {
    match first {
        CurlyBracketBlock(content)
        => return Ok(QualifiedRule { location: location, prelude: ~[], block: content }),
        _ => (),
    }
    let mut prelude = ~[first];
    for_iter!(iter, (component_value, _location), {
        match component_value {
            CurlyBracketBlock(content)
            => return Ok(QualifiedRule {location: location, prelude: prelude, block: content}),
            component_value => prelude.push(component_value),
        }
    })
    Err(ErrMissingQualifiedRuleBlock)
}


fn parse_declaration<T: Iterator<Node>>(iter: &mut T, first: ComponentValue,
                                        location: SourceLocation)
                                        -> Result<Declaration, ErrorReason> {
    let name = match first {
        Ident(name) => name,
        _ => return Err(ErrInvalidDeclarationSyntax)
    };
    match next_non_whitespace(iter) {
        Some((Colon, _)) => (),
        _ => return Err(ErrInvalidDeclarationSyntax),
    }
    let mut value = ~[];
    let mut important = false;
    for_iter!(iter, (component_value, _location), {
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
