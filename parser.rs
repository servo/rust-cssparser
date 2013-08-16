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
use std::ascii::StrAsciiExt;

use ast::*;


/// Parse top-level of a CSS stylesheet.
/// Return a Iterator<Result<Rule, SyntaxError>>
#[inline]
pub fn parse_stylesheet_rules<T: Iterator<Node>>(iter: T) -> StylesheetParser<T> {
    StylesheetParser{ iter: iter }
}


/// Parse a non-top level list of rules eg. the content of an @media rule.
/// Return a Iterator<Result<Rule, SyntaxError>>
#[inline]
pub fn parse_rule_list<T: Iterator<Node>>(iter: T) -> RuleListParser<T> {
    RuleListParser{ iter: iter }
}


/// Parse a list of declarations and at-rules,
/// like @page in CSS 2.1, all declaration lists in level 3
/// Return a Iterator<Result<DeclarationListItem, SyntaxError>>
#[inline]
pub fn parse_declaration_list<T: Iterator<Node>>(iter: T) -> DeclarationListParser<T> {
    DeclarationListParser{ iter: iter }
}


/// Parse a single rule.
/// Used eg. for CSSRuleList.insertRule()
pub fn parse_one_rule<T: Iterator<Node>>(iter: T) -> Result<Rule, SyntaxError> {
    let mut parser = RuleListParser{ iter: iter };
    match parser.next() {
        None => error(START_LOCATION, ErrEmptyInput),
        Some(result) => {
            if result.is_err() { result }
            else { match next_non_whitespace(&mut parser.iter) {
                None => result,
                Some((_component_value, location)) => error(location, ErrExtraInput),
            }}
        }
    }
}


/// Parse a single declaration (not an at-rule)
/// Used eg. in @supports
pub fn parse_one_declaration<T: Iterator<Node>>(mut iter: T) -> Result<Declaration, SyntaxError> {
    match next_non_whitespace(&mut iter) {
        None => error(START_LOCATION, ErrEmptyInput),
        Some((component_value, location)) => {
            let result = parse_declaration(&mut iter, component_value, location);
            if result.is_err() { result }
            else { match next_non_whitespace(&mut iter) {
                None => result,
                Some((_component_value, location)) => error(location, ErrExtraInput),
            }}
        }
    }
}


/// Parse a single component value.
/// Used eg. in attr(foo, color)
pub fn parse_one_component_value<T: Iterator<Node>>(mut iter: T)
                                -> Result<ComponentValue, SyntaxError> {
    match next_non_whitespace(&mut iter) {
        None => error(START_LOCATION, ErrEmptyInput),
        Some((component_value, _location)) => {
            match next_non_whitespace(&mut iter) {
                None => Ok(component_value),
                Some((_component_value, location)) => error(location, ErrExtraInput),
            }
        }
    }
}


//  ***********  End of public API  ***********


struct StylesheetParser<T>{ iter: T }
struct RuleListParser<T>{ iter: T }
struct DeclarationListParser<T>{ iter: T }


// Work around "error: cannot borrow `*iter` as mutable more than once at a time"
// when using a normal for loop.
macro_rules! for_iter(
    ($iter: ident, $pattern: pat, $loop_body: expr) => (
        loop {
            match $iter.next() { None => break, Some($pattern) => $loop_body }
        }
    );
)


impl<T: Iterator<Node>> Iterator<Result<Rule, SyntaxError>> for StylesheetParser<T> {
    fn next(&mut self) -> Option<Result<Rule, SyntaxError>> {
        let iter = &mut self.iter;
        for_iter!(iter, (component_value, location), {
            match component_value {
                WhiteSpace | CDO | CDC => (),
                AtKeyword(name) => return Some(Ok(AtRule(parse_at_rule(iter, name, location)))),
                _ => return Some(match parse_qualified_rule(iter, component_value, location) {
                    Ok(rule) => Ok(QualifiedRule(rule)),
                    Err(e) => Err(e),
                }),
            }
        })
        None
    }
}


impl<T: Iterator<Node>> Iterator<Result<Rule, SyntaxError>> for RuleListParser<T> {
    fn next(&mut self) -> Option<Result<Rule, SyntaxError>> {
        let iter = &mut self.iter;
        for_iter!(iter, (component_value, location), {
            match component_value {
                WhiteSpace => (),
                AtKeyword(name) => return Some(Ok(AtRule(parse_at_rule(iter, name, location)))),
                _ => return Some(match parse_qualified_rule(iter, component_value, location) {
                    Ok(rule) => Ok(QualifiedRule(rule)),
                    Err(e) => Err(e),
                }),
            }
        })
        None
    }
}


impl<T: Iterator<Node>> Iterator<Result<DeclarationListItem, SyntaxError>>
for DeclarationListParser<T> {
    fn next(&mut self) -> Option<Result<DeclarationListItem, SyntaxError>> {
        let iter = &mut self.iter;
        for_iter!(iter, (component_value, location), {
            match component_value {
                WhiteSpace | Semicolon => (),
                AtKeyword(name)
                => return Some(Ok(Decl_AtRule(parse_at_rule(iter, name, location)))),
                _ => return Some(match parse_declaration(iter, component_value, location) {
                    Ok(declaration) => Ok(Declaration(declaration)),
                    Err(e) => {
                        // Find the end of the declaration
                        for (v, _) in *iter { if v == Semicolon { break } }
                        Err(e)
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
                                           -> Result<QualifiedRule, SyntaxError> {
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
    error(location, ErrMissingQualifiedRuleBlock)
}


fn parse_declaration<T: Iterator<Node>>(iter: &mut T, first: ComponentValue,
                                        location: SourceLocation)
                                        -> Result<Declaration, SyntaxError> {
    let name = match first {
        Ident(name) => name,
        _ => return error(location, ErrInvalidDeclarationSyntax)
    };
    match next_non_whitespace(iter) {
        Some((Colon, _)) => (),
        _ => return error(location, ErrInvalidDeclarationSyntax),
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
                return error(location, ErrInvalidBangImportantSyntax)
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
    if !ident_value.eq_ignore_ascii_case("important") { return false }
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


#[inline]
fn error<T>(location: SourceLocation, reason: ErrorReason) -> Result<T, SyntaxError> {
    Err(SyntaxError{location: location, reason: reason})
}


// When parsing one thing on an empty input
static START_LOCATION: SourceLocation = SourceLocation{ line: 1, column: 1 };
