/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

// http://dev.w3.org/csswg/css-syntax/#parsing

use std::ascii::AsciiExt;
use std::str::CowString;
use super::{Token, Parser, Delimiter};


#[deriving(Copy, Eq, PartialEq)]
pub enum Priority {
    Normal,
    Important,
}


pub fn parse_important(input: &mut Parser) -> Result<Priority, ()> {
    match input.next() {
        Ok(Token::Delim('!')) => {
            match try!(input.next()) {
                Token::Ident(ref value) if value.eq_ignore_ascii_case("important") => {
                    Ok(Priority::Important)
                }
                token => input.unexpected(token)
            }
        }
        token => {
            input.push_back_result(token);
            Ok(Priority::Normal)
        }
    }
}


pub enum AtRulePrelude<P, R> {
    WithoutBlock(R),
    WithBlock(P),
    OptionalBlock(P),
}


// FIXME: Use associated types
pub trait DeclarationParser<D> {
    fn parse_value(&mut self, name: &str, input: &mut Parser) -> Result<D, ()> {
        let _ = name;
        let _ = input;
        Err(())
    }
}

pub trait AtRuleParser<P, R> {
    fn parse_prelude(&mut self, name: &str, input: &mut Parser)
                     -> Result<AtRulePrelude<P, R>, ()> {
        let _ = name;
        let _ = input;
        Err(())
    }

    fn parse_block(&mut self, prelude: P, input: &mut Parser) -> Result<R, ()> {
        let _ = prelude;
        let _ = input;
        Err(())
    }

    /// An `OptionalBlock` prelude was followed by `;`
    fn rule_without_block(&mut self, prelude: P) -> Result<R, ()> {
        let _ = prelude;
        Err(())
    }
}

pub trait QualifiedRuleParser<P, R> {
    fn parse_prelude(&mut self, input: &mut Parser) -> Result<P, ()> {
        let _ = input;
        Err(())
    }

    fn parse_block(&mut self, prelude: P, input: &mut Parser) -> Result<R, ()> {
        let _ = prelude;
        let _ = input;
        Err(())
    }
}


pub struct DeclarationListParser<'i: 't, 't: 'a, 'a, AP, I, P>
where P: DeclarationParser<I> + AtRuleParser<AP, I> {
    pub input: &'a mut Parser<'i, 't>,
    pub parser: P,
}


impl<'i, 't, 'a, AP, I, P> DeclarationListParser<'i, 't, 'a, AP, I, P>
where P: DeclarationParser<I> + AtRuleParser<AP, I> {
    pub fn new(input: &'a mut Parser<'i, 't>, parser: P)
               -> DeclarationListParser<'i, 't, 'a, AP, I, P> {
        DeclarationListParser {
            input: input,
            parser: parser,
        }
    }

    /// Parse until the input is exhausted, and ignore all results.
    /// This can be useful when `parser` collects results by mutating itself.
    pub fn run(mut self) -> P {
        while let Some(_) = self.next() {}
        self.parser
    }
}

impl<'i, 't, 'a, AP, I, P> Iterator<Result<I, ()>>
for DeclarationListParser<'i, 't, 'a, AP, I, P>
where P: DeclarationParser<I> + AtRuleParser<AP, I> {
    fn next(&mut self) -> Option<Result<I, ()>> {
        loop {
            match self.input.next() {
                Ok(Token::Semicolon) => {}
                Ok(Token::Ident(name)) => {
                    return Some(parse_declaration(name, self.input, &mut self.parser))
                }
                Ok(Token::AtKeyword(name)) => {
                    return Some(parse_at_rule(name, self.input, &mut self.parser))
                }
                Ok(_) => {
                    return Some(self.input.err_consume_until_after(Delimiter::Semicolon))
                }
                Err(()) => return None,
            }
        }
    }
}


pub struct RuleListParser<'i: 't, 't: 'a, 'a, R, QP, AP, P>
where P: QualifiedRuleParser<QP, R> + AtRuleParser<AP, R> {
    pub input: &'a mut Parser<'i, 't>,
    pub parser: P,
    is_stylesheet: bool,
}


impl<'i: 't, 't: 'a, 'a, R, QP, AP, P> RuleListParser<'i, 't, 'a, R, QP, AP, P>
where P: QualifiedRuleParser<QP, R> + AtRuleParser<AP, R> {
    pub fn new_for_stylesheet(input: &'a mut Parser<'i, 't>, parser: P)
                              -> RuleListParser<'i, 't, 'a, R, QP, AP, P> {
        RuleListParser {
            input: input,
            parser: parser,
            is_stylesheet: true,
        }
    }

    pub fn new_for_nested_rule(input: &'a mut Parser<'i, 't>, parser: P)
                               -> RuleListParser<'i, 't, 'a, R, QP, AP, P> {
        RuleListParser {
            input: input,
            parser: parser,
            is_stylesheet: false,
        }
    }

    /// Parse until the input is exhausted, and ignore all results.
    /// This can be useful when `parser` collects results by mutating itself.
    pub fn run(mut self) -> P {
        while let Some(_) = self.next() {}
        self.parser
    }
}



impl<'i, 't, 'a, R, QP, AP, P> Iterator<Result<R, ()>>
for RuleListParser<'i, 't, 'a, R, QP, AP, P>
where P: QualifiedRuleParser<QP, R> + AtRuleParser<AP, R> {
    fn next(&mut self) -> Option<Result<R, ()>> {
        loop {
            match self.input.next() {
                Ok(Token::CDO) | Ok(Token::CDC) if self.is_stylesheet => {}
                Ok(Token::AtKeyword(name)) => {
                    return Some(parse_at_rule(name, self.input, &mut self.parser))
                }
                Ok(token) => {
                    self.input.push_back(token);
                    return Some(parse_qualified_rule(self.input, &mut self.parser))
                }
                Err(()) => return None,
            }
        }
    }
}

pub fn parse_one_declaration<D, P>(input: &mut Parser, parser: &mut P)
                                   -> Result<D, ()>
                                   where P: DeclarationParser<D> {
    input.parse_entirely(|input| {
        let name = try!(input.expect_ident());
        try!(input.expect_colon());
        parser.parse_value(name.as_slice(), input)
    })
}


pub fn parse_one_rule<QP, AP, R, P>(input: &mut Parser, parser: &mut P)
                                    -> Result<R, ()>
                                    where P: QualifiedRuleParser<QP, R> + AtRuleParser<AP, R> {
    input.parse_entirely(|input| {
        match try!(input.next()) {
            Token::AtKeyword(name) => {
                parse_at_rule(name, input, parser)
            }
            token => {
                input.push_back(token);
                parse_qualified_rule(input, parser)
            }
        }
    })
}


fn parse_declaration<D, P>(name: CowString, input: &mut Parser, parser: &mut P)
                           -> Result<D, ()>
                           where P: DeclarationParser<D> {
    let result = input.parse_until_before(Delimiter::Semicolon).parse_entirely(|input| {
        try!(input.expect_colon());
        parser.parse_value(name.as_slice(), input)
    });
    match input.next() {
        Ok(Token::Semicolon) | Err(()) => result,
        _ => input.err_consume_until_after(Delimiter::Semicolon)
    }
}


fn parse_at_rule<R, AP, P>(name: CowString, input: &mut Parser, parser: &mut P)
                           -> Result<R, ()>
                           where P: AtRuleParser<AP, R> {
    let delimiters = Delimiter::Semicolon | Delimiter::CurlyBracketBlock;
    let result = try!(input.parse_until_before(delimiters).parse_entirely(|input| {
        parser.parse_prelude(name.as_slice(), input)
    }).or_else(|()| input.err_consume_until_after(delimiters)));
    match result {
        AtRulePrelude::WithoutBlock(rule) => {
            match input.next() {
                Ok(Token::Semicolon) | Err(()) => Ok(rule),
                _ => input.err_consume_until_after(delimiters)
            }
        }
        AtRulePrelude::WithBlock(prelude) => {
            match input.next() {
                Ok(Token::CurlyBracketBlock) => {
                    // FIXME: Make parse_entirely take `FnOnce`
                    // and remove this Option dance.
                    let mut prelude = Some(prelude);
                    input.parse_nested_block().parse_entirely(|input| {
                        parser.parse_block(prelude.take().unwrap(), input)
                    })
                }
                _ => input.err_consume_until_after(delimiters)
            }
        }
        AtRulePrelude::OptionalBlock(prelude) => {
            match input.next() {
                Ok(Token::Semicolon) | Err(()) => parser.rule_without_block(prelude),
                Ok(Token::CurlyBracketBlock) => {
                    // FIXME: Make parse_entirely take `FnOnce`
                    // and remove this Option dance.
                    let mut prelude = Some(prelude);
                    input.parse_nested_block().parse_entirely(|input| {
                        parser.parse_block(prelude.take().unwrap(), input)
                    })
                }
                _ => input.err_consume_until_after(delimiters)
            }
        }
    }
}


fn parse_qualified_rule<R, AP, P>(input: &mut Parser, parser: &mut P)
                                  -> Result<R, ()>
                                  where P: QualifiedRuleParser<AP, R> {
    let prelude = try!(input.parse_until_before(Delimiter::CurlyBracketBlock)
                            .parse_entirely(|input| {
        parser.parse_prelude(input)
    }).or_else(|()| input.err_consume_until_after(Delimiter::CurlyBracketBlock)));
    match input.next() {
        Ok(Token::CurlyBracketBlock) => {
            // FIXME: Make parse_entirely take `FnOnce`
            // and remove this Option dance.
            let mut prelude = Some(prelude);
            input.parse_nested_block().parse_entirely(|input| {
                parser.parse_block(prelude.take().unwrap(), input)
            })
        }
        _ => input.err_consume_until_after(Delimiter::CurlyBracketBlock)
    }
}
