/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use std::str;

use encoding::label::encoding_from_whatwg_label;
use encoding::all::UTF_8;
use encoding::{EncodingObj, DecodeReplace, decode};

use tokenizer::{tokenize, Tokenizer};
use parser::{parse_stylesheet_rules, StylesheetParser};


/// Determine the character encoding of a CSS stylesheet and decode it.
///
/// This is based on the presence of a :abbr:`BOM (Byte Order Mark)`,
/// an `@charset` rule,
/// and encoding meta-information.
///
/// :param css_bytes: A byte string.
/// :param protocol_encoding:
///     The encoding label, if any, defined by HTTP or equivalent protocol.
///     (e.g. via the `charset` parameter of the `Content-Type` header.)
/// :param environment_encoding:
///     An optional `Encoding` object
///     for the `environment encoding
///     <http://www.w3.org/TR/css-syntax/#environment-encoding>`_,
///     if any.
/// :returns:
///     A 2-tuple of a decoded Unicode string
///     and the `Encoding` object that was used.
pub fn decode_stylesheet_bytes(css: &[u8], protocol_encoding_label: Option<&str>,
                               environment_encoding: Option<EncodingObj>)
                            -> (~str, EncodingObj) {
    // http://dev.w3.org/csswg/css-syntax/#the-input-byte-stream
    match protocol_encoding_label {
        None => (),
        Some(label) => match encoding_from_whatwg_label(label) {
            None => (),
            Some(fallback) => return decode_replace(css, fallback)
        }
    }
    if css.starts_with("@charset \"".as_bytes()) {
        // 10 is "@charset \"".len()
        // 100 is arbitrary so that no encoding label is more than 100-10 bytes.
        match css.slice(10, css.len().min(&100)).position_elem(&('"' as u8)) {
            None => (),
            Some(label_length)
            => if css.slice_from(10 + label_length).starts_with("\";".as_bytes()) {
                let label = css.slice(10, 10 + label_length);
                let label = str::from_chars(label.iter().map(|&b| b as char).to_owned_vec());
                match encoding_from_whatwg_label(label) {
                    None => (),
                    Some(fallback) => match fallback.name() {
                        "utf-16be" | "utf-16le"
                        => return decode_replace(css, UTF_8 as EncodingObj),
                        _ => return decode_replace(css, fallback),
                    }
                }
            }
        }
    }
    match environment_encoding {
        None => (),
        Some(fallback) => return decode_replace(css, fallback)
    }
    return decode_replace(css, UTF_8 as EncodingObj)
}


#[inline]
fn decode_replace(input: &[u8], fallback_encoding: EncodingObj)-> (~str, EncodingObj) {
    let (result, used_encoding) = decode(input, DecodeReplace, fallback_encoding);
    (result.unwrap(), used_encoding)
}


/// Parse stylesheet from bytes.
///
/// :param css_bytes: A byte string.
/// :param protocol_encoding:
///     The encoding label, if any, defined by HTTP or equivalent protocol.
///     (e.g. via the `charset` parameter of the `Content-Type` header.)
/// :param environment_encoding:
///     An optional `Encoding` object
///     for the `environment encoding
///     <http://www.w3.org/TR/css-syntax/#environment-encoding>`_,
///     if any.
/// :returns:
///     A 2-tuple of a Iterator<Result<Rule, SyntaxError>>
///     and the `Encoding` object that was used.
pub fn parse_stylesheet_rules_from_bytes(
        css_bytes: &[u8], protocol_encoding_label: Option<&str>,
        environment_encoding: Option<EncodingObj>)
     -> (StylesheetParser<Tokenizer>, EncodingObj) {
    let (css_unicode, encoding) = decode_stylesheet_bytes(
        css_bytes, protocol_encoding_label, environment_encoding);
    (parse_stylesheet_rules(tokenize(css_unicode)), encoding)
}
