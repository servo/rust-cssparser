/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use std::libc::c_float;
use std::ascii::StrAsciiExt;

use ast::*;
use self::color_data::{COLOR_KEYWORDS, COLOR_VALUES};

mod color_data;


// Try to match rust-azure’s AzFloat
pub type ColorFloat = c_float;


#[deriving(Clone, Eq)]
pub struct RGBA {
    // All in 0..1
    red: ColorFloat,
    green: ColorFloat,
    blue: ColorFloat,
    alpha: ColorFloat,
}

#[deriving(Clone, Eq)]
pub enum Color {
    CurrentColor,
    RGBA(RGBA),
}


// Return None on invalid/unsupported value (not a color)
impl Color {
    pub fn parse(component_value: &ComponentValue) -> Option<Color> {
        match *component_value {
            Hash(ref value) | IDHash(ref value) => parse_color_hash(*value),
            Ident(ref value) => parse_color_keyword(*value),
            Function(ref name, ref arguments) => parse_color_function(*name, *arguments),
            _ => None
        }
    }
}


#[inline]
fn parse_color_keyword(value: &str) -> Option<Color> {
    let lower_value = value.to_ascii_lower();
    match COLOR_KEYWORDS.bsearch_elem(&lower_value.as_slice()) {
        Some(index) => Some(RGBA(COLOR_VALUES[index])),
        None => if "currentcolor" == lower_value { Some(CurrentColor) }
                else { None }
    }
}


#[inline]
fn parse_color_hash(value: &str) -> Option<Color> {
    macro_rules! from_hex(
        ($c: expr) => {{
            let c = $c;
            match c as char {
                '0' .. '9' => c - ('0' as u8),
                'a' .. 'f' => c - ('a' as u8) + 10,
                'A' .. 'F' => c - ('A' as u8) + 10,
                _ => return None  // Not a valid color
            }
        }};
    )
    macro_rules! to_rgba(
        ($r: expr, $g: expr, $b: expr,) => {
            Some(RGBA(RGBA { red: $r as ColorFloat / 255.,
                             green: $g as ColorFloat / 255.,
                             blue: $b as ColorFloat / 255.,
                             alpha: 1. }))
        };
    )

    match value.len() {
        6 => to_rgba!(
            from_hex!(value[0]) * 16 + from_hex!(value[1]),
            from_hex!(value[2]) * 16 + from_hex!(value[3]),
            from_hex!(value[4]) * 16 + from_hex!(value[5]),
        ),
        3 => to_rgba!(
            from_hex!(value[0]) * 17,
            from_hex!(value[1]) * 17,
            from_hex!(value[2]) * 17,
        ),
        _ => None
    }
}


#[inline]
fn parse_color_function(name: &str, arguments: &[ComponentValue])
                        -> Option<Color> {
    let lower_name = name.to_ascii_lower();

    let (is_rgb, has_alpha) =
        if "rgba" == lower_name { (true, true) }
        else if "rgb" == lower_name { (true, false) }
        else if "hsl" == lower_name { (false, false) }
        else if "hsla" == lower_name { (false, true) }
        else { return None };

    let mut iter = arguments.skip_whitespace();
    macro_rules! expect_comma(
        () => ( if iter.next() != Some(&Comma) { return None } );
    )
    macro_rules! expect_percentage(
        () => ( match iter.next() {
            Some(&Percentage(ref v)) => v.value,
            _ => return None,
        });
    )
    macro_rules! expect_integer(
        () => ( match iter.next() {
            Some(&Number(ref v)) if v.int_value.is_some() => v.value,
            _ => return None,
        });
    )
    macro_rules! expect_number(
        () => ( match iter.next() {
            Some(&Number(ref v)) => v.value,
            _ => return None,
        });
    )

    let red: ColorFloat;
    let green: ColorFloat;
    let blue: ColorFloat;
    if is_rgb {
        // Either integers or percentages, but all the same type.
        match iter.next() {
            Some(&Number(ref v)) if v.int_value.is_some() => {
                red = (v.value as ColorFloat / 255.) as ColorFloat;
                expect_comma!();
                green = (expect_integer!() / 255.) as ColorFloat;
                expect_comma!();
                blue = (expect_integer!() / 255.) as ColorFloat;
            }
            Some(&Percentage(ref v)) => {
                red = (v.value as ColorFloat / 100.) as ColorFloat;
                expect_comma!();
                green = (expect_percentage!() / 100.) as ColorFloat;
                expect_comma!();
                blue = (expect_percentage!() / 100.) as ColorFloat;
            }
            _ => return None
        };
    } else {
        let hue = expect_number!() / 360.;
        let hue = hue - hue.floor();
        expect_comma!();
        let saturation = (expect_percentage!() / 100.).max(&0.).min(&1.);
        expect_comma!();
        let lightness = (expect_percentage!() / 100.).max(&0.).min(&1.);

        // http://www.w3.org/TR/css3-color/#hsl-color
        fn hue_to_rgb(m1: f64, m2: f64, mut h: f64) -> f64 {
            if h < 0. { h += 1. }
            if h > 1. { h -= 1. }

            if h * 6. < 1. { m1 + (m2 - m1) * h * 6. }
            else if h * 2. < 1. { m2 }
            else if h * 3. < 2. { m1 + (m2 - m1) * (2. / 3. - h) * 6. }
            else { m1 }
        }
        let m2 = if lightness <= 0.5 { lightness * (saturation + 1.) }
                 else { lightness + saturation - lightness * saturation };
        let m1 = lightness * 2. - m2;
        red = hue_to_rgb(m1, m2, hue + 1. / 3.) as ColorFloat;
        green = hue_to_rgb(m1, m2, hue) as ColorFloat;
        blue = hue_to_rgb(m1, m2, hue - 1. / 3.) as ColorFloat;
    }

    let alpha = if has_alpha {
        expect_comma!();
        (expect_number!()).max(&0.).min(&1.) as ColorFloat
    } else {
        1.
    };
    if iter.next().is_none() {
        Some(RGBA(RGBA { red: red, green: green, blue: blue, alpha: alpha }))
    } else {
        None
    }
}
