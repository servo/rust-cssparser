use ast::*;
use super::to_ascii_lower;
use self::color_data::{COLOR_KEYWORDS, COLOR_VALUES};

mod color_data;

pub type ColorFloat = f64;


pub enum Color {
    CurrentColor,
    RGBA(ColorFloat, ColorFloat, ColorFloat, ColorFloat),  // 0..1
}


// Return None on invalid/unsupported value (not a color)
pub fn parse_color(component_value: &ComponentValue) -> Option<Color> {
    match *component_value {
        Hash(ref value) | IDHash(ref value) => parse_hash_color(*value),
        Ident(ref value) => parse_keyword_color(*value),
        Function(ref name, ref arguments) => parse_function_color(*name, *arguments),
        _ => None
    }
}


#[inline]
fn parse_keyword_color(value: &str) -> Option<Color> {
    let lower_value = to_ascii_lower(value);
    match COLOR_KEYWORDS.bsearch_elem(&lower_value.as_slice()) {
        Some(index) => Some(COLOR_VALUES[index]),
        None => if "currentcolor" == lower_value { Some(CurrentColor) }
                else { None }
    }
}


#[inline]
fn parse_hash_color(value: &str) -> Option<Color> {
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
            Some(RGBA($r as ColorFloat / 255., $g as ColorFloat / 255.,
                      $b as ColorFloat / 255., 1.))
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
fn parse_function_color(name: &str, arguments: &[(ComponentValue, SourceLocation)])
                        -> Option<Color> {
    let _lower_name = to_ascii_lower(name);
    let _iter = arguments.iter();
    None  // TODO
}
