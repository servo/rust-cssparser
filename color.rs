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
pub fn parse_color(component_value: ComponentValue) -> Option<Color> {
    match component_value {
        Ident(value) => {
            let lower_value = to_ascii_lower(value);
            match COLOR_KEYWORDS.bsearch_elem(&lower_value.as_slice()) {
                Some(index) => Some(COLOR_VALUES[index]),
                None => {
                    if "currentcolor" == lower_value { Some(CurrentColor) }
                    else { None }
                }
            }
        }
        _ => None
    }
}
