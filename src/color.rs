/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use std::ascii::AsciiExt;
use std::fmt;
use std::num::Float;

use text_writer::{self, TextWriter};

use super::{Token, Parser, ToCss};


/// A color with red, green, blue, and alpha components.
#[derive(Clone, Copy, PartialEq)]
pub struct RGBA {
    /// The red channel. Nominally in 0.0 ... 1.0.
    pub red: f32,
    /// The green channel. Nominally in 0.0 ... 1.0.
    pub green: f32,
    /// The blue channel. Nominally in 0.0 ... 1.0.
    pub blue: f32,
    /// The alpha (opacity) channel. Clamped to 0.0 ... 1.0.
    pub alpha: f32,
}

impl ToCss for RGBA {
    fn to_css<W>(&self, dest: &mut W) -> text_writer::Result where W: TextWriter {
        if self.alpha == 1f32 {
            write!(dest, "rgb({}, {}, {})",
                   (self.red * 255.).round(),
                   (self.green * 255.).round(),
                   (self.blue * 255.).round())
        } else {
            write!(dest, "rgba({}, {}, {}, {})",
                   (self.red * 255.).round(),
                   (self.green * 255.).round(),
                   (self.blue * 255.).round(),
                   self.alpha)
        }
    }
}

/// A <color> value.
#[derive(Clone, Copy, PartialEq)]
pub enum Color {
    /// The 'currentColor' keyword
    CurrentColor,
    /// Everything else gets converted to RGBA during parsing
    RGBA(RGBA),
}

impl ToCss for Color {
    fn to_css<W>(&self, dest: &mut W) -> text_writer::Result where W: TextWriter {
        match self {
            &Color::CurrentColor => dest.write_str("currentColor"),
            &Color::RGBA(rgba) => rgba.to_css(dest),
        }
    }
}

impl fmt::Show for RGBA {
    #[inline] fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { self.fmt_to_css(f) }
}

impl fmt::Show for Color {
    #[inline] fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { self.fmt_to_css(f) }
}

impl Color {
    /// Parse a <color> value, per CSS Color Module Level 3.
    ///
    /// FIXME(#2) Deprecated CSS2 System Colors are not supported yet.
    pub fn parse(input: &mut Parser) -> Result<Color, ()> {
        match try!(input.next()) {
            Token::Hash(value) | Token::IDHash(value) => parse_color_hash(&*value),
            Token::Ident(value) => parse_color_keyword(&*value),
            Token::Function(name) => {
                input.parse_nested_block(|arguments| {
                    parse_color_function(&*name, arguments)
                })
            }
            _ => Err(())
        }
    }
}


/// Return the named color with the given name.
///
/// Matching is case-insensitive in the ASCII range.
/// CSS escaping (if relevant) should be resolved before calling this function.
/// (For example, the value of an `Ident` token is fine.)
#[inline]
pub fn parse_color_keyword(ident: &str) -> Result<Color, ()> {
    macro_rules! rgba {
        ($red: expr, $green: expr, $blue: expr) => {
            Ok(Color::RGBA(RGBA {
                red: $red / 255.,
                green: $green / 255.,
                blue: $blue / 255.,
                alpha: 1.,
            }))
        }
    }

    match_ignore_ascii_case! { ident,
        "black" => rgba!(0., 0., 0.),
        "silver" => rgba!(192., 192., 192.),
        "gray" => rgba!(128., 128., 128.),
        "white" => rgba!(255., 255., 255.),
        "maroon" => rgba!(128., 0., 0.),
        "red" => rgba!(255., 0., 0.),
        "purple" => rgba!(128., 0., 128.),
        "fuchsia" => rgba!(255., 0., 255.),
        "green" => rgba!(0., 128., 0.),
        "lime" => rgba!(0., 255., 0.),
        "olive" => rgba!(128., 128., 0.),
        "yellow" => rgba!(255., 255., 0.),
        "navy" => rgba!(0., 0., 128.),
        "blue" => rgba!(0., 0., 255.),
        "teal" => rgba!(0., 128., 128.),
        "aqua" => rgba!(0., 255., 255.),

        "aliceblue" => rgba!(240., 248., 255.),
        "antiquewhite" => rgba!(250., 235., 215.),
        "aquamarine" => rgba!(127., 255., 212.),
        "azure" => rgba!(240., 255., 255.),
        "beige" => rgba!(245., 245., 220.),
        "bisque" => rgba!(255., 228., 196.),
        "blanchedalmond" => rgba!(255., 235., 205.),
        "blueviolet" => rgba!(138., 43., 226.),
        "brown" => rgba!(165., 42., 42.),
        "burlywood" => rgba!(222., 184., 135.),
        "cadetblue" => rgba!(95., 158., 160.),
        "chartreuse" => rgba!(127., 255., 0.),
        "chocolate" => rgba!(210., 105., 30.),
        "coral" => rgba!(255., 127., 80.),
        "cornflowerblue" => rgba!(100., 149., 237.),
        "cornsilk" => rgba!(255., 248., 220.),
        "crimson" => rgba!(220., 20., 60.),
        "cyan" => rgba!(0., 255., 255.),
        "darkblue" => rgba!(0., 0., 139.),
        "darkcyan" => rgba!(0., 139., 139.),
        "darkgoldenrod" => rgba!(184., 134., 11.),
        "darkgray" => rgba!(169., 169., 169.),
        "darkgreen" => rgba!(0., 100., 0.),
        "darkgrey" => rgba!(169., 169., 169.),
        "darkkhaki" => rgba!(189., 183., 107.),
        "darkmagenta" => rgba!(139., 0., 139.),
        "darkolivegreen" => rgba!(85., 107., 47.),
        "darkorange" => rgba!(255., 140., 0.),
        "darkorchid" => rgba!(153., 50., 204.),
        "darkred" => rgba!(139., 0., 0.),
        "darksalmon" => rgba!(233., 150., 122.),
        "darkseagreen" => rgba!(143., 188., 143.),
        "darkslateblue" => rgba!(72., 61., 139.),
        "darkslategray" => rgba!(47., 79., 79.),
        "darkslategrey" => rgba!(47., 79., 79.),
        "darkturquoise" => rgba!(0., 206., 209.),
        "darkviolet" => rgba!(148., 0., 211.),
        "deeppink" => rgba!(255., 20., 147.),
        "deepskyblue" => rgba!(0., 191., 255.),
        "dimgray" => rgba!(105., 105., 105.),
        "dimgrey" => rgba!(105., 105., 105.),
        "dodgerblue" => rgba!(30., 144., 255.),
        "firebrick" => rgba!(178., 34., 34.),
        "floralwhite" => rgba!(255., 250., 240.),
        "forestgreen" => rgba!(34., 139., 34.),
        "gainsboro" => rgba!(220., 220., 220.),
        "ghostwhite" => rgba!(248., 248., 255.),
        "gold" => rgba!(255., 215., 0.),
        "goldenrod" => rgba!(218., 165., 32.),
        "greenyellow" => rgba!(173., 255., 47.),
        "grey" => rgba!(128., 128., 128.),
        "honeydew" => rgba!(240., 255., 240.),
        "hotpink" => rgba!(255., 105., 180.),
        "indianred" => rgba!(205., 92., 92.),
        "indigo" => rgba!(75., 0., 130.),
        "ivory" => rgba!(255., 255., 240.),
        "khaki" => rgba!(240., 230., 140.),
        "lavender" => rgba!(230., 230., 250.),
        "lavenderblush" => rgba!(255., 240., 245.),
        "lawngreen" => rgba!(124., 252., 0.),
        "lemonchiffon" => rgba!(255., 250., 205.),
        "lightblue" => rgba!(173., 216., 230.),
        "lightcoral" => rgba!(240., 128., 128.),
        "lightcyan" => rgba!(224., 255., 255.),
        "lightgoldenrodyellow" => rgba!(250., 250., 210.),
        "lightgray" => rgba!(211., 211., 211.),
        "lightgreen" => rgba!(144., 238., 144.),
        "lightgrey" => rgba!(211., 211., 211.),
        "lightpink" => rgba!(255., 182., 193.),
        "lightsalmon" => rgba!(255., 160., 122.),
        "lightseagreen" => rgba!(32., 178., 170.),
        "lightskyblue" => rgba!(135., 206., 250.),
        "lightslategray" => rgba!(119., 136., 153.),
        "lightslategrey" => rgba!(119., 136., 153.),
        "lightsteelblue" => rgba!(176., 196., 222.),
        "lightyellow" => rgba!(255., 255., 224.),
        "limegreen" => rgba!(50., 205., 50.),
        "linen" => rgba!(250., 240., 230.),
        "magenta" => rgba!(255., 0., 255.),
        "mediumaquamarine" => rgba!(102., 205., 170.),
        "mediumblue" => rgba!(0., 0., 205.),
        "mediumorchid" => rgba!(186., 85., 211.),
        "mediumpurple" => rgba!(147., 112., 219.),
        "mediumseagreen" => rgba!(60., 179., 113.),
        "mediumslateblue" => rgba!(123., 104., 238.),
        "mediumspringgreen" => rgba!(0., 250., 154.),
        "mediumturquoise" => rgba!(72., 209., 204.),
        "mediumvioletred" => rgba!(199., 21., 133.),
        "midnightblue" => rgba!(25., 25., 112.),
        "mintcream" => rgba!(245., 255., 250.),
        "mistyrose" => rgba!(255., 228., 225.),
        "moccasin" => rgba!(255., 228., 181.),
        "navajowhite" => rgba!(255., 222., 173.),
        "oldlace" => rgba!(253., 245., 230.),
        "olivedrab" => rgba!(107., 142., 35.),
        "orange" => rgba!(255., 165., 0.),
        "orangered" => rgba!(255., 69., 0.),
        "orchid" => rgba!(218., 112., 214.),
        "palegoldenrod" => rgba!(238., 232., 170.),
        "palegreen" => rgba!(152., 251., 152.),
        "paleturquoise" => rgba!(175., 238., 238.),
        "palevioletred" => rgba!(219., 112., 147.),
        "papayawhip" => rgba!(255., 239., 213.),
        "peachpuff" => rgba!(255., 218., 185.),
        "peru" => rgba!(205., 133., 63.),
        "pink" => rgba!(255., 192., 203.),
        "plum" => rgba!(221., 160., 221.),
        "powderblue" => rgba!(176., 224., 230.),
        "rebeccapurple" => rgba!(102., 51., 153.),
        "rosybrown" => rgba!(188., 143., 143.),
        "royalblue" => rgba!(65., 105., 225.),
        "saddlebrown" => rgba!(139., 69., 19.),
        "salmon" => rgba!(250., 128., 114.),
        "sandybrown" => rgba!(244., 164., 96.),
        "seagreen" => rgba!(46., 139., 87.),
        "seashell" => rgba!(255., 245., 238.),
        "sienna" => rgba!(160., 82., 45.),
        "skyblue" => rgba!(135., 206., 235.),
        "slateblue" => rgba!(106., 90., 205.),
        "slategray" => rgba!(112., 128., 144.),
        "slategrey" => rgba!(112., 128., 144.),
        "snow" => rgba!(255., 250., 250.),
        "springgreen" => rgba!(0., 255., 127.),
        "steelblue" => rgba!(70., 130., 180.),
        "tan" => rgba!(210., 180., 140.),
        "thistle" => rgba!(216., 191., 216.),
        "tomato" => rgba!(255., 99., 71.),
        "turquoise" => rgba!(64., 224., 208.),
        "violet" => rgba!(238., 130., 238.),
        "wheat" => rgba!(245., 222., 179.),
        "whitesmoke" => rgba!(245., 245., 245.),
        "yellowgreen" => rgba!(154., 205., 50.),

        "transparent" => Ok(Color::RGBA(RGBA { red: 0., green: 0., blue: 0., alpha: 0. })),
        "currentcolor" => Ok(Color::CurrentColor)
        _ => Err(())
    }
}


#[inline]
fn parse_color_hash(value: &str) -> Result<Color, ()> {
    macro_rules! from_hex(
        ($c: expr) => {{
            let c = $c;
            match c {
                '0' ... '9' => c as u8 - ('0' as u8),
                'a' ... 'f' => c as u8 - ('a' as u8) + 10,
                'A' ... 'F' => c as u8 - ('A' as u8) + 10,
                _ => return Err(())  // Not a valid color
            }
        }};
    );
    macro_rules! to_rgba(
        ($r: expr, $g: expr, $b: expr,) => {
            Ok(Color::RGBA(RGBA { red: $r as f32 / 255.,
                                  green: $g as f32 / 255.,
                                  blue: $b as f32 / 255.,
                                  alpha: 1. }))
        };
    );

    match value.len() {
        6 => to_rgba!(
            from_hex!(value.char_at(0)) * 16 + from_hex!(value.char_at(1)),
            from_hex!(value.char_at(2)) * 16 + from_hex!(value.char_at(3)),
            from_hex!(value.char_at(4)) * 16 + from_hex!(value.char_at(5)),
        ),
        3 => to_rgba!(
            from_hex!(value.char_at(0)) * 17,
            from_hex!(value.char_at(1)) * 17,
            from_hex!(value.char_at(2)) * 17,
        ),
        _ => Err(())
    }
}


#[inline]
fn parse_color_function(name: &str, arguments: &mut Parser) -> Result<Color, ()> {
    let (is_rgb, has_alpha) = match_ignore_ascii_case! { name,
        "rgba" => (true, true),
        "rgb" => (true, false),
        "hsl" => (false, false),
        "hsla" => (false, true)
        _ => return Err(())
    };

    let red: f32;
    let green: f32;
    let blue: f32;
    if is_rgb {
        // Either integers or percentages, but all the same type.
        match try!(arguments.next()) {
            Token::Number(ref v) if v.int_value.is_some() => {
                red = (v.value / 255.) as f32;
                try!(arguments.expect_comma());
                green = try!(arguments.expect_integer()) as f32 / 255.;
                try!(arguments.expect_comma());
                blue = try!(arguments.expect_integer()) as f32 / 255.;
            }
            Token::Percentage(ref v) => {
                red = v.unit_value as f32;
                try!(arguments.expect_comma());
                green = try!(arguments.expect_percentage()) as f32;
                try!(arguments.expect_comma());
                blue = try!(arguments.expect_percentage()) as f32;
            }
            _ => return Err(())
        };
    } else {
        let hue = try!(arguments.expect_number()) / 360.;
        let hue = hue - hue.floor();
        try!(arguments.expect_comma());
        let saturation = (try!(arguments.expect_percentage())).max(0.).min(1.);
        try!(arguments.expect_comma());
        let lightness = (try!(arguments.expect_percentage())).max(0.).min(1.);

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
        red = hue_to_rgb(m1, m2, hue + 1. / 3.) as f32;
        green = hue_to_rgb(m1, m2, hue) as f32;
        blue = hue_to_rgb(m1, m2, hue - 1. / 3.) as f32;
    }

    let alpha = if has_alpha {
        try!(arguments.expect_comma());
        (try!(arguments.expect_number())).max(0.).min(1.) as f32
    } else {
        1.
    };
    try!(arguments.expect_exhausted());
    Ok(Color::RGBA(RGBA { red: red, green: green, blue: blue, alpha: alpha }))
}
