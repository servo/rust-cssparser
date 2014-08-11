/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use std::ascii::StrAsciiExt;
use std::fmt;

use ast::*;


#[deriving(Clone, PartialEq)]
pub struct RGBA {
    // All in 0..1
    // Use f32 to try and match rust-azure’s AzFloat
    pub red: f32,
    pub green: f32,
    pub blue: f32,
    pub alpha: f32,
}

impl fmt::Show for RGBA {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(r={} g={} b={} a={})", self.red, self.green, self.blue, self.alpha)
    }
}

#[deriving(Clone, PartialEq)]
pub enum Color {
    CurrentColor,
    RGBA(RGBA),
}


// Return None on invalid/unsupported value (not a color)
impl Color {
    pub fn parse(component_value: &ComponentValue) -> Result<Color, ()> {
        match *component_value {
            Hash(ref value) | IDHash(ref value) => parse_color_hash(value.as_slice()),
            Ident(ref value) => parse_color_keyword(value.as_slice()),
            Function(ref name, ref arguments)
                => parse_color_function(name.as_slice(), arguments.as_slice()),
            _ => Err(())
        }
    }
}


#[inline]
fn parse_color_keyword(value: &str) -> Result<Color, ()> {
    let lower_value = value.to_ascii_lower();
    let (r, g, b) = match lower_value.as_slice() {
        "black" => (0., 0., 0.),
        "silver" => (192., 192., 192.),
        "gray" => (128., 128., 128.),
        "white" => (255., 255., 255.),
        "maroon" => (128., 0., 0.),
        "red" => (255., 0., 0.),
        "purple" => (128., 0., 128.),
        "fuchsia" => (255., 0., 255.),
        "green" => (0., 128., 0.),
        "lime" => (0., 255., 0.),
        "olive" => (128., 128., 0.),
        "yellow" => (255., 255., 0.),
        "navy" => (0., 0., 128.),
        "blue" => (0., 0., 255.),
        "teal" => (0., 128., 128.),
        "aqua" => (0., 255., 255.),

        "aliceblue" => (240., 248., 255.),
        "antiquewhite" => (250., 235., 215.),
        "aquamarine" => (127., 255., 212.),
        "azure" => (240., 255., 255.),
        "beige" => (245., 245., 220.),
        "bisque" => (255., 228., 196.),
        "blanchedalmond" => (255., 235., 205.),
        "blueviolet" => (138., 43., 226.),
        "brown" => (165., 42., 42.),
        "burlywood" => (222., 184., 135.),
        "cadetblue" => (95., 158., 160.),
        "chartreuse" => (127., 255., 0.),
        "chocolate" => (210., 105., 30.),
        "coral" => (255., 127., 80.),
        "cornflowerblue" => (100., 149., 237.),
        "cornsilk" => (255., 248., 220.),
        "crimson" => (220., 20., 60.),
        "cyan" => (0., 255., 255.),
        "darkblue" => (0., 0., 139.),
        "darkcyan" => (0., 139., 139.),
        "darkgoldenrod" => (184., 134., 11.),
        "darkgray" => (169., 169., 169.),
        "darkgreen" => (0., 100., 0.),
        "darkgrey" => (169., 169., 169.),
        "darkkhaki" => (189., 183., 107.),
        "darkmagenta" => (139., 0., 139.),
        "darkolivegreen" => (85., 107., 47.),
        "darkorange" => (255., 140., 0.),
        "darkorchid" => (153., 50., 204.),
        "darkred" => (139., 0., 0.),
        "darksalmon" => (233., 150., 122.),
        "darkseagreen" => (143., 188., 143.),
        "darkslateblue" => (72., 61., 139.),
        "darkslategray" => (47., 79., 79.),
        "darkslategrey" => (47., 79., 79.),
        "darkturquoise" => (0., 206., 209.),
        "darkviolet" => (148., 0., 211.),
        "deeppink" => (255., 20., 147.),
        "deepskyblue" => (0., 191., 255.),
        "dimgray" => (105., 105., 105.),
        "dimgrey" => (105., 105., 105.),
        "dodgerblue" => (30., 144., 255.),
        "firebrick" => (178., 34., 34.),
        "floralwhite" => (255., 250., 240.),
        "forestgreen" => (34., 139., 34.),
        "gainsboro" => (220., 220., 220.),
        "ghostwhite" => (248., 248., 255.),
        "gold" => (255., 215., 0.),
        "goldenrod" => (218., 165., 32.),
        "greenyellow" => (173., 255., 47.),
        "grey" => (128., 128., 128.),
        "honeydew" => (240., 255., 240.),
        "hotpink" => (255., 105., 180.),
        "indianred" => (205., 92., 92.),
        "indigo" => (75., 0., 130.),
        "ivory" => (255., 255., 240.),
        "khaki" => (240., 230., 140.),
        "lavender" => (230., 230., 250.),
        "lavenderblush" => (255., 240., 245.),
        "lawngreen" => (124., 252., 0.),
        "lemonchiffon" => (255., 250., 205.),
        "lightblue" => (173., 216., 230.),
        "lightcoral" => (240., 128., 128.),
        "lightcyan" => (224., 255., 255.),
        "lightgoldenrodyellow" => (250., 250., 210.),
        "lightgray" => (211., 211., 211.),
        "lightgreen" => (144., 238., 144.),
        "lightgrey" => (211., 211., 211.),
        "lightpink" => (255., 182., 193.),
        "lightsalmon" => (255., 160., 122.),
        "lightseagreen" => (32., 178., 170.),
        "lightskyblue" => (135., 206., 250.),
        "lightslategray" => (119., 136., 153.),
        "lightslategrey" => (119., 136., 153.),
        "lightsteelblue" => (176., 196., 222.),
        "lightyellow" => (255., 255., 224.),
        "limegreen" => (50., 205., 50.),
        "linen" => (250., 240., 230.),
        "magenta" => (255., 0., 255.),
        "mediumaquamarine" => (102., 205., 170.),
        "mediumblue" => (0., 0., 205.),
        "mediumorchid" => (186., 85., 211.),
        "mediumpurple" => (147., 112., 219.),
        "mediumseagreen" => (60., 179., 113.),
        "mediumslateblue" => (123., 104., 238.),
        "mediumspringgreen" => (0., 250., 154.),
        "mediumturquoise" => (72., 209., 204.),
        "mediumvioletred" => (199., 21., 133.),
        "midnightblue" => (25., 25., 112.),
        "mintcream" => (245., 255., 250.),
        "mistyrose" => (255., 228., 225.),
        "moccasin" => (255., 228., 181.),
        "navajowhite" => (255., 222., 173.),
        "oldlace" => (253., 245., 230.),
        "olivedrab" => (107., 142., 35.),
        "orange" => (255., 165., 0.),
        "orangered" => (255., 69., 0.),
        "orchid" => (218., 112., 214.),
        "palegoldenrod" => (238., 232., 170.),
        "palegreen" => (152., 251., 152.),
        "paleturquoise" => (175., 238., 238.),
        "palevioletred" => (219., 112., 147.),
        "papayawhip" => (255., 239., 213.),
        "peachpuff" => (255., 218., 185.),
        "peru" => (205., 133., 63.),
        "pink" => (255., 192., 203.),
        "plum" => (221., 160., 221.),
        "powderblue" => (176., 224., 230.),
        "rebeccapurple" => (102., 51., 153.),
        "rosybrown" => (188., 143., 143.),
        "royalblue" => (65., 105., 225.),
        "saddlebrown" => (139., 69., 19.),
        "salmon" => (250., 128., 114.),
        "sandybrown" => (244., 164., 96.),
        "seagreen" => (46., 139., 87.),
        "seashell" => (255., 245., 238.),
        "sienna" => (160., 82., 45.),
        "skyblue" => (135., 206., 235.),
        "slateblue" => (106., 90., 205.),
        "slategray" => (112., 128., 144.),
        "slategrey" => (112., 128., 144.),
        "snow" => (255., 250., 250.),
        "springgreen" => (0., 255., 127.),
        "steelblue" => (70., 130., 180.),
        "tan" => (210., 180., 140.),
        "thistle" => (216., 191., 216.),
        "tomato" => (255., 99., 71.),
        "turquoise" => (64., 224., 208.),
        "violet" => (238., 130., 238.),
        "wheat" => (245., 222., 179.),
        "whitesmoke" => (245., 245., 245.),
        "yellowgreen" => (154., 205., 50.),

        "transparent" => return Ok(RGBA(RGBA { red: 0., green: 0., blue: 0., alpha: 0. })),
        "currentcolor" => return Ok(CurrentColor),
        _ => return Err(()),
    };
    Ok(RGBA(RGBA { red: r / 255., green: g / 255., blue: b / 255., alpha: 1. }))
}


#[inline]
fn parse_color_hash(value: &str) -> Result<Color, ()> {
    macro_rules! from_hex(
        ($c: expr) => {{
            let c = $c;
            match c {
                '0' .. '9' => c as u8 - ('0' as u8),
                'a' .. 'f' => c as u8 - ('a' as u8) + 10,
                'A' .. 'F' => c as u8 - ('A' as u8) + 10,
                _ => return Err(())  // Not a valid color
            }
        }};
    )
    macro_rules! to_rgba(
        ($r: expr, $g: expr, $b: expr,) => {
            Ok(RGBA(RGBA { red: $r as f32 / 255.,
                           green: $g as f32 / 255.,
                           blue: $b as f32 / 255.,
                           alpha: 1. }))
        };
    )

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
fn parse_color_function(name: &str, arguments: &[ComponentValue])
                        -> Result<Color, ()> {
    let lower_name = name.to_ascii_lower();
    let lower_name = lower_name.as_slice();

    let (is_rgb, has_alpha) =
        if "rgba" == lower_name { (true, true) }
        else if "rgb" == lower_name { (true, false) }
        else if "hsl" == lower_name { (false, false) }
        else if "hsla" == lower_name { (false, true) }
        else { return Err(()) };

    let mut iter = arguments.skip_whitespace();
    macro_rules! expect_comma(
        () => ( match iter.next() { Some(&Comma) => {}, _ => { return Err(()) } } );
    )
    macro_rules! expect_percentage(
        () => ( match iter.next() {
            Some(&Percentage(ref v)) => v.value,
            _ => return Err(()),
        });
    )
    macro_rules! expect_integer(
        () => ( match iter.next() {
            Some(&Number(ref v)) if v.int_value.is_some() => v.value,
            _ => return Err(()),
        });
    )
    macro_rules! expect_number(
        () => ( match iter.next() {
            Some(&Number(ref v)) => v.value,
            _ => return Err(()),
        });
    )

    let red: f32;
    let green: f32;
    let blue: f32;
    if is_rgb {
        // Either integers or percentages, but all the same type.
        match iter.next() {
            Some(&Number(ref v)) if v.int_value.is_some() => {
                red = (v.value / 255.) as f32;
                expect_comma!();
                green = (expect_integer!() / 255.) as f32;
                expect_comma!();
                blue = (expect_integer!() / 255.) as f32;
            }
            Some(&Percentage(ref v)) => {
                red = (v.value / 100.) as f32;
                expect_comma!();
                green = (expect_percentage!() / 100.) as f32;
                expect_comma!();
                blue = (expect_percentage!() / 100.) as f32;
            }
            _ => return Err(())
        };
    } else {
        let hue = expect_number!() / 360.;
        let hue = hue - hue.floor();
        expect_comma!();
        let saturation = (expect_percentage!() / 100.).max(0.).min(1.);
        expect_comma!();
        let lightness = (expect_percentage!() / 100.).max(0.).min(1.);

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
        expect_comma!();
        (expect_number!()).max(0.).min(1.) as f32
    } else {
        1.
    };
    if iter.next().is_none() {
        Ok(RGBA(RGBA { red: red, green: green, blue: blue, alpha: alpha }))
    } else {
        Err(())
    }
}
