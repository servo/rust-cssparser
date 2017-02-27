/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */


/// Expands to an expression equivalent to a `match` with string patterns,
/// but matching is case-insensitive in the ASCII range.
///
/// Requirements:
///
/// * The `cssparser_macros` crate must also be imported at the crate root
/// * The patterns must not contain ASCII upper case letters. (They must be already be lower-cased.)
///
/// # Example
///
/// ```rust
/// #[macro_use] extern crate cssparser;
/// #[macro_use] extern crate cssparser_macros;
///
/// # fn main() {}  // Make doctest not wrap everythig in its own main
/// # fn dummy(function_name: &String) { let _ =
/// match_ignore_ascii_case! { &function_name,
///     "rgb" => parse_rgb(..),
///     "rgba" => parse_rgba(..),
///     "hsl" => parse_hsl(..),
///     "hsla" => parse_hsla(..),
///     _ => Err("unknown function")
/// }
/// # ;}
/// # use std::ops::RangeFull;
/// # fn parse_rgb(_: RangeFull) -> Result<(), &'static str> { Err("") }
/// # fn parse_rgba(_: RangeFull) -> Result<(), &'static str> { Err("") }
/// # fn parse_hsl(_: RangeFull) -> Result<(), &'static str> { Err("") }
/// # fn parse_hsla(_: RangeFull) -> Result<(), &'static str> { Err("") }
/// ```
#[macro_export]
macro_rules! match_ignore_ascii_case {
    // parse the last case plus the fallback
    (@inner $value:expr, ($string:expr => $result:expr, _ => $fallback:expr) -> ($($parsed:tt)*) ) => {
        match_ignore_ascii_case!(@inner $value, () -> ($($parsed)* ($string => $result)) $fallback)
    };

    // parse a case (not the last one)
    (@inner $value:expr, ($string:expr => $result:expr, $($rest:tt)*) -> ($($parsed:tt)*) ) => {
        match_ignore_ascii_case!(@inner $value, ($($rest)*) -> ($($parsed)* ($string => $result)))
    };

    // finished parsing
    (@inner $value:expr, () -> ($(($string:expr => $result:expr))*) $fallback:expr ) => {
        {
            _cssparser_internal__pretend_proc_macro! {
                cssparser__assert_ascii_lowercase!( $( $string )+ )
            }

            {
                _cssparser_internal__to_lowercase!($value => lowercase, $($string),+);
                match lowercase {
                    $(
                        Some($string) => $result,
                    )+
                    _ => $fallback
                }
            }
        }
    };

    // entry point, start parsing
    ( $value:expr, $($rest:tt)* ) => {
        match_ignore_ascii_case!(@inner $value, ($($rest)*) -> ())
    };
}

/// Define a function `$name(&str) -> Option<&'static $ValueType>`
///
/// The function finds a match for the input string
/// in a [`phf` map](https://github.com/sfackler/rust-phf)
/// and returns a reference to the corresponding value.
/// Matching is case-insensitive in the ASCII range.
///
/// The `phf` and `cssparser_macros` crates must also be imported at the crate root
///
/// ## Example:
///
/// ```rust
/// extern crate phf;
/// #[macro_use] extern crate cssparser;
/// #[macro_use] extern crate cssparser_macros;
///
/// # fn main() {}  // Make doctest not wrap everythig in its own main
///
/// fn color_rgb(input: &str) -> Option<(u8, u8, u8)> {
///     ascii_case_insensitive_phf_map! {
///         keyword -> (u8, u8, u8) = {
///             "red" => (255, 0, 0),
///             "green" => (0, 255, 0),
///             "blue" => (0, 0, 255),
///         }
///     }
///     keyword(input).cloned()
/// }
#[macro_export]
macro_rules! ascii_case_insensitive_phf_map {
    ($name: ident -> $ValueType: ty = { $( $key: expr => $value: expr, )* }) => {
        fn $name(input: &str) -> Option<&'static $ValueType> {
            _cssparser_internal__pretend_proc_macro! {
                cssparser__phf_map!( ($ValueType) $( $key ($value) )+ )
            }

            {
                _cssparser_internal__to_lowercase!(input => lowercase, $($key),+);
                lowercase.and_then(|s| MAP.get(s))
            }
        }
    }
}

/// Implementation detail of match_ignore_ascii_case! and ascii_case_insensitive_phf_map! macros.
///
/// **This macro is not part of the public API. It can change or be removed between any versions.**
///
/// * Check at compile-time that none of the `$string`s contain ASCII uppercase letters
/// * Define a local variable named `$output` to the result of calling `_internal__to_lowercase`
///   with a stack-allocated buffer as long as the longest `$string`.
#[macro_export]
#[doc(hidden)]
macro_rules! _cssparser_internal__to_lowercase {
    ($input: expr => $output: ident, $($string: expr),+) => {
        _cssparser_internal__pretend_proc_macro! {
            cssparser__max_len!( $( $string )+ )
        }

        // mem::uninitialized() is ok because `buffer` is only used in `_internal__to_lowercase`,
        // which initializes with `copy_from_slice` the part of the buffer it uses,
        // before it uses it.
        #[allow(unsafe_code)]
        // MAX_LENGTH is generated by cssparser__max_len
        let mut buffer: [u8; MAX_LENGTH] = unsafe {
            ::std::mem::uninitialized()
        };
        let input: &str = $input;
        let $output = $crate::_internal__to_lowercase(&mut buffer, input);
    }
}

/// Implementation detail of match_ignore_ascii_case! and ascii_case_insensitive_phf_map! macros.
///
/// **This macro is not part of the public API. It can change or be removed between any versions.**
#[macro_export]
#[doc(hidden)]
macro_rules! _cssparser_internal__pretend_proc_macro {
    ($macro_name: ident ! ( $($tt: tt)* )) => {
        #[derive($macro_name)]
        #[allow(unused)]
        enum Dummy {
            Input = (0, stringify!( $( $tt )* )).0
        }
    }
}

/// Implementation detail of match_ignore_ascii_case! and ascii_case_insensitive_phf_map! macros.
///
/// **This function is not part of the public API. It can change or be removed between any verisons.**
///
/// Return `input`, lower-cased, unless larger than `buffer`
/// which is used temporary space for lower-casing a copy of `input` if necessary.
#[doc(hidden)]
#[allow(non_snake_case)]
pub fn _internal__to_lowercase<'a>(buffer: &'a mut [u8], input: &'a str) -> Option<&'a str> {
    if let Some(buffer) = buffer.get_mut(..input.len()) {
        if let Some(first_uppercase) = input.bytes().position(|byte| matches!(byte, b'A'...b'Z')) {
            buffer.copy_from_slice(input.as_bytes());
            ::std::ascii::AsciiExt::make_ascii_lowercase(&mut buffer[first_uppercase..]);
            // `buffer` was initialized to a copy of `input` (which is &str so well-formed UTF-8)
            // then lowercased (which preserves UTF-8 well-formedness)
            unsafe {
                Some(::std::str::from_utf8_unchecked(buffer))
            }
        } else {
            // Input is already lower-case
            Some(input)
        }
    } else {
        // Input is longer than buffer, which has the length of the longest expected string:
        // none of the expected strings would match.
        None
    }
}

#[cfg(feature = "dummy_match_byte")]
macro_rules! match_byte {
    ($value:expr, $($rest:tt)* ) => {
        match $value {
            $(
                $rest
            )+
        }
    };
}
