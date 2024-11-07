/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#[inline]
pub(crate) fn f32_trunc(val: f32) -> f32 {
    #[cfg(feature = "std")]
    { val.round() }
    #[cfg(not(feature = "std"))]
    { libm::roundf(val) }
}

#[inline]
pub(crate) fn f32_round(val: f32) -> f32 {
    #[cfg(feature = "std")]
    { val.round() }
    #[cfg(not(feature = "std"))]
    { libm::roundf(val) }
}

#[inline]
pub(crate) fn f64_pow(a: f64, b: f64) -> f64 {
    #[cfg(feature = "std")]
    { f64::powf(a, b) }
    #[cfg(not(feature = "std"))]
    { libm::pow(a, b) }
}
