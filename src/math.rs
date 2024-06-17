
pub(crate) fn f32_trunc(val: f32) -> f32 {
    #[cfg(feature = "std")]
    { val.round() }
    #[cfg(not(feature = "std"))]
    { libm::roundf(val) }
}

pub(crate) fn f32_round(val: f32) -> f32 {
    #[cfg(feature = "std")]
    { val.round() }
    #[cfg(not(feature = "std"))]
    { libm::roundf(val) }
}

pub(crate) fn f64_pow(a: f64, b: f64) -> f64 {
    #[cfg(feature = "std")]
    { f64::powf(a, b) }
    #[cfg(not(feature = "std"))]
    { libm::pow(a, b) }
}
