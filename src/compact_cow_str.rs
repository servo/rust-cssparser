/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use std::borrow::{Borrow, Cow};
use std::cmp;
use std::fmt;
use std::hash;
use std::marker::PhantomData;
use std::mem;
use std::ops::Deref;
use std::rc::Rc;
use std::slice;
use std::str;
use std::usize;

/// A string that is either shared (heap-allocated and reference-counted) or borrowed.
///
/// Equivalent to `enum { Borrowed(&'a str), Shared(Rc<String>) }`, but stored more compactly.
///
/// FIXME(https://github.com/rust-lang/rfcs/issues/1230): use an actual enum if/when
/// the compiler can do this layout optimization.
pub struct CowRcStr<'a> {
    /// FIXME: https://github.com/rust-lang/rust/issues/27730 use NonZero or Shared
    /// In the meantime we abuse `&'static _` to get the effect of `NonZero<*const _>`.
    /// `ptr` doesn’t really have the 'static lifetime!
    ptr: &'static (),

    /// * If `borrowed_len_or_max == usize::MAX`, then `ptr` represents `NonZero<*const String>`
    ///   from `Rc::into_raw`.
    ///   The lifetime parameter `'a` is irrelevant in this case.
    ///
    /// * Otherwise, `ptr` represents the `NonZero<*const u8>` data component of `&'a str`,
    ///   and `borrowed_len_or_max` its length.
    borrowed_len_or_max: usize,

    phantom: PhantomData<Result<&'a str, Rc<String>>>,
}

fn _static_assert_same_size<'a>() {
    // "Instantiate" the generic function without calling it.
    let _ = mem::transmute::<CowRcStr<'a>, Option<CowRcStr<'a>>>;
}

impl<'a> From<&'a str> for CowRcStr<'a> {
    #[inline]
    fn from(s: &'a str) -> Self {
        let len = s.len();
        assert!(len < usize::MAX);
        CowRcStr {
            ptr: unsafe { &*(s.as_ptr() as *const ()) },
            borrowed_len_or_max: len,
            phantom: PhantomData,
        }
    }
}

impl<'a> From<Rc<String>> for CowRcStr<'a> {
    #[inline]
    fn from(s: Rc<String>) -> Self {
        let ptr = unsafe { &*(Rc::into_raw(s) as *const ()) };
        CowRcStr {
            ptr: ptr,
            borrowed_len_or_max: usize::MAX,
            phantom: PhantomData,
        }
    }
}

impl<'a> CowRcStr<'a> {
    #[inline]
    fn unpack(&self) -> Result<&'a str, *const String> {
        if self.borrowed_len_or_max == usize::MAX {
            Err(self.ptr as *const () as *const String)
        } else {
            unsafe {
                Ok(str::from_utf8_unchecked(slice::from_raw_parts(
                    self.ptr as *const () as *const u8,
                    self.borrowed_len_or_max,
                )))
            }
        }
    }

    #[inline]
    fn into_enum(self) -> Result<&'a str, Rc<String>> {
        self.unpack().map_err(|ptr| {
            mem::forget(self);
            unsafe {
                Rc::from_raw(ptr)
            }
        })
    }

    /// Convert into `String`, re-using an existing memory allocation if possible.
    #[inline]
    pub fn into_owned(self) -> String {
        match self.into_enum() {
            Ok(s) => s.to_owned(),
            Err(rc) => match Rc::try_unwrap(rc) {
                Ok(s) => s,
                Err(rc) => (*rc).clone()
            }
        }
    }
}

impl<'a> Clone for CowRcStr<'a> {
    #[inline]
    fn clone(&self) -> Self {
        match self.unpack() {
            Err(ptr) => {
                let rc = unsafe {
                    Rc::from_raw(ptr)
                };
                let new_rc = rc.clone();
                mem::forget(rc);  // Don’t actually take ownership of this strong reference
                new_rc.into()
            }
            Ok(_) => {
                CowRcStr { ..*self }
            }
        }
    }
}

impl<'a> Drop for CowRcStr<'a> {
    #[inline]
    fn drop(&mut self) {
        if let Err(ptr) = self.unpack() {
            mem::drop(unsafe {
                Rc::from_raw(ptr)
            })
        }
    }
}

impl<'a> Deref for CowRcStr<'a> {
    type Target = str;

    #[inline]
    fn deref(&self) -> &str {
        self.unpack().unwrap_or_else(|ptr| unsafe {
            &**ptr
        })
    }
}

impl<'a> From<CowRcStr<'a>> for Cow<'a, str> {
    #[inline]
    fn from(cow: CowRcStr<'a>) -> Self {
        match cow.into_enum() {
            Ok(s) => Cow::Borrowed(s),
            Err(rc) => match Rc::try_unwrap(rc) {
                Ok(s) => Cow::Owned(s),
                Err(rc) => Cow::Owned((*rc).clone())
            }
        }
    }
}

impl<'a> From<String> for CowRcStr<'a> {
    #[inline]
    fn from(s: String) -> Self {
        Self::from(Rc::new(s))
    }
}

impl<'a> From<Cow<'a, str>> for CowRcStr<'a> {
    #[inline]
    fn from(s: Cow<'a, str>) -> Self {
        match s {
            Cow::Borrowed(s) => Self::from(s),
            Cow::Owned(s) => Self::from(s),
        }
    }
}


// Boilerplate / trivial impls below.

impl<'a> AsRef<str> for CowRcStr<'a> {
    #[inline]
    fn as_ref(&self) -> &str {
        self
    }
}

impl<'a> Borrow<str> for CowRcStr<'a> {
    #[inline]
    fn borrow(&self) -> &str {
        self
    }
}

impl<'a> Default for CowRcStr<'a> {
    #[inline]
    fn default() -> Self {
        Self::from("")
    }
}

impl<'a> hash::Hash for CowRcStr<'a> {
    #[inline]
    fn hash<H: hash::Hasher>(&self, hasher: &mut H) {
        str::hash(self, hasher)
    }
}

impl<'a, T: AsRef<str>> PartialEq<T> for CowRcStr<'a> {
    #[inline]
    fn eq(&self, other: &T) -> bool {
        str::eq(self, other.as_ref())
    }
}

impl<'a, T: AsRef<str>> PartialOrd<T> for CowRcStr<'a> {
    #[inline]
    fn partial_cmp(&self, other: &T) -> Option<cmp::Ordering> {
        str::partial_cmp(self, other.as_ref())
    }
}

impl<'a> Eq for CowRcStr<'a> {}

impl<'a> Ord for CowRcStr<'a> {
    #[inline]
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        str::cmp(self, other)
    }
}

impl<'a> fmt::Display for CowRcStr<'a> {
    #[inline]
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        str::fmt(self, formatter)
    }
}

impl<'a> fmt::Debug for CowRcStr<'a> {
    #[inline]
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        str::fmt(self, formatter)
    }
}
