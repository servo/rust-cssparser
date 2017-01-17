
#[macro_use] extern crate quote;
extern crate syn;

use std::env;
use std::path::Path;

#[path = "src/macros/mod.rs"]
mod macros;

fn main() {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap();

    let tokenizer_rs = Path::new(&manifest_dir).join("src/tokenizer.rs");
    macros::match_byte::expand(&tokenizer_rs,
                               &Path::new(&env::var("OUT_DIR").unwrap()).join("tokenizer.rs"));

     println!("cargo:rerun-if-changed={}", tokenizer_rs.display());
}
