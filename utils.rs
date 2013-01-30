pub pure fn ascii_lower(string: &str) -> ~str {
    // Warning: premature optimization ahead ;)
    // TODO: would it be more efficient to work on bytes,
    // without decoding/re-encoding UTF-8?
    do str::map(string) |c| {
        match c {
            'A'..'Z' => c + ASCII_LOWER_OFFSET,
            _ => c,
        }
    }
}

#[cfg(test)]
pub fn check_results<T: cmp::Eq, E: cmp::Eq>(
        results: &[T], expected: &[T],
        errors: &[E], expected_errors: &[E]) {
    assert_vec_equals("Results", results, expected);
    assert_vec_equals("Errors", errors, expected_errors);

    fn assert_vec_equals<T: cmp::Eq>(
            message: &str, results: &[T], expected: &[T]) {
        let len_r = results.len();
        let len_e = expected.len();
        if len_r != len_e {
            fail fmt!(
                "\n%s: expected %u items, got %u. First extra item:\n%?\n",
                message, len_e, len_r,
                if len_r > len_e { results[len_e] }
                else { expected[len_r] })
        }
        let mut i = 0u;
        for vec::each2(results, expected) |a, b| {
            if a != b {
                fail fmt!("\n%s at index %u:\n%?\n!=\n%?\n", message, i, a, b)
            }
            i += 1
        }
    }
}


//  ***********  End of public API  ***********


const ASCII_LOWER_OFFSET: char = 'a' - 'A';

#[test]
fn test_ascii_lower() {
    assert ascii_lower("url()URL()uRl()Ürl") == ~"url()url()url()Ürl";
    // Dotted capital I, Kelvin sign, Sharp S.
    assert ascii_lower("HİKß") == ~"hİKß";
}
