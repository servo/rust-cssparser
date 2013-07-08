// When reporting these errors to the user, the application is expected
// to show the source filename/URL/location in addition to line and column.
pub struct ParseError {
    message: ~str,
    // TODO: add these:
//    source_line: uint,
//    source_column: uint,
}


pub fn ascii_lower(string: &str) -> ~str {
    // Warning: premature optimization ahead ;)
    // TODO: would it be more efficient to work on bytes,
    // without decoding/re-encoding UTF-8?
    do string.map_chars |c| {
        match c {
            'A'..'Z' => c + ASCII_LOWER_OFFSET,
            _ => c,
        }
    }
}

#[cfg(test)]
pub fn check_results<T: Eq, E: Eq>(
        input: &str, results: &[T], expected: &[T],
        errors: &[E], expected_errors: &[E]) {
    assert_vec_equals("Results", results, expected, input);
    assert_vec_equals("Errors", errors, expected_errors, input);

    fn assert_vec_equals<T: Eq>(
            message: &str, results: &[T], expected: &[T], input: &str) {
        for results.iter().zip(expected.iter()).enumerate().advance |(i, (a, b))| {
            if a != b {
                fail!(fmt!("\n%s at index %u:\n%?\n!=\n%?\nInput:\n%?\n",
                           message, i, a, b, input))
            }
        }
        let len_r = results.len();
        let len_e = expected.len();
        if len_r != len_e {
            fail!(fmt!(
                "\n%s: expected %u items, got %u. First extra item:\n%?\nInput:\n%?\n",
                message, len_e, len_r,
                if len_r > len_e { &results[len_e] }
                else { &expected[len_r] },
                input))
        }
    }
}


//  ***********  End of public API  ***********


static ASCII_LOWER_OFFSET: char = 'a' - 'A';

#[test]
fn test_ascii_lower() {
    assert!(ascii_lower("url()URL()uRl()Ürl") == ~"url()url()url()Ürl");
    // Dotted capital I, Kelvin sign, Sharp S.
    assert!(ascii_lower("HİKß") == ~"hİKß");
}
