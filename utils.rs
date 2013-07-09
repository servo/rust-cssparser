use std::io;
use std::os;
use std::run;
use std::task;
use extra::json;
use extra::tempfile;


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
fn write_whole_file(path: &Path, data: &str) {
    match io::file_writer(path, [io::Create]) {
        Ok(writer) => writer.write_str(data),
        Err(message) => fail!(message),
    }
}


#[cfg(test)]
pub fn assert_json_eq(results: json::Json, expected: json::Json, message: &str) {
    if results != expected {
        let temp = tempfile::mkdtemp(&os::tmpdir(), "rust-cssparser-tests").get();
        let temp_ = copy temp;
        let results = json::to_pretty_str(&results) + "\n";
        let expected = json::to_pretty_str(&expected) + "\n";
        do task::try {
            let result_path = temp.push("results.json");
            let expected_path = temp.push("expected.json");
            write_whole_file(&result_path, results);
            write_whole_file(&expected_path, expected);
            run::process_status("colordiff", [~"-u1000", result_path.to_str(),
                                              expected_path.to_str()]);
        };
        os::remove_dir_recursive(&temp_);

        fail!(fmt!("%s", message))
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
