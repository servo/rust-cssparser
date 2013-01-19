extern mod std;
use str;


// TODO http://dev.w3.org/csswg/css3-syntax/#the-input-byte-stream
// fn decode(input: &[u8], protocol_encoding: &str, link_encoding: &str,
//           document_encoding: &str) -> ~str
// Use empty strings for “no such encoding information”?


// http://dev.w3.org/csswg/css3-syntax/#preprocessing-the-input-stream
fn preprocess(input: &str) -> ~str {
    // TODO: Is this faster if done in one pass?
    str::replace(str::replace(str::replace(input,
    "\r\n", "\n"),
    "\r", "\n"),
    "\x00", "\uFFFD")
}


#[test]
fn test_preprocess() {
    assert preprocess("") == ~"";
    assert preprocess("Lorem\r\n\t\x00ipusm\ndoror\uFFFD\r")
        == ~"Lorem\n\t\uFFFDipusm\ndoror\uFFFD\n";
}
