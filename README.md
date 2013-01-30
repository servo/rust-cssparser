rust-cssparser
==============

WIP rust implementation of the 2012 version of
[css3-syntax](http://dev.w3.org/csswg/css3-syntax/)


TODO
----

* [x] Tokenization
* [ ] Tree construction
  - [x] Declarations
  - [ ] Style rules
  - [ ] At-rules
* [ ] Detect character encoding & decode from bytes
* [ ] Packaging on cargo-central
* [ ] Track line/column number for errors & tokens.
* [ ] Figure out float and integer overflow
* [ ] Inclemental tokenization aka. accept "streamed" input instead of ~str
* [ ] Make it fast!
