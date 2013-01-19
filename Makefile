run-tests: tests
	./tests

tests: cssparser.rc cssparser.rs tokenizer.rs
	rustc --test $< -o $@

.PHONY: run-tests
