run-tests: tests
	./tests

tests: cssparser.rs
	rustc --test $< -o $@

.PHONY: run-tests
