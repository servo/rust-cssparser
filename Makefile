run-tests: cssparser-tests
	./cssparser-tests

debug: cssparser-tests
	echo -e "start\n break upcall_fail\n continue\n where\n continue" | gdb -q ./cssparser-tests

cssparser-tests: cssparser.rc tokenizer.rs parser.rs ast.rs tests.rs
	rustc --test $< -o $@

.PHONY: run-tests debug
