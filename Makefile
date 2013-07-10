RUSTC ?= rustc
RUSTFLAGS ?=

RUST_SRC=tokenizer.rs parser.rs ast.rs tests.rs

.PHONY: all
all:    libcssparser.dummy

libcssparser.dummy: cssparser.rc $(RUST_SRC)
	$(RUSTC) $(RUSTFLAGS) $< -o $@
	touch $@

cssparser-test: cssparser.rc $(RUST_SRC)
	$(RUSTC) $(RUSTFLAGS) $< -o $@ --test

.PHONY: check
check: cssparser-test
	./cssparser-test

.PHONY: check-debug
check-debug: cssparser-tests
	echo -e "start\n break upcall_fail\n continue\n where\n continue" | gdb -q ./cssparser-tests

.PHONY: clean
clean:
	rm -f *.o *.a *.so *.dylib *.dll *.dummy *-test
