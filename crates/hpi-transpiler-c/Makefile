.PHONY: run valgrind

HPI_FILE:=test.hpi
DEBUG:=true
CFLAGS:=-ggdb3

run: main
	./main

dbg: main
	gdb ./main

output.c: $(HPI_FILE) src/*.rs
	cargo r $(HPI_FILE)

main: output.c libSAP/*.c libSAP/*.h
	gcc output.c \
		-lcurl \
		libSAP/*.c \
		hpi-c-tests/list/list.c \
		hpi-c-tests/dynstring/dynstring.c \
		hpi-c-tests/hashmap/map.c \
		hpi-c-tests/json-parser/json.c \
		hpi-c-tests/json-parser/parser.c \
		hpi-c-tests/json-parser/lexer.c \
		hpi-c-tests/json-parser/token.c \
		$(CFLAGS) -o main

valgrind: main
	valgrind --leak-check=full \
         --show-leak-kinds=all \
         --track-origins=yes \
         --verbose ./main

init:
	bash fetch_deps.sh
