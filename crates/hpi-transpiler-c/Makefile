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
	bat output.c

main: output.c libSAP/*.c libSAP/*.h
	gcc output.c \
		-lcurl \
		libSAP/*.c \
		~/Coding/hpi/hpi-c-tests/list/list.c \
		~/Coding/hpi/hpi-c-tests/dynstring/dynstring.c \
		~/Coding/hpi/hpi-c-tests/hashmap/map.c \
		~/Coding/hpi/hpi-c-tests/json-parser/json.c \
		~/Coding/hpi/hpi-c-tests/json-parser/parser.c \
		~/Coding/hpi/hpi-c-tests/json-parser/lexer.c \
		~/Coding/hpi/hpi-c-tests/json-parser/token.c \
		$(CFLAGS) -o main

valgrind: main
	valgrind --leak-check=full \
         --show-leak-kinds=all \
         --track-origins=yes \
         --verbose ./main
