.PHONY: run valgrind

HPI_FILE:=test.hpi
DEBUG:=true
CFLAGS:=-ggdb3

run: main
	./main

dbg: main
	gdb ./main

output.c: $(HPI_FILE) src/*.rs
	echo $(HPI_FILE)
	cargo r $(HPI_FILE)

libSAP/libSAP.a:
	cd libSAP && make

main: output.c libSAP/*.c libSAP/*.h libSAP/libSAP.a
	gcc output.c \
		-lcurl \
		-L ./libSAP \
		-lSAP \
		$(CFLAGS) -o main

valgrind: main
	valgrind --leak-check=full \
         --show-leak-kinds=all \
         --track-origins=yes \
         --verbose ./main

init:
	bash fetch_deps.sh
	cd ./hpi-c-tests/list/ && make
	cd ./hpi-c-tests/hashmap/ && make
	cd ./hpi-c-tests/json-parser/ && make
	cd ./hpi-c-tests/dynstring/ && make
