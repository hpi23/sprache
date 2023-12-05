.PHONY: run valgrind libSAP

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

libSAP:
	mkdir -p libSAP/obj
	cd libSAP && make

main: output.c libSAP
	gcc output.c \
		./libSAP/libSAP.a \
		-lcurl \
		$(CFLAGS) -o main

valgrind: main
	valgrind --leak-check=full \
         --show-leak-kinds=all \
         --track-origins=yes \
         --verbose ./main

init_wrapper:
	bash fetch_deps.sh
	cd ./hpi-c-tests/list/ && make
	cd ./hpi-c-tests/hashmap/ && make
	cd ./hpi-c-tests/json-parser/ && make
	cd ./hpi-c-tests/dynstring/ && make

cleanSAP:
	rm -f ./libSAP/libSAP.a ./libSAP/libSAP_raw.a

.NOTPARALLEL:
init: init_wrapper cleanSAP libSAP
