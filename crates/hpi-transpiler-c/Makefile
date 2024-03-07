.PHONY: run valgrind libSAP

HPI_FILE:=test.hpi
DEBUG:=true
CFLAGS:=-ggdb3
VG_EXEC:=./main

run: main
	./main

dbg: main
	gdb ./main

output.c: $(HPI_FILE) src/*.rs
	echo $(HPI_FILE)
	cargo r $(HPI_FILE) 1 1 1 1

libSAP:
	mkdir -p libSAP/obj
	cd libSAP && make

main: output.c libSAP
	gcc output.c -ggdb \
		./libSAP/libSAP.a \
		-lcurl \
		$(CFLAGS) -o main

valgrind: main
	valgrind --leak-check=full \
         --show-leak-kinds=all \
         --track-origins=yes \
		 --suppressions=valgrind.suppressions \
         $(VG_EXEC)

init_wrapper:
	# bash fetch_deps.sh
	cd ./libSAP/list/ && make
	cd ./libSAP/hashmap/ && make
	cd ./libSAP/json-parser/ && make
	cd ./libSAP/dynstring/ && make
	cd ./libSAP/vec/ && make

cleanSAP:
	rm -f ./libSAP/libSAP.a ./libSAP/libSAP_raw.a

.NOTPARALLEL:
init: init_wrapper cleanSAP libSAP
