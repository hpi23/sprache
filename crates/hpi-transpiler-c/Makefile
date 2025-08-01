.PHONY: run valgrind libSAP

HPI_FILE:=test.hpi
DEBUG:=true
CFLAGS:=-ggdb3
VG_EXEC:=./main
RARGS:=

run: main
	./main $(RARGS)

dbg: main
	gdb ./main

output.c: $(HPI_FILE) src/*.rs
	echo $(HPI_FILE)
	cargo r $(HPI_FILE) 1 1 1 0

libSAP:
	mkdir -p libSAP/obj
	cd libSAP && make

lib: libSAP
	echo $(HPI_FILE)
	cargo r $(HPI_FILE) 1 1 1 1

main: output.c libSAP
	gcc output.c -ggdb \
		./libSAP/libSAP.a \
		-lcurl \
		-lm \
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
	rm -rf ./libSAP/**/obj/
	rm -f ./libSAP/libSAP.a ./libSAP/libSAP_raw.a
	rm -f ./libSAP/**/*.a

clean: cleanSAP
	rm output.c

.NOTPARALLEL:
init: cleanSAP init_wrapper  libSAP
