FILE:=output.c
COMPILE_WITH:=

build:
	gcc -c -ggdb -Wall -Werror -fpic $(COMPILE_WITH) -o ./runtime/$(COMPILE_WITH).o
	gcc -c -ggdb -Wall -Werror -fpic ./runtime/$(FILE) -o ./runtime/$(FILE).o
	gcc -ggdb -shared -o ./runtime/$(FILE).so ./runtime/$(FILE).o ./runtime/$(COMPILE_WITH).o

run: *.c
	rm -rf runtime
	mkdir -p runtime
	gcc -ggdb \
		main.c \
		../dynstring/dynstring.c \
		../hashmap/map.c \
		../list/list.c \
		./MyClass.c \
		./bali_codegen.c \
		./bali_dispatcher.c \
		-o main
	# LD_LIBRARY_PATH=$(PWD)/runtime ./main
	LD_LIBRARY_PATH=/home/mik/Coding/hpi/hpi-c-tests/bali/runtime ./main
