SRC_DIR := .
OBJ_DIR := ./obj
SRC_FILES := $(wildcard $(SRC_DIR)/*.c)

archiver := ar
gcc := gcc

clean:
	rm -f main

main: ../dynstring/dynstring.c ../list/list.c ../vec/vec.c lexer.c token.c span.c
	gcc -ggdb ../dynstring/dynstring.c ../list/list.c ../vec/vec.c lexer.c token.c span.c main.c -o main

run: main
	./main

debug: build
	gdb main
