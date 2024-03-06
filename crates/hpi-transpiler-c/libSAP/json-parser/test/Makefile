main: main.c ../parser.c ../lexer.c ../token.c ../json.c ../../dynstring/*
	gcc -Wall -ggdb \
		../parser.c \
		../lexer.c \
		../token.c \
		../json.c \
		../../hashmap/map.c \
		../../list/list.c \
		../../dynstring/dynstring.c \
		main.c \
		-o main

valgrind: main
	valgrind --leak-check=full \
         --show-leak-kinds=all \
         --track-origins=yes \
         --verbose ./main

run: main
	./main
