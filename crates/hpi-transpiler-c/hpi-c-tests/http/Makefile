main: main.c ../dynstring/*
	gcc -Wall -ggdb3 \
		-lcurl \
		../dynstring/dynstring.c \
		../list/list.c \
		main.c \
		-o main

valgrind: main
	valgrind --leak-check=full \
         --show-leak-kinds=all \
         --track-origins=yes \
         --verbose ./main

run: main
	./main
