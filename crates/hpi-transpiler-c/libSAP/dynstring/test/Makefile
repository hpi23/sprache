main: ../dynstring.c main.c
	gcc -Wall -ggdb3 \
		../dynstring.c \
		../../list/list.c \
		main.c -o main

valgrind: main
	valgrind --leak-check=full \
         --show-leak-kinds=all \
         --track-origins=yes \
         --verbose ./main

run: main
	./main
