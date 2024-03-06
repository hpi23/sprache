main: ../map.c main.c ../map.h ../../list/*
	gcc -Wall -ggdb3 ../map.c main.c ../../list/list.c -o main

valgrind: main
	valgrind --leak-check=full \
         --show-leak-kinds=all \
         --track-origins=yes \
         --verbose ./main

run: main
	./main
