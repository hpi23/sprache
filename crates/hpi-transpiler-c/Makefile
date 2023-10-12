.PHONY: run

HPI_FILE:=test.hpi

run:
	cargo r $(HPI_FILE)
	bat output.c
	gcc output.c \
		libSAP.c \
		~/Coding/hpi/hpi-c-tests/list/list.c \
		~/Coding/hpi/hpi-c-tests/dynstring/dynstring.c \
		-ggdb3 -o main
	./main
