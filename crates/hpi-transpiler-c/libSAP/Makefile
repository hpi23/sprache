SRC_DIR := .
OBJ_DIR := ./obj
SRC_FILES := $(wildcard $(SRC_DIR)/*.c)
OBJ_FILES := $(patsubst $(SRC_DIR)/%.c,$(OBJ_DIR)/%.o,$(SRC_FILES))

archiver := ar
gcc := gcc

libSAP.a: $(OBJ_FILES)
	@echo $(OBJ_FILES)
	$(archiver) -cq libSAP.a $(OBJ_FILES)

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
	gcc -c -o $@ $<
