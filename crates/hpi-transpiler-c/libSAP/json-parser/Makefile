SRC_DIR := .
OBJ_DIR := ./obj
SRC_FILES := $(wildcard $(SRC_DIR)/*.c)
OBJ_FILES := $(patsubst $(SRC_DIR)/%.c,$(OBJ_DIR)/%.o,$(SRC_FILES))

archiver := ar
gcc := gcc

libJson.a: $(OBJ_FILES)
	@echo $(OBJ_FILES)
	$(archiver) -cq libJson.a $(OBJ_FILES)

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
	mkdir -p obj
	gcc -c -o $@ $<
