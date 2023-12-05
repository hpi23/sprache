SRC_DIR := .
OBJ_DIR := ./obj
SRC_FILES := $(wildcard $(SRC_DIR)/*.c)
OBJ_FILES := $(patsubst $(SRC_DIR)/%.c,$(OBJ_DIR)/%.o,$(SRC_FILES))

archiver := ar
gcc := gcc

libSAP.a: $(OBJ_FILES)
	@echo $(OBJ_FILES)
	$(archiver) -cq libSAP_raw.a $(OBJ_FILES)

	cd obj \
	&& $(archiver) -x ../../hpi-c-tests/dynstring/libDynstring.a \
	&& $(archiver) -x ../../hpi-c-tests/list/libList.a \
	&& $(archiver) -x ../../hpi-c-tests/json-parser/libJson.a \
	&& $(archiver) -x ../../hpi-c-tests/hashmap/libHashMap.a \

	$(archiver) -cq libSAP.a $(OBJ_FILES) obj/*.o


$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
	gcc -c -o $@ $<
