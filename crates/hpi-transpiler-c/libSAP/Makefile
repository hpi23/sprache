SRC_DIR := .
OBJ_DIR := ./obj
SRC_FILES := $(wildcard $(SRC_DIR)/*.c)
OBJ_FILES := $(patsubst $(SRC_DIR)/%.c,$(OBJ_DIR)/%.o,$(SRC_FILES))

archiver := ar
GCC := gcc
GCC_FLAGS :=-ggdb

libSAP.a: $(OBJ_FILES)
	@echo $(OBJ_FILES)
	$(archiver) -cq libSAP_raw.a $(OBJ_FILES)

	cd obj \
	&& $(archiver) -x ../dynstring/libDynstring.a \
	&& $(archiver) -x ../list/libList.a \
	&& $(archiver) -x ../vec/libVec.a \
	&& $(archiver) -x ../json-parser/libJson.a \
	&& $(archiver) -x ../hashmap/libHashMap.a \

	$(archiver) -cq libSAP.a $(OBJ_FILES) obj/*.o


$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
	$(GCC) $(GCC_FLAGS) -c -o $@ $<
