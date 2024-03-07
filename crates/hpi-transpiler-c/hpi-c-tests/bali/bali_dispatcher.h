#include "../hashmap/map.h"
#include "./bali_codegen.h"

HashMap *new_dispatcher(Function *functions, ssize_t num_functions, void *ptr_to_instance, char *class_name, char *class_header_path);
