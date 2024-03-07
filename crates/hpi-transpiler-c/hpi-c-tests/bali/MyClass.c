#include "./MyClass.h"
#include "bali_codegen.h"
#include "bali_dispatcher.h"
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

MyClass *new_MyClass(int64_t value) {
  MyClass temp = {.value = value};

  MyClass *ptr_temp = malloc(sizeof(MyClass));

  if (MyClass_VERBOSE) {
    printf("=== Configuring new instance ===\n");
  }

  ssize_t num_functions = 3;
  Function *functions = malloc(sizeof(Function) * num_functions);
  functions[0].name = "myclass_get_value";
  functions[0].len_args = 0;
  functions[0].return_value = "int64_t";

  functions[1].name = "myclass_set_value";
  functions[1].len_args = 1;
  functions[1].args = malloc(sizeof(FunctionArg));
  functions[1].args[0].name = "value";
  functions[1].args[0].type = "int64_t";
  functions[1].return_value = "void";

  functions[2].name = "myclass_print";
  functions[2].len_args = 0;
  functions[2].return_value = "void";

  HashMap *methods = new_dispatcher(functions, num_functions, ptr_temp, "MyClass", "MyClass.h");

  MapGetResult res = hashmap_get(methods, "myclass_get_value");
  assert(res.found);
  temp.get_value = res.value;

  res = hashmap_get(methods, "myclass_set_value");
  assert(res.found);
  temp.set_value = res.value;

  res = hashmap_get(methods, "myclass_print");
  assert(res.found);
  temp.print = res.value;

  // temp.set_value = methods.set_value;
  // temp.get_value = methods.get_value;

  if (MyClass_VERBOSE) {
    printf("\tDynamic dispatch configured.\n");
    printf("=== New instance configured ===\n");
  }

  *ptr_temp = temp;
  return ptr_temp;
}
