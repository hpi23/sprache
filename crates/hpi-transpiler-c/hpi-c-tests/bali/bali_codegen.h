#pragma once
#include "../dynstring/dynstring.h"

typedef struct {
  char *name;
  char *type;
} FunctionArg;

typedef struct {
  char *return_value;
  char *name;
  // begin args
  FunctionArg *args;
  ssize_t len_args;
} Function;

DynString *bali_codegen(Function *functions, ssize_t num_functions, char *class_header, void * instance_ptr);
