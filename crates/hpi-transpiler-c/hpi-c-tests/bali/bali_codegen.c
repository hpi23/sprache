#include "./bali_codegen.h"
#include "../dynstring/dynstring.h"
#include <assert.h>
#include <stdio.h>
#include <sys/types.h>

char *codegen_params(FunctionArg *args, ssize_t len_args) {
  DynString *out = dynstring_new();

  for (int i = 0; i < len_args; i++) {
    dynstring_push_fmt(out, "%s %s", args[i].type, args[i].type);

    if (i + 1 < len_args) {
      dynstring_push_string(out, ", ");
    }
  }

  char *out_str = dynstring_as_cstr(out);
  dynstring_free(out);
  return out_str;
}

char *codegen_args(FunctionArg *args, ssize_t len_args, void * instance_ptr) {
  DynString *out = dynstring_new();

  dynstring_push_fmt(out, "(void *) %p", instance_ptr);
  if (len_args > 0) {
      dynstring_push_string(out, ", ");
  }

  for (int i = 0; i < len_args; i++) {
    dynstring_push_string(out, args[i].type);

    if (i + 1 < len_args) {
      dynstring_push_string(out, ", ");
    }
  }

  char *out_str = dynstring_as_cstr(out);
  dynstring_free(out);
  return out_str;
}

DynString *codegen_fn(Function fn, void * instance_ptr) {
  DynString *out = dynstring_new();

  dynstring_push_fmt(out, "%s bali_%s(%s) { return %s(%s); }", fn.return_value, fn.name, codegen_params(fn.args, fn.len_args), fn.name,
                     codegen_args(fn.args, fn.len_args, instance_ptr));

  return out;
}

DynString *bali_codegen(Function *functions, ssize_t num_functions, char *class_header, void * instance_ptr) {
  DynString *out = dynstring_new();

  dynstring_push_fmt(out, "#include \"../%s\"\n", class_header);

  for (int i = 0; i < num_functions; i++) {
    dynstring_push(out, codegen_fn(functions[i], instance_ptr));

    if (i + 1 < num_functions) {
      dynstring_push_string(out, "\n\n");
    }
  }

  return out;
}
