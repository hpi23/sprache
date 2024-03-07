#include "dynstring/dynstring.h"
#include "format.h"
#include "libAnyObj.h"
#include "libGC.h"
#include "libTime.h"
#include "list/list.h"
#include "reflection.h"
#include "to_string.h"
#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

size_t argc;
char **argv;

extern char **environ;

int indent;

void __hpi_internal_libSAP_reset() { indent = 4; }

int64_t __hpi_internal_generate_matrikelnummer() {
  // TODO: use lfsr instead of just time
  TimeStruct t = __hpi_internal_time_provider();
  return t.second * t.minute * t.hour * t.year;
}

void __hpi_internal_print(ssize_t num_args, ...) {
  va_list args;

  va_start(args, num_args);

  DynString *output = dynstring_new();

  for (int i = 0; i < num_args; i++) {
    TypeDescriptor type = va_arg(args, TypeDescriptor);
    void *value = va_arg(args, void *);

    DynString *res = to_string(type, value);
    dynstring_push(output, res);
    dynstring_free(res);

    if (i < num_args && num_args > 1) {
      dynstring_push_char(output, ' ');
    }
  }

  dynstring_print(output);
  dynstring_free(output);

  va_end(args);
}

DynString *__hpi_internal_fmt(ssize_t num_args, DynString *fmt, ...) {
  va_list args;

  va_start(args, fmt);

  char *fmt_str = dynstring_as_cstr(fmt);

  ListNode *input_args_temp = list_new();
  ListNode *input_args = input_args_temp;

  for (int i = 0; i < num_args; i++) {
    FmtArg *fmt_arg = malloc(sizeof(FmtArg));
    TypeDescriptor fmt_type = va_arg(args, TypeDescriptor);
    FmtArg *arg = (FmtArg *)malloc(sizeof(FmtArg));
    arg->value = va_arg(args, void *);
    arg->type = fmt_type;
    list_append(input_args, arg);
  }

  Formatter *formatter = formatter_new(fmt_str, input_args);
  DynString *output = formatter_fmt(formatter);

  list_free(input_args_temp);

  return output;
}

void __hpi_internal_sleep(double duration) { sleep(duration); }

AnyObject *__hpi_internal_env() {
  AnyObject *obj = anyobj_new();

  for (char **env = environ; *env; env++) {
    DynString *env_item_raw = dynstring_from(*env);
    ListNode *split = dynstring_split_cstr(env_item_raw, "=", 1);

    ListGetResult key = list_at(split, 0);
    assert(key.found);
    ListGetResult value = list_at(split, 1);
    assert(value.found);

    char *key_cstr = dynstring_as_cstr(key.value);

    TypeDescriptor string_type_descriptor = {.kind = TYPE_STRING, .ptr_count = 0};

    DynString **val = malloc(sizeof(DynString *));
    *val = value.value;

    AnyValue anyvalue = {.type = string_type_descriptor, .value = val};

    anyobj_insert(obj, key_cstr, anyvalue);

    free(key_cstr);
    dynstring_free(key.value);
  }

  return obj;
}

ListNode *__hpi_internal_args(void *(allocator)(TypeDescriptor type), void(tracer)(void *addr, TypeDescriptor type, TypeDescriptor *type_heap)) {
  // ListNode *list = list_new();
  TypeDescriptor type_string = {.kind = TYPE_STRING, .obj_fields = NULL, .ptr_count = 0, .list_inner = NULL};

  TypeDescriptor *type_list_str_heap = malloc(sizeof(TypeDescriptor));
  *type_list_str_heap = type_string;

  // ListNode *list = allocator();

  ListNode *list = list_new();
  if (tracer != NULL)
    tracer(list, (TypeDescriptor){.kind = TYPE_LIST, .list_inner = type_list_str_heap, .ptr_count = 0, .obj_fields = NULL}, type_list_str_heap);

  for (int i = 0; i < argc; i++) {
    // TypeDescriptor type_string = {.kind = TYPE_STRING, .ptr_count = 0, .list_inner = NULL};

    // DynString **temp_ptr = malloc(sizeof(DynString *));

    TypeDescriptor type_ptr_string = type_string;
    type_ptr_string.ptr_count = 1;
    DynString **temp_ptr = allocator(type_ptr_string);

    DynString *dynstr_from = dynstring_from(argv[i]);
    *temp_ptr = dynstr_from;
    if (tracer != NULL)
      tracer(dynstr_from, type_string, NULL);

    list_append(list, temp_ptr);
  }

  return list;
}

void __hpi_internal_init_libSAP(size_t p_argc, char **p_argv) {
  argc = p_argc;
  argv = p_argv;
}
