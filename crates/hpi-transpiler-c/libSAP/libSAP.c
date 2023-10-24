#include "./format.h"
#include "./to_string.h"
#include "../hpi-c-tests/dynstring/dynstring.h"
#include "../hpi-c-tests/list/list.h"
#include "libAnyObj.h"
#include "libTime.h"
#include "reflection.h"
#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

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

    TypeDescriptor string_type_descriptor = {.kind = TYPE_STRING,
                                             .ptr_count = 0};

    DynString **val = malloc(sizeof(DynString *));
    *val = value.value;

    AnyValue anyvalue = {.type = string_type_descriptor, .value = val};

    anyobj_insert(obj, key_cstr, anyvalue);

    free(key_cstr);
    dynstring_free(key.value);
  }

  return obj;
}
