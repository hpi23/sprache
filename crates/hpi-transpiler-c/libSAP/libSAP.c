#include "./format.h"
#include "/home/mik/Coding/hpi/hpi-c-tests/dynstring/dynstring.h"
#include "/home/mik/Coding/hpi/hpi-c-tests/list/list.h"
#include "libAnyObj.h"
#include "libTime.h"
#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

extern char **environ;

bool newline;
int indent;

void __hpi_internal_libSAP_reset() {
  indent = 4;
  newline = true;
}

int64_t __hpi_internal_generate_matrikelnummer() {
  // TODO: use lfsr instead of just time

  TimeStruct t = __hpi_internal_time_provider();

  return t.second * t.minute * t.hour * t.year;
}

void __hpi_internal_print(ssize_t num_args, ...) {
  va_list args;

  va_start(args, num_args);

  for (int i = 0; i < num_args; i++) {
    TypeDescriptor type = va_arg(args, TypeDescriptor);

    switch (type.kind) {
    case TYPE_INT: {
      int64_t *number = va_arg(args, int64_t *);
      printf("%ld", *number);
      break;
    }
    case TYPE_FLOAT: {
      double *number = va_arg(args, double *);
      printf("%f", *number);
      break;
    }
    case TYPE_CHAR: {
      int *character = va_arg(args, int *);
      printf("%c", *character);
      break;
    }
    case TYPE_BOOL: {
      bool *bool_ = va_arg(args, bool *);
      if (*bool_) {
        printf("%s", "ja");
      } else {
        printf("%s", "nein");
      }
      break;
    }
    case TYPE_LIST: {
      ListNode **list_ptr = va_arg(args, ListNode **);
      ListNode *list = *list_ptr;

      printf("[");

      while (list != NULL) {
        TypeDescriptor new_type = {.kind = type.list_inner->kind,
                                   .list_inner = type.list_inner->list_inner,
                                   .ptr_count = 0};

        bool old_newline = newline;
        newline = false;
        __hpi_internal_print(1, new_type, list->value);
        newline = old_newline;

        list = list->next;
        if (list != NULL) {
          printf(", ");
        }
      }

      printf("]");

      break;
    }
    case TYPE_OBJECT: {
      HashMap **map_ptr = va_arg(args, HashMap **);
      HashMap *map = *map_ptr;

      ListNode *keys = hashmap_keys(map);

      printf("Objekt {\n");

      while (keys != NULL) {
        char *key = keys->value;
        MapGetResult res = hashmap_get(map, key);
        assert(res.found);

        MapGetResult type_res = hashmap_get(type.obj_fields, key);
        assert(type_res.found);

        TypeDescriptor *type_descriptor = (TypeDescriptor *)type_res.value;

        for (int i = 0; i < indent; i++) {
          printf(" ");
        }

        printf("%s: ", key);
        bool old_newline = newline;
        newline = false;
        indent += 4;
        __hpi_internal_print(1, *type_descriptor, res.value);
        indent -= 4;
        newline = old_newline;

        if (keys->next != NULL) {
          printf(",");
          printf("\n");
        }

        keys = keys->next;
      }

      printf("\n");
      for (int i = 0; i < indent - 4; i++) {
        printf(" ");
      }

      printf("}");

      break;
    }
    case TYPE_ANY_OBJECT: {
      AnyObject *obj = *va_arg(args, AnyObject **);

      ListNode *keys = hashmap_keys(obj->fields);

      printf("Speicherbox {\n");

      ssize_t keys_len = list_len(keys);

      for (int i = 0; i < keys_len; i++) {
        char *key = (char *)list_at(keys, i).value;
        MapGetResult res = hashmap_get(obj->fields, key);
        assert(res.found);

        AnyValue *item = res.value;

        for (int i = 0; i < indent; i++) {
          printf(" ");
        }

        printf("%s: ", key);
        bool old_newline = newline;
        newline = false;
        indent += 4;
        __hpi_internal_print(1, item->type, &item->value);
        indent -= 4;
        newline = old_newline;

        if (i + 1 < keys_len) {
          printf(",");
          printf("\n");
        }
      }

      printf("\n");
      for (int i = 0; i < indent - 4; i++) {
        printf(" ");
      }

      printf("}");

      break;
    }
    case TYPE_STRING: {
      DynString **string = va_arg(args, DynString **);
      char *string_raw = dynstring_as_cstr(*string);

      printf("%s", string_raw);

      free(string_raw);
      break;
    }
    }

    if (i < num_args && num_args > 1) {
      printf(" ");
    }
  }

  va_end(args);
  if (newline) {
    printf("\n");
  }
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

void __hpi_internal_sleep(double duration) {
  // TODO: implement this
}

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

    AnyValue anyvalue = {.type = string_type_descriptor, .value = value.value};

    anyobj_insert(obj, key_cstr, anyvalue);

    free(key_cstr);
    dynstring_free(key.value);
  }

  return obj;
}
