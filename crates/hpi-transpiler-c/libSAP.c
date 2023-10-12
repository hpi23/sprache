#include "./libSAP.h"
#include "/home/mik/Coding/hpi/hpi-c-tests/dynstring/dynstring.h"
#include "/home/mik/Coding/hpi/hpi-c-tests/list/list.h"
#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

bool newline;
int indent;

void __hpi_internal_libSAP_reset() {
  indent = 4;
  newline = true;
}

void __hpi_internal_drucke(ssize_t num_args, ...) {
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
      int *bool_ = va_arg(args, int *);
      if (*bool_) {
        printf("%s", "true");
      } else {
        printf("%s", "false");
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
        __hpi_internal_drucke(1, new_type, list->value);
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

        TypeDescriptor type_descriptor = *(TypeDescriptor *)type_res.value;

        for (int i = 0; i < indent; i++) {
          printf(" ");
        }

        printf("%s: ", key);
        bool old_newline = newline;
        newline = false;
        indent += 4;
        __hpi_internal_drucke(1, type_descriptor, res.value);
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
