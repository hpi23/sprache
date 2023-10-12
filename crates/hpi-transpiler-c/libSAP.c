#include "./libSAP.h"
#include "/home/mik/Coding/hpi/hpi-c-tests/dynstring/dynstring.h"
#include "/home/mik/Coding/hpi/hpi-c-tests/list/list.h"
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

bool newline = true;

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
    case TYPE_BOOl: {
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
        TypeDescriptor new_type = {.kind = type.inner->kind,
                                   .inner = type.inner->inner,
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
    case TYPE_STRING: {
      DynString **string = va_arg(args, DynString **);
      char * string_raw = dynstring_as_cstr(*string);

      printf("%s", string_raw);

      free(string_raw);
      break;
    }
    }

    if (i < num_args) {
      printf(" ");
    }
  }

  va_end(args);
  if (newline) {
    printf("\n");
  }
}
