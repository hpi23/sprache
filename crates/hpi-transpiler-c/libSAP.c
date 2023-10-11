#include "./libSAP.h"
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include "/home/mik/Coding/hpi/hpi-c-tests/list/list.h"

void __hpi_internal_drucke(TypeDescriptor type, ...) {
  va_list args;

  va_start(args, type);

  switch (type.kind) {
  case TYPE_INT: {
    int64_t * number = va_arg(args, int64_t *);
    printf("%ld", *number);
    break;
  }
  case TYPE_FLOAT: {
    double *number = va_arg(args, double*);
    printf("%f", *number);
    break;
  }
  case TYPE_CHAR: {
    int *character = va_arg(args, int*);
    printf("%c", *character);
    break;
  }
  case TYPE_BOOl: {
    int * bool_ = va_arg(args, int*);
    if (*bool_) {
      printf("%s", "true");
    } else {
      printf("%s", "false");
    }
    break;
  }
  case TYPE_LIST: {
    ListNode * list = va_arg(args, ListNode *);

    printf("[");

    while (list != NULL) {
        TypeDescriptor new_type = { .kind = type.inner->kind, .inner = NULL, .ptr_count = 0};
        __hpi_internal_drucke(new_type, list->value);
    }

    printf("]");

    break;
  }
  case TYPE_STRING:
    break;
  }

  va_end(args);
  printf("\n");
}
