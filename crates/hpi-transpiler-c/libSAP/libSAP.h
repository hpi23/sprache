#pragma once
#include "/home/mik/Coding/hpi/hpi-c-tests/dynstring/dynstring.h"
#include "/home/mik/Coding/hpi/hpi-c-tests/hashmap/map.h"
#include <stdio.h>

typedef enum {
  TYPE_INT,
  TYPE_FLOAT,
  TYPE_CHAR,
  TYPE_BOOL,
  TYPE_LIST,
  TYPE_OBJECT,
  TYPE_STRING,
} TypeKind;

struct TypeDescriptor {
  TypeKind kind;
  struct TypeDescriptor *list_inner;
  HashMap *obj_fields;
  ssize_t ptr_count;
};

typedef struct TypeDescriptor TypeDescriptor;

void __hpi_internal_libSAP_reset();
void __hpi_internal_drucke(ssize_t num_args, ...);
int64_t __hpi_internal_generate_matrikelnummer();
DynString *__hpi_internal_fmt(ssize_t num_args, DynString *fmt, ...);
