#pragma once
#include "hashmap/map.h"
#include <sys/types.h>

typedef enum {
  TYPE_NONE,
  TYPE_INT,
  TYPE_FLOAT,
  TYPE_CHAR,
  TYPE_BOOL,
  TYPE_LIST,
  TYPE_OBJECT,
  TYPE_ANY_OBJECT,
  TYPE_ANY_VALUE,
  TYPE_STRING,
} TypeKind;

struct TypeDescriptor {
  TypeKind kind;
  struct TypeDescriptor *list_inner;
  HashMap *obj_fields;
  ssize_t ptr_count;
};

typedef struct TypeDescriptor TypeDescriptor;

char *display_type(TypeDescriptor type);
