#pragma once
#include "/home/mik/Coding/hpi/hpi-c-tests/hashmap/map.h"

typedef enum {
  TYPE_INT,
  TYPE_FLOAT,
  TYPE_CHAR,
  TYPE_BOOL,
  TYPE_LIST,
  TYPE_OBJECT,
  TYPE_ANY_OBJECT,
  TYPE_STRING,
} TypeKind;

struct TypeDescriptor {
  TypeKind kind;
  struct TypeDescriptor *list_inner;
  HashMap *obj_fields;
  ssize_t ptr_count;
};

typedef struct TypeDescriptor TypeDescriptor;
