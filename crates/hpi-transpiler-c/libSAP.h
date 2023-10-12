#include <stdio.h>
#include "/home/mik/Coding/hpi/hpi-c-tests/hashmap/map.h"

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
  HashMap * obj_fields;
  ssize_t ptr_count;
};

typedef struct TypeDescriptor TypeDescriptor;

void __hpi_internal_libSAP_reset();
void __hpi_internal_drucke(ssize_t num_args, ...);
