#include <stdio.h>

typedef enum {
  TYPE_INT,
  TYPE_FLOAT,
  TYPE_CHAR,
  TYPE_BOOl,
  TYPE_LIST,
  TYPE_STRING,
} TypeKind;

struct TypeDescriptor {
  TypeKind kind;
  struct TypeDescriptor *inner;
  ssize_t ptr_count;
};

typedef struct TypeDescriptor TypeDescriptor;

void __hpi_internal_drucke(ssize_t num_args, ...);
