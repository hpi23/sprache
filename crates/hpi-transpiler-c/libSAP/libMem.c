#pragma once
#include "./reflection.h"
#include "./libMem.h"
#include "./libAnyObj.h"
#include "../hpi-c-tests/dynstring/dynstring.h"
#include <assert.h>

void *non_tracing_alloc(TypeDescriptor type) {
  void *allocated = NULL;

  if (type.ptr_count >= 1) {
    switch (type.kind) {
    case TYPE_NONE:
      assert(0 && "This type is not supported");
    case TYPE_INT:
    case TYPE_FLOAT:
    case TYPE_CHAR:
    case TYPE_BOOL:
    case TYPE_LIST:
    case TYPE_OBJECT:
    case TYPE_ANY_OBJECT:
    case TYPE_STRING:
      return malloc(sizeof(void *));
    }
  }

  switch (type.kind) {
  case TYPE_NONE:
  case TYPE_INT:
  case TYPE_FLOAT:
  case TYPE_CHAR:
  case TYPE_BOOL:
    assert(0 && "This type is not supported");
  case TYPE_LIST:
    return list_new();
  case TYPE_OBJECT:
    return hashmap_new();
  case TYPE_ANY_OBJECT:
    return anyobj_new();
  case TYPE_STRING:
    return dynstring_new();
  default: {
    char *typ = display_type(type);
    printf("gc_alloc(): illegal type: `%s`\n", typ);
    free(typ);
    assert(0 && "GC crashed");
  }
  }
}
