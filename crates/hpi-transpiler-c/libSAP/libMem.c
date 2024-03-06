#include "./libMem.h"
#include "./libAnyObj.h"
#include "./reflection.h"
#include "dynstring/dynstring.h"

void *alloc_maybe_trace(TypeDescriptor type,
                        void(trace_func)(void *addr, TypeDescriptor type)) {
  void *allocated = NULL;

  if (type.ptr_count >= 1) {
    switch (type.kind) {
    case TYPE_NONE:
      goto fail;
    case TYPE_INT:
    case TYPE_FLOAT:
    case TYPE_CHAR:
    case TYPE_BOOL:
    case TYPE_LIST:
    case TYPE_OBJECT:
    case TYPE_ANY_OBJECT:
    case TYPE_ANY_VALUE:
    case TYPE_STRING: {
      void *ptr = malloc(sizeof(void *));
      if (trace_func != NULL)
        trace_func(ptr, type);
      return ptr;
    }
    default:
      goto fail;
    }
  }

  switch (type.kind) {
  case TYPE_NONE:
  case TYPE_INT:
  case TYPE_FLOAT:
  case TYPE_CHAR:
  case TYPE_BOOL:
    goto fail;
  case TYPE_LIST: {
    ListNode *ptr = list_new();
    if (trace_func != NULL)
      trace_func(ptr, type);
    return ptr;
  }
  case TYPE_OBJECT: {
    HashMap *ptr = hashmap_new();
    if (trace_func != NULL)
      trace_func(ptr, type);
    return ptr;
  }
  case TYPE_ANY_OBJECT: {
    AnyObject *ptr = anyobj_new();
    if (trace_func != NULL)
      trace_func(ptr, type);
    return ptr;
  }
  case TYPE_ANY_VALUE: {
    AnyValue *ptr = malloc(sizeof(AnyValue));
    if (trace_func != NULL)
      trace_func(ptr, type);
    return ptr;
  }
  case TYPE_STRING: {
    DynString *ptr = dynstring_new();
    if (trace_func != NULL)
      trace_func(ptr, type);
    return ptr;
  }
  default:
    goto fail;
  }

fail: {
  char *typ = display_type(type);
  printf("gc_alloc(): illegal type: `%s`\n", typ);
  free(typ);
  abort();
}
}
