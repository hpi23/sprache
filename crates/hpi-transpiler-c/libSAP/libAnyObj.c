#include "./libAnyObj.h"
#include "reflection.h"
#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>

void anyobj_insert(AnyObject *obj, char *key, AnyValue value) {
  AnyValue *value_heap = (AnyValue *)malloc(sizeof(AnyValue));
  *value_heap = value;
  hashmap_insert(obj->fields, key, value_heap);
}

AnyObject *anyobj_new() {
  AnyObject *obj = malloc(sizeof(AnyObject));
  obj->fields = hashmap_new();
  return obj;
}

void anyobj_free(AnyObject * obj) {
    assert(0 && "Not implemented");
}

AnyValue __hpi_internal_anyobj_take(AnyObject *obj, DynString *key) {
  char *key_c = dynstring_as_cstr(key);
  MapGetResult res = hashmap_get(obj->fields, key_c);
  free(key_c);

  if (res.found) {
    return *(AnyValue *)res.value;
  } else {
    TypeDescriptor val_type = {.kind = TYPE_NONE};
    AnyValue val = {.value = NULL, .type = val_type};
    return val;
  }
}

ListNode *__hpi_internal_anyobj_keys(AnyObject *obj) {
  ListNode *raw_keys = hashmap_keys(obj->fields);

  ListNode *new_list = list_new();

  for (ssize_t i = 0; i < list_len(raw_keys); i++) {
    ListGetResult temp = list_at(raw_keys, i);
    assert(temp.found);

    DynString **str = malloc(sizeof(DynString *));
    *str = dynstring_from(temp.value);
    list_append(new_list, str);
    free(temp.value);
  }

  list_free(raw_keys);

  return new_list;
}

void *__hpi_internal_runtime_cast(AnyValue from, TypeDescriptor as_type) {
  // Detect basic type mismatch
  if (from.type.kind != as_type.kind ||
      from.type.ptr_count != as_type.ptr_count) {

    if (from.type.kind == TYPE_FLOAT && as_type.kind == TYPE_INT) {
      int64_t *as_int = malloc(sizeof(int64_t));
      *as_int = (int64_t) * (double *)from.value;
      return as_int;
    } else if (from.type.kind == TYPE_INT && as_type.kind == TYPE_FLOAT) {
      double *as_double = malloc(sizeof(double));
      *as_double = (double)*(int64_t *)from.value;
      return as_double;
    }

    goto fail;
  }

  switch (from.type.kind) {
  case TYPE_NONE:
    assert(0);
  case TYPE_INT:
  case TYPE_FLOAT:
  case TYPE_CHAR:
  case TYPE_BOOL:
  case TYPE_STRING:
    return from.value;
  case TYPE_LIST: {
    ListNode *new_list = list_new();

    ListNode *old_list = *(ListNode **)from.value;
    ssize_t len = list_len(old_list);

    for (int i = 0; i < len; i++) {
      ListGetResult res = list_at(old_list, i);
      assert(res.found);

      AnyValue *val = (AnyValue *)res.value;
      list_append(new_list,
                  __hpi_internal_runtime_cast(*val, *as_type.list_inner));

      // cleanup
      free(res.value);
    }

    // cleanup
    list_free(old_list);

    ListNode **list_ptr = malloc(sizeof(ListNode *));
    *list_ptr = new_list;
    return list_ptr;
  }
  case TYPE_OBJECT: {
    // TODO: check this, (is obj -> any obj allowed?)
  }
  case TYPE_ANY_OBJECT:
    if (from.type.kind == TYPE_ANY_OBJECT) {
      return from.value;
    }
    break;
  }

fail:
  printf("Runtime error: Unsupported cast: Cannot cast value of type `%s` to "
         "`%s`\n",
         display_type(from.type), display_type(as_type));
  assert(0);
  exit(-1);
}
