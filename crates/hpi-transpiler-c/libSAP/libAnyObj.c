#include "./libAnyObj.h"
#include "reflection.h"
#include <assert.h>
#include <stdio.h>
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

char *display_type(TypeDescriptor type) { return "TODO"; }

void __hpi_internal_validate_runtime_cast(TypeDescriptor as_type,
                                          TypeDescriptor from_type) {

  if (as_type.ptr_count != from_type.ptr_count ||
      as_type.kind != from_type.kind) {
    goto fail;
  }

  switch (from_type.kind) {
  case TYPE_LIST: {
    // TODO: validate inner types
  }
  }

  return;

fail:
  printf("Runtime error: Unsupported cast: Cannot cast value of type `%s` to "
         "`%s`\n",
         display_type(from_type), display_type(as_type));
  exit(-1);
}
