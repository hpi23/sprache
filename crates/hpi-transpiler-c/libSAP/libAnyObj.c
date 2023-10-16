#include "./libAnyObj.h"
#include "reflection.h"
#include <assert.h>

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

void __hpi_internal_validate_runtime_cast(TypeDescriptor as_type,
                                          TypeDescriptor from_type) {
  assert("TODO" && false);
}
