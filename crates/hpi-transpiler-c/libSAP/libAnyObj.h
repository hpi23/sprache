#pragma once
#include "./reflection.h"
#include "/home/mik/Coding/hpi/hpi-c-tests/dynstring/dynstring.h"
#include "/home/mik/Coding/hpi/hpi-c-tests/hashmap/map.h"

typedef struct {
  TypeDescriptor type;
  void *value;
} AnyValue;

typedef struct {
  HashMap *fields; // Inner type: AnyValue
} AnyObject;

// AnyObj utility functions
AnyObject *anyobj_new();
ListNode *__hpi_internal_anyobj_keys(HashMap obj);
AnyValue __hpi_internal_anyobj_take(AnyObject *obj, DynString *key);
void anyobj_insert(AnyObject *obj, char *key, AnyValue value);

// Runtime type validation
void __hpi_internal_validate_runtime_cast(TypeDescriptor as_type,
                                          TypeDescriptor from_type);
