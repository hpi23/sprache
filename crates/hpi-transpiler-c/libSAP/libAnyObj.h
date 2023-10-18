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
ListNode *__hpi_internal_anyobj_keys(AnyObject *obj);
AnyValue __hpi_internal_anyobj_take(AnyObject *obj, DynString *key);
void anyobj_insert(AnyObject *obj, char *key, AnyValue value);
char *display_type(TypeDescriptor type);

// Runtime type casting
void *__hpi_internal_runtime_cast(AnyValue from, TypeDescriptor as_type);
