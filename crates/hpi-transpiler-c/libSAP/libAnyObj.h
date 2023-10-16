#pragma once
#include "/home/mik/Coding/hpi/hpi-c-tests/hashmap/map.h"
#include "./reflection.h"

typedef struct {
  TypeDescriptor type;
  void *value;
} AnyValue;

typedef struct {
  HashMap *fields; // Inner type: AnyValue
} AnyObject;

void anyobj_insert(AnyObject *obj, char *key, AnyValue value);
AnyObject * anyobj_new();
