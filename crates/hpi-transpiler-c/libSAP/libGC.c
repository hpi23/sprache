#include "./libGC.h"
#include "libAnyObj.h"
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#define GC_VERBOSE true
#define GC_PRINT                                                               \
  if (GC_VERBOSE)                                                              \
  gc_print()

GC *gc;

GC *gc_new() {
  GC *new = (GC *)malloc(sizeof(GC));
  new->objects = list_new();
  return new;
}

void gc_init() {
  gc = gc_new();
  assert(gc != NULL);
}

void _gc_trace(GC *self, void *address, TypeDescriptor type) {
  GCEntry *entry = malloc(sizeof(GCEntry));

  entry->type = type;
  entry->address = address;
  entry->references = 1;

  list_append(self->objects, entry);
}

void gc_trace(void *address, TypeDescriptor type) {
  _gc_trace(gc, address, type);
  GC_PRINT;
}

void *_gc_alloc(GC *self, TypeDescriptor type) {
  void *allocated = NULL;

  switch (type.kind) {
  case TYPE_NONE:
    assert(0 && "This type is not supported");
  case TYPE_INT:
    assert(0 && "This type is not supported");
  case TYPE_FLOAT:
    assert(0 && "This type is not supported");
  case TYPE_CHAR:
    assert(0 && "This type is not supported");
  case TYPE_BOOL:
    assert(0 && "This type is not supported");
  case TYPE_LIST:
    allocated = list_new();
  case TYPE_OBJECT:
    allocated = hashmap_new();
  case TYPE_ANY_OBJECT:
    allocated = anyobj_new();
  case TYPE_STRING:
    allocated = dynstring_new();
  default:
    assert(0 && "This type is not supported");
  }

  _gc_trace(self, allocated, type);

  return allocated;
}

void gc_alloc(TypeDescriptor type) { _gc_alloc(gc, type); }

GCEntry *gc_find_object(GC *self, void *address) {
  for (int i = 0; i < list_len(self->objects); i++) {
    ListGetResult currRes = list_at(self->objects, i);
    assert(currRes.found);

    GCEntry *curr = (GCEntry *)currRes.value;
    if (curr->address == address) {
      return curr;
    }
  }

  assert(0 && "gc_find_object() could not find object");
}

void _gc_ref(GC *self, void *address) {
  GCEntry *obj = gc_find_object(self, address);
  obj->references++;
}

void gc_ref(GC *self, void *address) {
  _gc_ref(self, address);
  GC_PRINT;
}

void _gc_drop(GC *self, void *address) {
  GCEntry *obj = gc_find_object(self, address);

  if (obj->references == 0) {
    return;
  }

  obj->references--;
}

void gc_drop(void *address) {
  _gc_drop(gc, address);
  GC_PRINT;
}

void _gc_free(GCEntry *obj) {
  switch (obj->type.kind) {
  case TYPE_NONE:
    assert(0 && "Cannot free this type");
  case TYPE_INT:
    assert(0 && "Cannot free this type");
  case TYPE_FLOAT:
    assert(0 && "Cannot free this type");
  case TYPE_CHAR:
    assert(0 && "Cannot free this type");
  case TYPE_BOOL:
    assert(0 && "Cannot free this type");
  case TYPE_LIST:
    list_free(obj->address);
  case TYPE_OBJECT:
    hashmap_free(obj->address);
  case TYPE_ANY_OBJECT:
    anyobj_free(obj->address);
  case TYPE_STRING:
    dynstring_free(obj->address);
  default:
    assert(0 && "Cannot free this type");
  }
}

void gc_free(GCEntry *obj) {
  gc_free(obj);
  GC_PRINT;
}

void _gc_run(GC *self) {
  for (int i = 0; i < list_len(self->objects); i++) {
    ListGetResult currRes = list_at(self->objects, i);
    assert(currRes.found);

    GCEntry *curr = (GCEntry *)currRes.value;
    if (curr->references == 0) {
      gc_free(curr);
      list_delete_index(self->objects, i);
    }
  }
}

void gc_run() {
  _gc_run(gc);
  GC_PRINT;
}

void _gc_print(GC *self) {
  uint objs_len = list_len(self->objects);
  puts("--------------------------------------------------------");
  printf("objects tracked: %d\n", objs_len);

  for (int i = 0; i < objs_len; i++) {
    ListGetResult currRes = list_at(self->objects, i);
    assert(currRes.found);

    GCEntry *curr = (GCEntry *)currRes.value;

    printf("- %p | type: %s | references: %d\n", curr->address,
           display_type(curr->type), curr->references);
  }
}

void gc_print() { _gc_print(gc); }
