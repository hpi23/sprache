#include "./libGC.h"
#include "libAnyObj.h"
#include "reflection.h"
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
  new->entries = list_new();
  return new;
}

void gc_init() {
  gc = gc_new();
  assert(gc != NULL);
}

void gc_add_root(void *address, TypeDescriptor type) {
  GCRoot *entry = malloc(sizeof(GCRoot));

  entry->type = type;
  entry->address = address;

  list_append(gc->roots, entry);
}

void gc_add_to_trace(void *address, TypeDescriptor type) {
  GCEntry *entry = malloc(sizeof(GCEntry));

  entry->type = type;
  entry->address = address;
  entry->marked = false;

  list_append(gc->entries, entry);
  // GC_PRINT;
}

void *gc_alloc(TypeDescriptor type) {
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
    break;
  case TYPE_OBJECT:
    allocated = hashmap_new();
    break;
  case TYPE_ANY_OBJECT:
    allocated = anyobj_new();
    break;
  case TYPE_STRING:
    allocated = dynstring_new();
    break;
  default: {
    char *typ = display_type(type);
    printf("gc_alloc(): illegal type: `%s`\n", typ);
    free(typ);
    assert(0 && "GC crashed");
  }
  }

  gc_add_to_trace(allocated, type);
  // GC_PRINT;
  return allocated;
}

GCEntry *gc_find_object(GC *self, void *address) {
  for (int i = 0; i < list_len(self->entries); i++) {
    ListGetResult currRes = list_at(self->entries, i);
    assert(currRes.found);

    GCEntry *curr = (GCEntry *)currRes.value;
    if (curr->address == address) {
      return curr;
    }
  }

  assert(0 && "gc_find_object() could not find object");
}

void gc_traverse_value(void *root, TypeDescriptor type) {
 GCEntry * obj = gc_find_object(gc, root);
 obj->marked = true;

 printf("Marked object with address %p\n", obj->address);

  switch (type.kind) {
  case TYPE_INT:
  case TYPE_FLOAT:
  case TYPE_CHAR:
  case TYPE_BOOL:
  case TYPE_NONE:
    assert(0 && "gc_traverse_root(): Unsupported type");
  case TYPE_LIST: {
    // TODO: what if list is a pointer?

    TypeDescriptor list_inner = *type.list_inner;

    ListNode *list = (ListNode *)root;
    for (int i = 0; i < list_len(list); i++) {
      ListGetResult currRes = list_at(list, i);
      assert(currRes.found);

      // currRes.value

      gc_traverse_value(currRes.value, list_inner);
    }

    break;
  }
  case TYPE_OBJECT: {
    HashMap *obj_fields = type.obj_fields;

    ListNode *keys = hashmap_keys(obj_fields);

    for (int i = 0; i < list_len(keys); i++) {
      ListGetResult currRes = list_at(keys, i);
      assert(currRes.found);

      char *key = (char *)currRes.value;

      MapGetResult valueRes = hashmap_get(root, key);
      assert(valueRes.found);

      MapGetResult typeRes = hashmap_get(obj_fields, key);
      assert(typeRes.found);

      TypeDescriptor *field_type = (TypeDescriptor *)typeRes.value;

      gc_traverse_value(valueRes.value, *field_type);
    }

    break;
  }
  case TYPE_ANY_OBJECT:
  case TYPE_STRING:
    break;
  }
}

// Mark phase of the garbage collection cycle.
void gc_mark() {
  // Unmark every entry.
  for (int i = 0; i < list_len(gc->entries); i++) {
    ListGetResult currRes = list_at(gc->entries, i);
    assert(currRes.found);
    GCEntry *curr = (GCEntry *)currRes.value;
    curr->marked = false;
  }

  // Traverse nodes, starting at the roots.
  for (int i = 0; i < list_len(gc->roots); i++) {
    ListGetResult currRes = list_at(gc->entries, i);
    assert(currRes.found);
    GCRoot *curr = (GCRoot *)currRes.value;
    gc_traverse_value(curr->address, curr->type);
  }
}

// Sweep phase of the garbage collection cycle.
void gc_sweep() {
  // TODO
}

// void _gc_ref(GC *self, void *address) {
//   GCEntry *obj = gc_find_object(self, address);
//   obj->references++;
// }
//
// void gc_ref(GC *self, void *address) {
//   _gc_ref(self, address);
//   GC_PRINT;
// }

// void _gc_drop(GC *self, void *address) {
//   GCEntry *obj = gc_find_object(self, address);
//
//   if (obj->references == 0) {
//     return;
//   }
//
//   obj->references--;
// }

// void gc_drop(void *address) {
//   _gc_drop(gc, address);
//   GC_PRINT;
// }

void gc_free(GCEntry *obj) {
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

  // GC_PRINT;
}

void _gc_mark(GC *self) {}

// void _gc_run(GC *self) {
// for (int i = 0; i < list_len(self->objects); i++) {
//   ListGetResult currRes = list_at(self->objects, i);
//   assert(currRes.found);
//
//   GCEntry *curr = (GCEntry *)currRes.value;
//   if (curr->references == 0) {
//     gc_free(curr);
//     list_delete_index(self->objects, i);
//   }
// }
// }

void gc_run_cycle() {
  puts("Would run a cycle now...");

  GC_PRINT;

  gc_mark();

  GC_PRINT;

  gc_sweep();

  GC_PRINT;
}

void _gc_print(GC *self) {
  uint objs_len = list_len(self->entries);
  puts("--------------------------------------------------------");
  printf("objects tracked: %d\n", objs_len);

  for (int i = 0; i < objs_len; i++) {
    ListGetResult currRes = list_at(self->entries, i);
    assert(currRes.found);

    GCEntry *curr = (GCEntry *)currRes.value;

    printf("- %p | type: %s | marked: %d\n", curr->address,
           display_type(curr->type), curr->marked);
  }
}

void gc_print() { _gc_print(gc); }
