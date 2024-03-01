#include "./libGC.h"
#include "libAnyObj.h"
#include "reflection.h"
#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>

#define GC_VERBOSE true
#define GC_PRINT                                                               \
  if (GC_VERBOSE)                                                              \
  gc_print()

GC *gc;

GC *gc_new() {
  GC *new = (GC *)malloc(sizeof(GC));
  new->entries = vec_new();
  new->roots = list_new();
  return new;
}

void gc_init() {
  gc = gc_new();
  assert(gc != NULL);
}

void gc_add_root(void *address, TypeDescriptor type, char *origin) {
  GCRoot *entry = malloc(sizeof(GCRoot));

  entry->type = type;
  entry->address = address;
  entry->origin = origin;

  list_append(gc->roots, entry);
  gc_add_to_trace(address, type);
}

// Returns `NULL` if the object does not exist.
// Otherwise, the object is returned.
GCEntry *gc_find_object_internal(GC *self, void *address) {
  for (int i = 0; i < self->entries->used; i++) {
    GCEntry *curr = (GCEntry *)vec_index(self->entries, i);
    if (curr->address == address) {
      return curr;
    }
  }

  return NULL;
}

// Unlike `gc_find_object_internal`, this call panics if the object is not
// found.
GCEntry *gc_find_object(GC *self, void *address) {
  GCEntry *object = gc_find_object_internal(self, address);
  printf("find %p: %p\n", address, object);
  assert(object != NULL && "gc_find_object() could not find object");
  return object;
}

void gc_add_to_trace(void *address, TypeDescriptor type) {
  // Check if this address is already tracked.
  GCEntry *object = gc_find_object_internal(gc, address);

  if (object != NULL) {
    printf("gc_add_to_trace(): address %p already traced.\n", address);
    return;
  }

  GCEntry *entry = malloc(sizeof(GCEntry));

  entry->type = type;
  entry->address = address;
  entry->marked = false;

  vec_push(gc->entries, entry);
  // GC_PRINT;
}

// void gc_remove_entry(void *address) {
//   uint len = list_len(gc->entries);
//   for (int i = len - 1; i >= 0; i--) {
//     ListGetResult currRes = list_at(gc->entries, i);
//     assert(currRes.found);
//
//     GCEntry *curr = (GCEntry *)currRes.value;
//     if (curr->address == address) {
//       list_delete_index(gc->entries, i);
//       return;
//     }
//   }
//
//   // printf("Could not remove address: %p\n", address);
//   // assert(0 && "gc_remove_entry(): Illegal address not found.");
// }

void gc_free(GCEntry *obj) {
  // TODO: how to deal with pointers?

  char *type = display_type(obj->type);
  printf("free(): %p | %s\n", obj->address, type);
  free(type);

  if (obj->type.ptr_count > 0) {
    // void *old = obj->address;

    // switch (obj->type.kind) {
    // case TYPE_NONE:
    //   assert(0 && "Cannot free this type");
    // case TYPE_INT:
    //   obj->address = *(int64_t **)obj->address;
    //   break;
    // case TYPE_FLOAT:
    //   obj->address = *(double **)obj->address;
    //   break;
    // case TYPE_CHAR:
    //   obj->address = *(char **)obj->address;
    //   break;
    // case TYPE_BOOL:
    //   obj->address = *(bool **)obj->address;
    //   break;
    // case TYPE_LIST:
    //   obj->address = *(ListNode **)obj->address;
    //   break;
    // case TYPE_OBJECT:
    //   obj->address = *(HashMap **)obj->address;
    //   break;
    // case TYPE_ANY_OBJECT:
    //   obj->address = *(AnyObject **)obj->address;
    //   break;
    // case TYPE_STRING:
    //   obj->address = *(DynString **)obj->address;
    //   break;
    // }
    //
    // obj->type.ptr_count--;
    free(obj->address);
    free(obj);
    puts("Only shallow free");
    return;
  }

  switch (obj->type.kind) {
  case TYPE_NONE:
    assert(0 && "Cannot free this type");
  case TYPE_INT:
  case TYPE_FLOAT:
  case TYPE_CHAR:
  case TYPE_BOOL:
    // free(obj->address);
    assert(0 && "Cannot free this type");
    break;
  case TYPE_LIST:
    // TODO: double free when traversing list???
    list_free(obj->address);
    break;
  case TYPE_OBJECT:
    hashmap_free(obj->address);
    break;
  case TYPE_ANY_OBJECT:
    anyobj_free(obj->address);
    break;
  case TYPE_STRING:
    dynstring_free(obj->address);
    break;
  default:
    printf("Invalid type: %s\n", display_type(obj->type));
    assert(0 && "gc_free(): Cannot free this type");
  }

  free(obj);
  // GC_PRINT;
}

void *gc_alloc(TypeDescriptor type) {
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
      allocated = malloc(sizeof(void *));
      goto finish;
      break;
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

finish:
  gc_add_to_trace(allocated, type);
  // GC_PRINT;
  return allocated;
}

void gc_traverse_value(void *root, TypeDescriptor type) {
  // GC_PRINT;
  // printf("Traversing value %p | %s ptr count: %ld...\n", root,
  //        display_type(type), type.ptr_count);

  GCEntry *obj = gc_find_object(gc, root);
  obj->marked = true;

  printf("gc_traverse_value(): marked address %p\n", obj->address);

  if (type.ptr_count > 0) {
    TypeDescriptor new_type = {.ptr_count = type.ptr_count - 1,
                               .kind = type.kind,
                               .list_inner = type.list_inner,
                               .obj_fields = type.obj_fields};
    void *new_ptr = NULL;

    switch (type.kind) {
    case TYPE_NONE:
      puts(display_type(type));
      assert(0 && "gc_traverse_value(): Unsupported type");
    case TYPE_INT:
      new_ptr = (void *)*(int64_t **)root;
      break;
    case TYPE_FLOAT:
      new_ptr = (void *)*(double **)root;
      break;
    case TYPE_CHAR:
      new_ptr = (void *)*(char **)root;
      break;
    case TYPE_BOOL:
      new_ptr = (void *)*(bool **)root;
      break;
    case TYPE_LIST:
      new_ptr = (void *)*(ListNode **)root;
      break;
    case TYPE_OBJECT:
      new_ptr = (void *)*(HashMap **)root;
      break;
    case TYPE_ANY_OBJECT:
      new_ptr = (void *)*(AnyObject **)root;
      break;
    case TYPE_STRING:
      new_ptr = (void *)*(DynString **)root;
      break;
    }

    char *from_type_str = display_type(type);
    char *new_type_str = display_type(new_type);
    printf("gc_traverse_value(): deref pointer %s to %s.\n", from_type_str,
           new_type_str);
    free(from_type_str);
    free(new_type_str);

    if (type.ptr_count > 1)
      gc_traverse_value(root, new_type);
    return;
  }

  switch (type.kind) {
  case TYPE_INT:
  case TYPE_FLOAT:
  case TYPE_CHAR:
  case TYPE_BOOL:
    break;
  case TYPE_STRING: {
    DynString *string = *(DynString **)root;

    break;
  }
  case TYPE_NONE:
    puts(display_type(type));
    assert(0 && "gc_traverse_value(): Unsupported type");
  case TYPE_LIST: {
    // TODO: what if list is a pointer?
    TypeDescriptor list_inner = *type.list_inner;

    ListNode *list = (ListNode *)root;
    for (int i = 0; i < list_len(list); i++) {
      ListGetResult currRes = list_at(list, i);
      assert(currRes.found);

      // currRes.value
      //

      list_inner.ptr_count++;
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
    abort();
    break;
  }
}

// NOTE: in most cases, should only be called before `gc_mark()`
void gc_internal_unmark_all() {
  for (int i = 0; i < gc->entries->used; i++) {
    GCEntry *curr = (GCEntry *)vec_index(gc->entries, i);
    curr->marked = false;
  }
}

// Mark phase of the garbage collection cycle.
void gc_mark() {
  // Unmark every entry.
  gc_internal_unmark_all();

  // Traverse nodes, starting at the roots.
  for (int i = 0; i < list_len(gc->roots); i++) {
    ListGetResult currRes = list_at(gc->roots, i);
    assert(currRes.found);
    GCRoot *curr = (GCRoot *)currRes.value;
    gc_traverse_value(curr->address, curr->type);
  }
}

// Sweep phase of the garbage collection cycle.
void gc_sweep() {
  for (int i = 0; i < gc->entries->used; i++) {
    GCEntry *entry = (GCEntry *)vec_index(gc->entries, i);

    if (entry->marked) {
      continue;
    }

    int before = gc->entries->used;
    // list_print(gc->entries);

    vec_remove(gc->entries, i);

    // list_print(gc->entries);

    // free(entry->address);
    gc_free(entry);

    printf("gc_sweep(): delete index %d; removed %d object(s).\n", i,
           before - gc->entries->used);
  }
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
  // GC_PRINT;

  gc_mark();

  GC_PRINT;

  gc_sweep();

  GC_PRINT;
}

void _gc_print(GC *self) {
  uint objs_len = self->entries->used;
  puts("--------------------------------------------------------");
  printf("# Entries tracked: %d\n", objs_len);

  for (int i = 0; i < objs_len; i++) {
    GCEntry *curr = (GCEntry *)vec_index(self->entries, i);

    char *type_str = display_type(curr->type);

    printf("    - %p | type: %s | marked: %d\n", curr->address, type_str,
           curr->marked);

    free(type_str);
  }

  uint root_len = list_len(gc->roots);
  printf("# Roots: %d\n", root_len);

  for (int i = 0; i < root_len; i++) {
    ListGetResult currRes = list_at(self->roots, i);
    assert(currRes.found);

    GCRoot *curr = (GCRoot *)currRes.value;

    char *type_str = display_type(curr->type);

    printf("    - %p | type: %s | origin: `%s`\n", curr->address, type_str,
           curr->origin);

    free(type_str);
  }
}

void gc_print() { _gc_print(gc); }

void gc_remove_root(void *address) {
  int rootc = list_len(gc->roots);

  for (int i = rootc - 1; i >= 0; i--) {
    ListGetResult currRes = list_at(gc->roots, i);
    // printf("curr: %d\n", i);
    assert(currRes.found);

    GCRoot *root = (GCRoot *)currRes.value;

    if (root->address == address) {
      // puts("root found!");
      list_delete_index(gc->roots, i);
      free(root);
      return;
    } else {
      printf("remove root: %p !== %p\n", root->address, address);
    }
  }

  printf("Invalid root address: %p\n", address);
  // assert(0 && "gc_remove_root(): Could not find root address to remove.");
}

void gc_remove_roots(int64_t num_roots, void **roots) {
  for (int i = num_roots - 1; i >= 0; i--) {
    uint64_t before = list_len(gc->roots);
    gc_remove_root(roots[i]);
    if (GC_VERBOSE) {
      printf("gc_remove_roots(): removed root: %ld -> %ld roots\n", before,
             list_len(gc->roots));
    }
  }
}

void gc_release_all() {
  uint root_len = list_len(gc->roots);
  for (int i = root_len - 1; i >= 0; i--) {
    ListGetResult currRes = list_at(gc->roots, i);
    assert(currRes.found);

    GCRoot *root = (GCRoot *)currRes.value;
    gc_remove_root(root);
  }

  // NOTE: if there is still garbage, it is not freed normally, so trigger a GC
  // run.
  gc_internal_unmark_all();
  gc_sweep();
}

void gc_free_self() {
  vec_free(gc->entries);
  list_free(gc->roots);
  free(gc);
}

void gc_die() {
  //gc_release_all();
  gc_free_self();
}

void external_print_state() {
  gc_mark();
  gc_print();
}
