#include "./libGC.h"
#include "./libAnyObj.h"
#include "./libMem.h"
#include "./reflection.h"
#include "hashmap/map.h"
#include "list/list.h"
#include "vec/vec.h"
#include <assert.h>
#include <malloc.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>

GC *gc;

GC *gc_new(bool clean_up_on_exit, bool verbose) {
  GC *new = (GC *)malloc(sizeof(GC));
  new->entries = vec_new();
  new->roots = list_new();
  new->clean_up_on_exit = clean_up_on_exit;
  new->verbose = verbose;
  return new;
}

void gc_init(bool clean_up_on_exit, bool verbose) {
  gc = gc_new(clean_up_on_exit, verbose);
  assert(gc != NULL);

  if (gc->verbose)
    puts("gc_init(): Garbage collector initialized successfully");
}

void gc_add_root(void *address, TypeDescriptor type, char *origin) {
  GCRoot *entry = malloc(sizeof(GCRoot));

  entry->type = type;
  entry->address = address;
  entry->origin = origin;

  list_append(gc->roots, entry);
  gc_add_to_trace(address, type, NULL);
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
  if (gc->verbose) {
    char *obj_type_str = display_type(object->type);
    printf("gc_find_object() address %p -> object %p (%s)\n", address, object, obj_type_str);
    free(obj_type_str);
  }

  if (object == NULL) {
    gc_print();
    printf("gc_find_object(): could not find object %p\n", address);
    abort();
  }
  return object;
}

void gc_add_to_trace(void *address, TypeDescriptor type, TypeDescriptor *type_to_free) {
  // Check if this address is already tracked.
  GCEntry *object = gc_find_object_internal(gc, address);

  if (object != NULL) {
    if (gc->verbose)
      printf("gc_add_to_trace(): address %p already traced.\n", address);
    return;
  }

  GCEntry *entry = malloc(sizeof(GCEntry));

  entry->type = type;
  entry->address = address;
  entry->marked = false;
  entry->associated_type_heap = type_to_free;

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

void gc_free_addr(void *addr) {
  GCEntry *obj = gc_find_object(gc, addr);
  printf("OBJ: %p", obj);
  assert(obj != NULL);
  gc_free_entry(obj);
  gc_remove_roots(1, (void *[]){addr});
}

void gc_free_entry(GCEntry *obj) {
  // TODO: how to deal with pointers?

  if (gc->verbose) {
    char *type_str = display_type(obj->type);
    printf("gc_free_entry(): %p | %s\n", obj->address, type_str);
    free(type_str);
  }

  if (obj->type.ptr_count > 0) {
    free(obj->address);
    obj->address = NULL;

    if (gc->verbose)
      puts("gc_free_entry(): Only shallow free, not traversing");

    goto _free;
  }

  switch (obj->type.kind) {
  case TYPE_NONE:
  case TYPE_INT:
  case TYPE_FLOAT:
  case TYPE_CHAR:
  case TYPE_BOOL:
    goto fail;
  case TYPE_LIST:
    // TODO: double free when traversing list???
    list_free(obj->address);
    obj->address = NULL;
    break;
  case TYPE_OBJECT:
    hashmap_free(obj->address);
    obj->address = NULL;
    break;
  case TYPE_ANY_OBJECT:
    anyobj_free(obj->address);
    obj->address = NULL;
    break;
  case TYPE_STRING:
    dynstring_free(obj->address);
    obj->address = NULL;
    break;
  case TYPE_ANY_VALUE: {
    AnyValue *value = obj->address;
    free(value);
    obj->address = NULL;
    break;
  }
  default:
    goto fail;
  }

_free:
  if (obj->associated_type_heap != NULL) {
    if (gc->verbose) {
      char *type_disp = display_type(*obj->associated_type_heap);
      printf("gc_free_entry(): Free of type `%s` %p\n", type_disp, obj->associated_type_heap);
      free(type_disp);
    }
    free_type(obj->associated_type_heap);
  }

  free(obj);
  return;

fail:
  printf("gc_free_entry(): Invalid type: `%s` on object %p of address %p\n", display_type(obj->type), obj, obj->address);
  abort();
}

void *gc_alloc(TypeDescriptor type) {
  if (gc->verbose) {
    char *v_str = display_type(type);
    printf("gc_alloc(): Allocating value of type `%s`\n", v_str);
    free(v_str);
  }
  return alloc_maybe_trace(type, gc_add_to_trace);
}

void gc_traverse_value(void *root) {
  // GC_PRINT;
  // printf("Traversing value %p | %s ptr count: %ld...\n", root,
  //        display_type(type), type.ptr_count);

  GCEntry *obj = gc_find_object(gc, root);
  obj->marked = true;

  assert(obj->address == root);
  assert(obj->type.ptr_count == obj->type.ptr_count);

  if (gc->verbose)
    printf("gc_traverse_value(): marked address %p\n", obj->address);

  if (obj->type.ptr_count > 0) {
    TypeDescriptor new_type = {
        .ptr_count = obj->type.ptr_count - 1,
        .kind = obj->type.kind,
        .list_inner = obj->type.list_inner,
        .obj_fields = obj->type.obj_fields,
    };
    void *new_ptr = NULL;

    switch (obj->type.kind) {
    case TYPE_NONE:
      puts(display_type(obj->type));
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
    case TYPE_ANY_VALUE:
      new_ptr = (void *)*(AnyValue **)root;
      break;
    case TYPE_STRING:
      new_ptr = (void *)*(DynString **)root;
      break;
    }

    char *from_type_str = display_type(obj->type);
    char *new_type_str = display_type(new_type);
    if (gc->verbose)
      printf("gc_traverse_value(): deref pointer %s to %s.\n", from_type_str, new_type_str);
    free(from_type_str);
    free(new_type_str);

    if (obj->type.ptr_count > 1) {
      gc_traverse_value(root);
    }
    return;
  }

  switch (obj->type.kind) {
  case TYPE_INT:
  case TYPE_FLOAT:
  case TYPE_CHAR:
  case TYPE_BOOL:
    break;
  case TYPE_STRING: {
    DynString *string = *(DynString **)root;
    // BUG: do anything?
    break;
  }
  case TYPE_NONE:
    puts(display_type(obj->type));
    assert(0 && "gc_traverse_value(): Unsupported type");
    abort();
    break;
  case TYPE_LIST: {
    // TODO: what if list is a pointer?
    TypeDescriptor list_inner = *obj->type.list_inner;

    ListNode *list = (ListNode *)root;
    for (int i = 0; i < list_len(list); i++) {
      ListGetResult currRes = list_at(list, i);
      assert(currRes.found);

      // currRes.value
      //

      list_inner.ptr_count++;
      gc_traverse_value(currRes.value);
    }

    break;
  }
  case TYPE_OBJECT: {
    HashMap *obj_fields = obj->type.obj_fields;

    ListNode *keys = hashmap_keys(obj_fields);

    for (int i = 0; i < list_len(keys); i++) {
      ListGetResult currRes = list_at(keys, i);
      assert(currRes.found);

      char *key = (char *)currRes.value;

      MapGetResult valueRes = hashmap_get(root, key);
      assert(valueRes.found);

      MapGetResult typeRes = hashmap_get(obj_fields, key);
      assert(typeRes.found);

      // TypeDescriptor *field_type = (TypeDescriptor *)typeRes.value;

      gc_traverse_value(valueRes.value);
    }

    break;
  }
  case TYPE_ANY_OBJECT: {
    AnyObject anyobj = *(AnyObject *)root;
    printf("OF ROOT: %p | %s\n", root, display_type(obj->type));

    ListNode *keys = hashmap_keys(anyobj.fields);
    uint key_len = list_len(keys);

    for (int i = 0; i < key_len; i++) {
      ListGetResult key_res = list_at(keys, i);
      assert(key_res.found);

      // printf("FOUND KEY: %s\n", (char *)key_res.value);

      MapGetResult value_res = hashmap_get(anyobj.fields, key_res.value);
      assert(value_res.found);

      AnyValue value = *(AnyValue *)value_res.value;

      if (value.value != NULL) {
        gc_traverse_value(value.value /*, value.type */);
      }
    }

    list_free(keys);
    break;
  }
  case TYPE_ANY_VALUE: {
    AnyValue value = *(AnyValue *)root;
    gc_traverse_value(value.value /* , value.type */);
    break;
  }
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
    gc_traverse_value(curr->address /* , curr->type */);
  }
}

void gc_sweep_rec() {
  GCEntry *remove_entry = NULL;
  int remove_index = -1;

  for (int i = 0; i < gc->entries->used; i++) {
    GCEntry *entry = (GCEntry *)vec_index(gc->entries, i);

    if (!entry->marked) {
      remove_entry = entry;
      remove_index = i;
      break;
    }
  }

  if (remove_entry == NULL) {
    return;
  }

  char *type_str;
  if (gc->verbose)
    type_str = display_type(remove_entry->type);

  int before = gc->entries->used;
  void *removed_addr = remove_entry->address;

  gc_free_entry(remove_entry);
  vec_remove(gc->entries, remove_index);

  int nremoved = before - gc->entries->used;
  assert(nremoved > 0);

  if (gc->verbose) {
    printf("gc_sweep_rec(): delete `%s` (index %d); removed %d object(s) %p.\n", type_str, remove_index, nremoved, removed_addr);
    free(type_str);
  }

  gc_sweep_rec();
}

// Sweep phase of the garbage collection cycle.
void gc_sweep() {
  gc_sweep_rec();
  malloc_trim(0);
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
  gc_print();

  gc_mark();

  gc_print();

  gc_sweep();

  gc_print();
}

void _gc_print(GC *self) {
  uint objs_len = self->entries->used;
  puts("--------------------------------------------------------");
  printf("# Entries tracked: %d\n", objs_len);

  for (int i = 0; i < objs_len; i++) {
    GCEntry *curr = (GCEntry *)vec_index(self->entries, i);

    char *type_str = display_type(curr->type);

    printf("    - %p | marked: %d | type-heap: %p | type: %s\n", curr->address, curr->marked, curr->associated_type_heap, type_str);

    free(type_str);
  }

  uint root_len = list_len(gc->roots);
  printf("# Roots: %d\n", root_len);

  for (int i = 0; i < root_len; i++) {
    ListGetResult currRes = list_at(self->roots, i);
    assert(currRes.found);

    GCRoot *curr = (GCRoot *)currRes.value;

    char *type_str = display_type(curr->type);

    printf("    - %p | type: %s | origin: `%s`\n", curr->address, type_str, curr->origin);

    free(type_str);
  }
}

void gc_print() {
  printf("GC_VERBOSE: %d\n", gc->verbose);
  if (gc->verbose)
    _gc_print(gc);
}

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
    }
  }

  if (gc->verbose)
    printf("gc_remove_root(): Invalid root address: %p\n", address);
}

void gc_remove_roots(int64_t num_roots, void **roots) {
  for (int i = num_roots - 1; i >= 0; i--) {
    uint64_t before = list_len(gc->roots);
    gc_remove_root(roots[i]);
    if (gc->verbose) {
      printf("gc_remove_roots(): removed root: %ld -> %ld roots\n", before, list_len(gc->roots));
    }
  }
}

void gc_release_all() {
  while (list_len(gc->roots) > 0) {
    ListGetResult currRes = list_at(gc->roots, 0);
    assert(currRes.found);

    GCRoot *root = (GCRoot *)currRes.value;
    gc_remove_root(root->address);
    break;
  }

  // NOTE: if there is still garbage, it is not freed normally, so trigger a GC
  // run.
  gc_internal_unmark_all();
  gc_mark();

  if (gc->verbose)
    gc_print();

  gc_sweep();
}

void gc_free_self() {
  vec_free(gc->entries);
  list_free(gc->roots);
  free(gc);
}

void gc_die() {
  gc_print();
  gc_release_all();
  gc_free_self();
}

void external_print_state() {
  gc_mark();
  gc_print();
}
