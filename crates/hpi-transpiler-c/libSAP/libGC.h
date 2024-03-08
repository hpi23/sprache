#pragma once
#include "assert.h"
#include "libAnyObj.h"
#include "list/list.h"
#include "reflection.h"
#include "vec/vec.h"
#include <sys/types.h>

typedef struct {
  TypeDescriptor type;
  void *address;
  char *origin;
} GCRoot;

typedef struct {
  // Requiref for calling the appropriate `free` function.
  TypeDescriptor type;
  void *address;
  bool marked;
  // If this type has an associated type which is on the heap, it should be freed alongside the type.
  TypeDescriptor *associated_type_heap;
} GCEntry;

typedef struct {
  // List of entries
  Vec *entries;
  // List of roots
  ListNode *roots;
  // If set to `true`, the GC performs a final run before program exit
  bool clean_up_on_exit;
  // Tracks the amount of bytes that the program currently uses,
  // is only updated if the memory is freed or allocated.
} GC;

void gc_init(bool clean_up_on_exit, bool verbose);
void gc_add_to_trace(void *address, TypeDescriptor type, TypeDescriptor * type_to_free);
void *gc_alloc(TypeDescriptor type);
void gc_add_root(void *address, TypeDescriptor type, char *origin);
void gc_remove_roots(int64_t argc, void **roots);
void gc_run_cycle();
void gc_print();
void gc_die();
void external_print_state();
