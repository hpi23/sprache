#pragma once
#include "../hpi-c-tests/list/list.h"
#include "../hpi-c-tests/vec/vec.h"
#include "assert.h"
#include "libAnyObj.h"
#include "reflection.h"
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
} GCEntry;

typedef struct {
  // List of entries
  Vec *entries;
  // List of roots
  ListNode *roots;
  // If set to `true`, the GC performs a final run before program exit
  bool clean_up_on_exit;
} GC;

void gc_init(bool clean_up_on_exit);
void gc_add_to_trace(void *address, TypeDescriptor type);
void *gc_alloc(TypeDescriptor type);
void gc_add_root(void *address, TypeDescriptor type, char *origin);
void gc_remove_roots(int64_t argc, void **roots);
void gc_run_cycle();
void gc_print();
void gc_die();
void external_print_state();
