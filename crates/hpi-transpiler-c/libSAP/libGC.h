#pragma once
#include "../hpi-c-tests/list/list.h"
#include "assert.h"
#include "libAnyObj.h"
#include "reflection.h"
#include <sys/types.h>

typedef struct {
  TypeDescriptor type;
  void *address;
} GCRoot;

typedef struct {
  // Requiref for calling the appropriate `free` function.
  TypeDescriptor type;
  void *address;
  bool marked;
} GCEntry;

typedef struct {
  // List of entries
  ListNode *entries;
  // List of roots
  ListNode *roots;
} GC;

void gc_init();
void gc_add_to_trace(void *address, TypeDescriptor type);
void * gc_alloc(TypeDescriptor type);
void gc_drop(void *address);
void gc_run_cycle();
void gc_print();
