#include "../hpi-c-tests/list/list.h"
#include "libAnyObj.h"
#include "reflection.h"
#include "assert.h"
#include <sys/types.h>

typedef struct {
  // Requiref for calling the appropriate `free` function.
  TypeDescriptor type;
  void *address;
  // When a heap object is dropped, this value is decremented
  uint references;
} GCEntry;

typedef struct {
  ListNode *objects;
} GC;

void gc_init();
void gc_trace(void *address, TypeDescriptor type);
void gc_alloc(TypeDescriptor type);
void gc_drop(void *address);
void gc_run();
void gc_print();
