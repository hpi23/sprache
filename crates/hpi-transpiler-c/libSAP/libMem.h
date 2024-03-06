#include "reflection.h"

void *alloc_maybe_trace(TypeDescriptor type,
                        void(trace_func)(void *addr, TypeDescriptor type, TypeDescriptor * associated_type));
