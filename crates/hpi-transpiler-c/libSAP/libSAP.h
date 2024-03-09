#pragma once
#include "dynstring/dynstring.h"
#include "hashmap/map.h"
#include "libAnyObj.h"
#include <stdio.h>

typedef struct {
  uint major;
  uint minor;
  uint patch;
} Semver;

void __hpi_internal_libSAP_reset();
void __hpi_internal_print(ssize_t num_args, ...);
int64_t __hpi_internal_generate_matrikelnummer();
DynString *__hpi_internal_fmt(ssize_t num_args, DynString *fmt, void(tracer)(void *addr, TypeDescriptor type, TypeDescriptor *type_heap), ...);
void __hpi_internal_sleep(double duration);
AnyObject *__hpi_internal_env();
ListNode *__hpi_internal_args(void *(allocator)(TypeDescriptor type), void(tracer)(void *addr, TypeDescriptor type, TypeDescriptor *type_heap));
void __hpi_internal_init_libSAP(size_t p_argc, char **p_argv, bool init_curl, bool init_gc);
DynString *__hpi_internal_get_version();
