#include "dynstring/dynstring.h"
#include "list/list.h"
#include "reflection.h"
#include <assert.h>
#include <stdint.h>
#include <sys/types.h>

bool __hpi_internal_string_starts_with(DynString *base, DynString *test) {
  if (test->length > base->length) {
    return false;
  }

  for (ssize_t i = 0; i < test->length; i++) {
    if (base->internal_str[i] != test->internal_str[i]) {
      return false;
    }
  }

  return true;
}

bool __hpi_internal_string_contains(DynString *base, DynString *test) { return dynstring_contains(base, test); }

DynString *__hpi_internal_string_replace(DynString *base_str, DynString *replace_src, DynString *replace_with) { return base_str; }

ListNode *__hpi_internal_string_split(DynString *base, DynString *delim, void(tracer)(void *addr, TypeDescriptor type, TypeDescriptor *type_heap)) {
  ListNode *split = dynstring_split(base, delim, 0);
  ssize_t len = list_len(split);

  ListNode *res = list_new();

  TypeDescriptor *inner = malloc(sizeof(TypeDescriptor));
  *inner = (TypeDescriptor){.list_inner = NULL, .obj_fields = NULL, .ptr_count = 0, .kind = TYPE_STRING};
  tracer(res, (TypeDescriptor){.kind = TYPE_LIST, .ptr_count = 0, .obj_fields = NULL, .list_inner = inner}, inner);

  for (int i = 0; i < len; i++) {
    ListGetResult curr = list_at(split, i);
    assert(curr.found);

    tracer(curr.value, (TypeDescriptor){.kind = TYPE_STRING, .ptr_count = 0, .obj_fields = NULL, .list_inner = NULL}, NULL);

    DynString **temp_ptr = malloc(sizeof(DynString *));
    tracer(temp_ptr, (TypeDescriptor){.kind = TYPE_STRING, .ptr_count = 1, .obj_fields = NULL, .list_inner = NULL}, NULL);
    *temp_ptr = curr.value;

    list_append(res, temp_ptr);
  }

  list_free(split);

  return res;
}

DynString *__hpi_internal_string_repeat(DynString *base, int64_t repeat_n) {
  dynstring_repeat(base, repeat_n);
  return base;
}

int64_t __hpi_internal_string_length(DynString *base) { return base->length; }
