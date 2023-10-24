#include "/home/mik/Coding/hpi/hpi-c-tests/dynstring/dynstring.h"
#include "/home/mik/Coding/hpi/hpi-c-tests/list/list.h"
#include <assert.h>
#include <stdio.h>
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

bool __hpi_internal_string_contains(DynString *base, DynString *test) {
  return dynstring_contains(base, test);
}

DynString *__hpi_internal_string_replace(DynString *base_str,
                                         DynString *replace_src,
                                         DynString *replace_with) {

  return base_str;
}

ListNode *__hpi_internal_string_split(DynString *base, DynString *delim) {
  ListNode *split = dynstring_split(base, delim, 0);
  ssize_t len = list_len(split);

  ListNode *res = list_new();

  for (int i = 0; i < len; i++) {
    ListGetResult curr = list_at(split, i);
    assert(curr.found);

    DynString **temp_ptr = malloc(sizeof(DynString *));
    *temp_ptr = curr.value;

    list_append(res, temp_ptr);
  }

  list_free(split);

  return res;
}
