#include "libSAP.h"
#include "dynstring/dynstring.h"
#include "list/list.h"
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

bool __hpi_internal_value_compare(TypeDescriptor type, void *left,
                                  void *right) {

  // TODO: are pointers being handled correctly
  if (type.ptr_count > 0) {
    return left == right;
  }

  switch (type.kind) {
  case TYPE_INT: {
    int64_t l_int = *(int64_t *)left;
    int64_t r_int = *(int64_t *)right;
    // printf("Comparing %ld with %ld = %d\n", l_int, r_int, l_int == r_int);
    return l_int == r_int;
  }
  case TYPE_FLOAT: {
    int64_t l_float = *(double *)left;
    int64_t r_float = *(double *)right;
    return l_float == r_float;
  }
  case TYPE_CHAR: {
    char l_char = *(char *)left;
    char r_char = *(char *)right;
    return l_char == r_char;
  }
  case TYPE_BOOL: {
    bool l_bool = *(bool *)left;
    bool r_bool = *(bool *)right;
    return l_bool == r_bool;
  }
  case TYPE_LIST: {
    TypeDescriptor new_type = {.kind = type.list_inner->kind,
                               .list_inner = type.list_inner->list_inner,
                               .ptr_count = type.list_inner->ptr_count,
                               .obj_fields = type.list_inner->obj_fields};

    ListNode *l_list = *(ListNode **)left;
    ListNode *r_list = *(ListNode **)right;

    int64_t l_list_len = list_len(l_list);
    int64_t r_list_len = list_len(r_list);

    if (l_list_len != r_list_len) {
      return false;
    }

    // FIXME: this is unperformant code: python will be faster :(

    for (int64_t i = 0; i < l_list_len; i++) {
      ListGetResult l_value = list_at(l_list, i);
      assert(l_value.found);

      ListGetResult r_value = list_at(r_list, i);
      assert(r_value.found);

      if (!__hpi_internal_value_compare(new_type, l_value.value,
                                        r_value.value)) {
        return false;
      }
    }
    return true;
  }
  case TYPE_STRING: {
    DynString *l_string = *(DynString **)left;
    DynString *r_string = *(DynString **)right;
    return dynstring_strcmp(l_string, r_string);
  }
  case TYPE_OBJECT:
    printf("Unimplemented type kind: `%d`\n", type.kind);
    exit(-1);
  }

  printf("Unimplemented type kind: `%d`\n", type.kind);
  exit(-1);
}

// List utility functions
int64_t __hpi_internal_list_len(ListNode *list) { return list_len(list); }

void *__hpi_internal_list_index(ListNode *list, int64_t index) {
  ListGetResult result = list_at(list, index);
  if (!result.found) {
    printf("__hpi_internal_list_index(): Buffer overdrive: Index out of bounds: cannot index list of length "
           "%ld using index "
           "%ld\n",
           list_len(list), index);
    abort();
  }

  return result.value;
}

bool __hpi_internal_list_contains(ListNode *list, TypeDescriptor type,
                                  void *to_check) {
  ListNode *list_iter = list;
  while (list_iter != NULL) {
    if (__hpi_internal_value_compare(type, list_iter->value, to_check)) {
      return true;
    }

    list_iter = list_iter->next;
  }

  return false;
}

void __hpi_internal_list_push(ListNode *list, void *to_add) {
  list_append(list, to_add);
}
