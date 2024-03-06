#pragma once
#include "dynstring/dynstring.h"
#include "list/list.h"
#include "libSAP.h"
#include <stdbool.h>

// List utility functions
int64_t __hpi_internal_list_len(ListNode * list);
void * __hpi_internal_list_index(ListNode * list, int64_t index);
bool __hpi_internal_list_contains(ListNode * list, TypeDescriptor type, void * to_check);
void __hpi_internal_list_push(ListNode * list, void * to_add);
