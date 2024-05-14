#include "./list.h"
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

struct ListNode *list_new() {
  struct ListNode *node = (struct ListNode *)malloc(sizeof(struct ListNode));
  node->is_initialized = false;
  node->next = NULL;
  node->value = 0;
  return node;
}

void list_append(struct ListNode *list, void *value) {
  assert(list != NULL);

  while (list->next != NULL) {
    list = list->next;
  }

  if (list->is_initialized) {
    list->next = list_new();
    list->next->value = value;
    list->next->is_initialized = true;
  } else {
    list->value = value;
    list->is_initialized = true;
  }
}

void list_print(struct ListNode *list) {
  printf("[");
  while (list != NULL && list->is_initialized) {
    printf("%p", list->value);
    list = list->next;
    if (list != NULL) {
      printf(", ");
    }
  }
  printf("]\n");
}

ssize_t list_len(struct ListNode *list) {
  ssize_t cnt = 0;
  while (list != NULL && list->is_initialized) {
    cnt++;
    assert(list != list->next); // cycle detected
    list = list->next;
  }
  return cnt;
}

// if this returns false, the index does not exist
ListGetResult list_at(struct ListNode *list, uint64_t index) {
  assert(list != NULL);

  ListGetResult result = {.found = false, .value = NULL};

  for (int count = 0; (list != NULL && list->is_initialized) && count <= index;
       count++) {
    if (index == count) {
      result.value = list->value;
      result.found = true;
      return result;
    }

    list = list->next;
  }

  return result;
}

void list_free(struct ListNode *list) {
  assert(list != NULL);
  while (list->next != NULL) {
    struct ListNode *next = list->next;
    free(list);
    list = next;
  }
  free(list);
}

void list_delete_index(struct ListNode *list, ssize_t index) {
  assert(list != NULL);

  ListNode *prev = NULL;

  for (int count = 0; (list != NULL && list->is_initialized) && count <= index; count++) {
    if (index == count) {
      if (index == 0) {
        if (list->next != NULL) {
          *list = *list->next;
        } else {
          list->is_initialized = false;
        }
      } else {
        prev->next = list->next;
        free(list);
      }
      return;
    }

    prev = list;
    list = list->next;
  }

  puts("list_delete_index(): Invalid index");
  abort();
}
