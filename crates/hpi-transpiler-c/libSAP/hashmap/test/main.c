#include "../map.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

int main() {
  HashMap * map = hashmap_new();
  hashmap_insert(map, "foo", "Bar");
  hashmap_insert(map, "foo", "Bar2");
  hashmap_insert(map, "foo2", "Hello World");

  MapGetResult result = hashmap_get(map, "foo2");
  printf("found: %d | value: %s\n", result.found, (char *)result.value);

  // for (int i = 0; i < 1000; i++) {
  //   char *str = (char *)malloc(1000);
  //   sprintf(str, "%d", i);
  //   hashmap_insert(&map, str, "My-Value");
  //   free(str);
  // }

  // hashmap_print_buckets(&map);

  ListNode * keys = hashmap_keys(map);
  int len = list_len(keys);
  for (int i = 0; i < len; i++) {
      ListGetResult key = list_at(keys, i);
      assert(key.found);
      printf("%s\n", (char *) key.value);
  }

  // hashmap_free(&map);
}
