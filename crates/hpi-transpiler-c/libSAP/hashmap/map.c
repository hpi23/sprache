#include "./map.h"
#include "../list/list.h"
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

// This is a VERY basic hashing function
u_int16_t map_internal_hash(char *key_ptr) {
  ssize_t value_byte_len = strlen(key_ptr);
  unsigned short *tmp = (unsigned short *)key_ptr;
  unsigned int hash = tmp[0];

  for (int i = 1; i < value_byte_len / 2; i++) {
    hash = tmp[i - 1] ^ tmp[i];
  }

  return hash;
}

uint32_t crc32b(const char *str) {
  // Source: https://stackoverflow.com/a/21001712
  unsigned int byte, crc, mask;
  int i = 0, j;
  crc = 0xFFFFFFFF;
  while (str[i] != 0) {
    byte = str[i];
    crc = crc ^ byte;
    for (j = 7; j >= 0; j--) {
      mask = -(crc & 1);
      crc = (crc >> 1) ^ (0xEDB88320 & mask);
    }
    i = i + 1;
  }
  return ~crc;
}

// Initializes a new bucket
Bucket bucket_new() {
  Bucket bucket;
  bucket.values = list_new();
  return bucket;
}

// Initializes a new bucket content struct
BucketContent bucket_content_new(char *key, void *value) {
  BucketContent content;
  char *content_key = (char *)malloc(strlen(key) + 1);
  strcpy(content_key, key);
  content.key = content_key;
  content.value = value;
  return content;
}

// Initializes a new HashMap
HashMap * hashmap_new() {
  HashMap * map = malloc(sizeof(HashMap));
  map->buckets = (Bucket *)malloc(sizeof(Bucket *) * NUM_BUCKETS);
  for (int i = 0; i < NUM_BUCKETS; i++) {
    map->buckets[i] = bucket_new();
  }
  return map;
}

// This inserts a new value into a given bucket
// This function will determine whether a new entry in this bucket's list is to
// be created or a new content element is to be added to the list
void bucket_insert_value(Bucket *bucket, KEY_TYPE key, void *value) {
  assert(bucket != NULL);
  ssize_t bucket_len = list_len(bucket->values);

  for (int i = 0; i < bucket_len; i++) {
    ListGetResult result = list_at(bucket->values, i);
    assert(result.found);
    BucketContent *content = (BucketContent *)result.value;

    MAP_VERBOSE("probing bucket content (bucket key %s | key %s)\n",
                content->key, key);

    // check if the key exists, if so, reuse this bucket content
    if (strcmp(content->key, key) == 0) {
      content->value = value;
      strcpy(content->key, key);
      MAP_VERBOSE("Inserted into existing bucket\n");
      return;
    }
  }

  BucketContent *content = (BucketContent *)malloc(sizeof(BucketContent));

  // in this case, the key does not exist in this bucket, insert it
  content->key = (char *)malloc(strlen(key) + 1);
  strcpy(content->key, key);

  MAP_VERBOSE("new bucket content str: %s\n", content->key);
  content->value = value;

  list_append(bucket->values, content);

  MAP_VERBOSE("Inserted key into new bucket. New bucket len: %d\n",
              list_len(bucket->values));
}

MapGetResult bucket_get_value(Bucket *bucket, KEY_TYPE key) {
  assert(bucket != NULL);
  ssize_t bucket_len = list_len(bucket->values);

  MapGetResult result = {.found = false, .value = NULL};

  for (int i = 0; i < bucket_len; i++) {
    ListGetResult list_res = list_at(bucket->values, i);
    assert(list_res.found);
    BucketContent *content = (BucketContent *)list_res.value;

    MAP_VERBOSE("probing bucket content (bucket key %s | key %s)\n",
                content->key, key);

    // if the key exists, return the bucket content
    if (strcmp(content->key, key) == 0) {
      result.value = content->value;
      result.found = true;
      return result;
    }
  }
  return result;
}

void hashmap_insert(HashMap *map, KEY_TYPE key, void *value) {
  assert(map != NULL);
  assert(key != NULL);

  u_int16_t hash = MAP_HASHING_FUNCTION(key);
  ssize_t index = hash % NUM_BUCKETS;

  MAP_VERBOSE("Key %s | Hash %d | Index %ld\n", key, hash, index);

  MAP_VERBOSE("Inserting into bucket at %ld\n", index);
  bucket_insert_value(&map->buckets[index], key, value);
}

MapGetResult hashmap_get(HashMap *map, KEY_TYPE key) {
  assert(map != NULL);
  assert(key != NULL);

  u_int16_t hash = MAP_HASHING_FUNCTION(key);
  ssize_t index = hash % NUM_BUCKETS;

  MAP_VERBOSE("Key %s | Hash %d | Index %ld\n", key, hash, index);

  MAP_VERBOSE("Retrieving key %s from bucket at %ld\n", key, index);

  return bucket_get_value(&map->buckets[index], key);
}

void hashmap_print_buckets(HashMap *map) {
  for (int i = 0; i < NUM_BUCKETS; i++) {
    Bucket bucket = map->buckets[i];
    ssize_t bucket_len = list_len(bucket.values);

    printf("Bucket %d has length %ld\n", i, bucket_len);
  }
}

bool bucket_delete_key(Bucket *bucket, KEY_TYPE key) {
  assert(bucket != NULL);
  ssize_t bucket_len = list_len(bucket->values);

  for (int i = 0; i < bucket_len; i++) {
    ListGetResult list_res = list_at(bucket->values, i);
    assert(list_res.found);
    BucketContent *content = (BucketContent *)list_res.value;

    MAP_VERBOSE("probing bucket content (bucket key %s | key %s)\n",
                content->key, key);

    // if the key exists, delete it from the bucket
    if (strcmp(content->key, key) == 0) {
      // DELETE THIS
      list_delete_index(bucket->values, i);
      free(content);
    }
  }

  return false;
}

bool hashmap_delete(HashMap *map, KEY_TYPE key) {
  assert(map != NULL);
  assert(key != NULL);

  u_int16_t hash = MAP_HASHING_FUNCTION(key);
  ssize_t index = hash % NUM_BUCKETS;

  MAP_VERBOSE("Key %s | Hash %d | Index %ld\n", key, hash, index);
  MAP_VERBOSE("Deleting key %s from bucket at %ld\n", key, index);

  return bucket_delete_key(&map->buckets[index], key);
}

// free contents

void bucket_free(Bucket bucket) {
  ssize_t len = list_len(bucket.values);
  for (ssize_t i = 0; i < len; i++) {
    ListGetResult result = list_at(bucket.values, i);
    assert(result.found);
    BucketContent *content = (BucketContent *)result.value;

    free(content->key);
    free(content);
  }

  list_free(bucket.values);
}

void hashmap_free(HashMap *map) {
  for (int i = 0; i < NUM_BUCKETS; i++) {
    bucket_free(map->buckets[i]);
  }
  free(map->buckets);
  free(map);
}

ListNode *hashmap_keys(HashMap *map) {
  ListNode *list = list_new();

  // iterate over all buckets first
  for (int bucket_idx = 0; bucket_idx < NUM_BUCKETS; bucket_idx++) {
    Bucket bucket = map->buckets[bucket_idx];
    ssize_t bucket_len = list_len(bucket.values);

    for (ssize_t i = 0; i < bucket_len; i++) {
        ListGetResult res = list_at(bucket.values, i);
        assert(res.found);
        BucketContent * bc = (BucketContent *) res.value;
        list_append(list, bc->key);
    }
  }

  return list;
}
