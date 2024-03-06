#pragma once
#include "../list/list.h"

#define NUM_BUCKETS 8
#define KEY_TYPE char *

// #define MAP_VERBOSE(...) printf(__VA_ARGS__)
#define MAP_VERBOSE(...)

// #define MAP_HASHING_FUNCTION crc32b
#define MAP_HASHING_FUNCTION map_internal_hash

typedef struct {
  bool found;
  void *value;
} MapGetResult;

typedef struct {
  KEY_TYPE key;
  void *value;
} BucketContent;

typedef struct {
  ListNode *values;
} Bucket;

typedef struct {
  Bucket *buckets;
} HashMap;

HashMap * hashmap_new();
void hashmap_insert(HashMap *map, KEY_TYPE key, void *value);
MapGetResult hashmap_get(HashMap *map, KEY_TYPE key);
void hashmap_print_buckets(HashMap *map);
bool hashmap_delete(HashMap *map, KEY_TYPE key);
void hashmap_free(HashMap *map);
ListNode * hashmap_keys(HashMap *map);
