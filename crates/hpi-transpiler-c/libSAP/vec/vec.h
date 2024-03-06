#pragma once
#include <assert.h>
#include <string.h>
#include <stdint.h>
#include <stdio.h>
#include <sys/types.h>

#define DEFAULT_CAPACITY 1

#ifndef VEC_FORMAT_SPECIFIER
#define VEC_FORMAT_SPECIFIER "%p"
#endif

#ifndef VEC_VALUE_TYPE
#define VEC_VALUE_TYPE void*
#endif

#define VEC_IS_VERBOSE false

#if VEC_IS_VERBOSE
#define VEC_VERBOSE(...) printf(__VA_ARGS__)
#else
#define VEC_VERBOSE(...) // (__VA_ARGS__)
#endif


typedef struct {
  VEC_VALUE_TYPE *values;
  uint capacity;
  uint used;
} Vec;

Vec *vec_new_with_capacity(uint capacity);
Vec *vec_new();
void vec_push(Vec *vec, VEC_VALUE_TYPE value);
void vec_pop(Vec *vec);
void vec_pop_front(Vec *vec);
void vec_shrink_to_fit(Vec *vec);
void vec_print(Vec *vec);
VEC_VALUE_TYPE vec_index(Vec *vec, uint index);
VEC_VALUE_TYPE vec_remove(Vec *vec, size_t index);
void vec_free(Vec *vec);
