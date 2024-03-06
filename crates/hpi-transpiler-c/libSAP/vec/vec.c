#include "./vec.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

void vec_set_size(Vec *vec, uint new_size);

Vec *vec_new_with_capacity(uint capacity) {
  Vec *vec_temp = (Vec *)malloc(sizeof(Vec));
  vec_temp->capacity = capacity;
  vec_temp->used = 0;
  vec_temp->values = malloc(sizeof(VEC_VALUE_TYPE) * vec_temp->capacity);
  return vec_temp;
}

Vec *vec_new() {
  Vec *vec_temp = vec_new_with_capacity(DEFAULT_CAPACITY);
  return vec_temp;
}

void vec_push(Vec *vec, VEC_VALUE_TYPE value) {
  assert(vec != NULL);
  if (vec->capacity <= vec->used) {
    uint new_capacity = vec->capacity * 2;
    if (new_capacity == 0) {
      new_capacity = 1;
    }

    VEC_VERBOSE("vec: growing from old capacity %d to new capacity: %d\n",
                vec->capacity, new_capacity);
    vec_set_size(vec, new_capacity);
  }
  vec->values[vec->used] = value;
  vec->used++;
}

void vec_set_size(Vec *vec, uint new_size) {
  vec->capacity = new_size;
  vec->values =
      (VEC_VALUE_TYPE *)realloc(vec->values, new_size * sizeof(VEC_VALUE_TYPE));
}

// Removes the last element of the vector without deallocating memory.
void vec_pop(Vec *vec) {
  if (vec->used == 0) {
    return;
  }
  vec->used--;
}

void vec_pop_front(Vec *vec) {
  if (vec->used == 0) {
    return;
  }
  vec->used--;
  vec->capacity = vec->used;
  VEC_VALUE_TYPE *buf_temp = malloc(sizeof(VEC_VALUE_TYPE) * vec->capacity);

  for (int i = 1; i < vec->capacity + 1; i++) {
    buf_temp[i - 1] = vec->values[i];
  }

  free(vec->values);
  vec->values = buf_temp;
};

// Shrinks the vector as much as possible.
void vec_shrink_to_fit(Vec *vec) {
  // cannot shrink, everything is used
  if (vec->capacity == vec->used) {
    return;
  }

  uint new_capacity = vec->used;
  if (new_capacity == 0) {
    new_capacity = 1;
  }

  VEC_VERBOSE("vec: shrinking from old size %uld to new size of %d\n",
              vec->capacity, new_capacity);
  vec_set_size(vec, new_capacity);
}

void vec_print(Vec *vec) {
  printf("[");
  for (int i = 0; i < vec->used; i++) {
    printf(VEC_FORMAT_SPECIFIER, vec->values[i]);
    if (i + 1 < vec->used) {
      printf(", ");
    }
  }
  printf("]\n");
}

VEC_VALUE_TYPE vec_index(Vec *vec, uint index) {
  if (index >= vec->used) {
    printf("vec_index(): Illegal index %ud\n", index);
    assert(0);
  }

  return vec->values[index];
}

VEC_VALUE_TYPE vec_remove(Vec *vec, size_t index) {
  assert(index >= 0 && index < vec->used);

  if (vec->used == 0) {
    return -1;
  }

  if (index == vec->used - 1) {
    VEC_VALUE_TYPE old = vec_index(vec, vec->used - 1);
    vec_pop(vec);
    return old;
  }

  VEC_VALUE_TYPE element = vec_index(vec, index);

  uint remaining_bytes = (vec->used - index - 1) * sizeof(VEC_VALUE_TYPE);

  memcpy(&vec->values[index], &vec->values[index + 1], remaining_bytes);

  vec->used--;

  return element;
};

void vec_free(Vec *vec) {
  free(vec->values);
  free(vec);
}
