#pragma once
#include <stdint.h>

#define MyClass_VERBOSE 0

// int64_t this.get_value()
// void this.set_value(int64_t value)
typedef struct MyClass {
  int64_t value;
  int64_t (*get_value)(void);
  void (*set_value)(int64_t);
  void (*print)(void);
} MyClass;

MyClass *new_MyClass(int64_t value);

int64_t myclass_get_value(MyClass *self);
void myclass_set_value(MyClass *self, int64_t value);
void myclass_print(MyClass *self);
