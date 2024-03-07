#include "../dynstring/dynstring.h"
#include "./MyClass.h"
#include <dirent.h>
#include <dlfcn.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <unistd.h>

int main() {
  for (int i = 0; i < 10; i++) {
    MyClass *obj = new_MyClass(i);
    printf("Before: %ld\n", obj->get_value());
    obj->set_value(42);
    printf("After: %ld\n", obj->get_value());
    obj->set_value(69);
    obj->print();
  }

  return 0;
}
