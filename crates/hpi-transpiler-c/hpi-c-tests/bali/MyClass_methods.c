#include "MyClass.h"
#include <stdint.h>
#include <stdio.h>

int64_t myclass_get_value(MyClass *this) { return this->value; }
void myclass_set_value(MyClass *this, int64_t value) { this->value = value; }
void myclass_print(MyClass *this) { printf("Printing from the object: %ld\n", this->value); }
