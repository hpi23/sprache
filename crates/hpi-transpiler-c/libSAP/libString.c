#include "/home/mik/Coding/hpi/hpi-c-tests/dynstring/dynstring.h"
#include "/home/mik/Coding/hpi/hpi-c-tests/list/list.h"

bool __hpi_internal_string_starts_with(DynString *base, DynString test) {
  if (test.length > base->length) {
    return false;
  }

  for (int i = 0; i < test.length; i++) {
    if (base->internal_str[i] != test.internal_str[i]) {
      return false;
    }
  }

  return true;
}

DynString * __hpi_internal_string_replace(DynString * base_str, DynString * replace_src, DynString * replace_with) {

    return base_str;
}
