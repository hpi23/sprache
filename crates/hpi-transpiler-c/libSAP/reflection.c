#include "./reflection.h"
#include "dynstring/dynstring.h"
#include "list/list.h"
#include <assert.h>

char *display_type(TypeDescriptor type) {
  DynString *output = dynstring_new();

  DynString *pointer_prefix = dynstring_from("Zeiger auf ");
  dynstring_repeat(pointer_prefix, type.ptr_count);

  switch (type.kind) {
  case TYPE_NONE:
    dynstring_set(output, "Nichts");
    break;
  case TYPE_INT:
    dynstring_set(output, "Zahl");
    break;
  case TYPE_FLOAT:
    dynstring_set(output, "Fließkommazahl");
    break;
  case TYPE_CHAR:
    dynstring_set(output, "Zeichen");
    break;
  case TYPE_BOOL:
    dynstring_set(output, "Wahrheitswert");
    break;
  case TYPE_LIST:
    dynstring_set(output, "Liste von ");
    char *inner_type_str = display_type(*type.list_inner);
    dynstring_push_string(output, inner_type_str);
    free(inner_type_str);
    break;
  case TYPE_OBJECT:
    dynstring_set(output, "Objekt {");

    ListNode *keys = hashmap_keys(type.obj_fields);
    size_t keys_len = list_len(keys);

    for (int i = 0; i < keys_len; i++) {
      ListGetResult key = list_at(keys, i);
      assert(key.found);

      MapGetResult type_res = hashmap_get(type.obj_fields, key.value);
      assert(type_res.found);

      char *inner_type_str = display_type(*(TypeDescriptor *)type_res.value);
      dynstring_push_string(output, inner_type_str);
      free(inner_type_str);
      dynstring_push_string(output, " ");
      dynstring_push_string(output, key.value);

      if (i + 1 < keys_len) {
        dynstring_push_string(output, ", ");
      }
    }

    list_free(keys);
    dynstring_push_char(output, '}');
    break;
  case TYPE_ANY_OBJECT:
    dynstring_set(output, "Speicherbox");
    break;
  case TYPE_ANY_VALUE:
    dynstring_set(output, "<AnyValue>");
    break;
  case TYPE_STRING:
    dynstring_set(output, "Zeichenkette");
    break;
  }

  dynstring_push(pointer_prefix, output);
  dynstring_free(output);
  char *out = dynstring_as_cstr(pointer_prefix);
  dynstring_free(pointer_prefix);
  return out;
}

// TODO: remove this
// uint type_sizeof(TypeDescriptor type) {
//   if (type.ptr_count > 1) {
//     return sizeof(void *);
//   }
//
//   switch (type.kind) {
//   case TYPE_NONE:
//       goto fail;
//   case TYPE_INT:
//       return sizeof(int64_t);
//   case TYPE_FLOAT:
//       return sizeof(double);
//   case TYPE_CHAR:
//       return sizeof(char);
//   case TYPE_BOOL:
//       return sizeof(char);
//   case TYPE_LIST:
//       return sizeof(ListNode);
//   case TYPE_OBJECT:
//   case TYPE_ANY_OBJECT:
//   case TYPE_ANY_VALUE:
//   case TYPE_STRING:
//     break;
//   }
//
// fail: {
//   char *type_str = display_type(type);
//   printf("type_sizeof(): Illegal type: %s\n", type_str);
//   free(type_str);
//   abort();
// }
// }
