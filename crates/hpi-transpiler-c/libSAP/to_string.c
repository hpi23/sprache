#include "./libSAP.h"
#include "/home/mik/Coding/hpi/hpi-c-tests/dynstring/dynstring.h"
#include "/home/mik/Coding/hpi/hpi-c-tests/vec/vec.h"

int indent_str = 4;

DynString *to_string(TypeDescriptor type, void *value) {
  DynString *output = dynstring_new();

  switch (type.kind) {
  case TYPE_INT: {
    dynstring_push_fmt(output, "%ld", *(int64_t *)value);
    break;
  }
  case TYPE_FLOAT: {
    dynstring_push_fmt(output, "%f", *(double *)value);
    break;
  }
  case TYPE_CHAR: {
    dynstring_push_fmt(output, "%c", *(char *)value);
    break;
  }
  case TYPE_BOOL: {
    if (*(bool *)value) {
      dynstring_push_string(output, "true");
    } else {
      dynstring_push_string(output, "false");
    }
    break;
  }
  case TYPE_LIST: {
    ListNode *list = *(ListNode **)value;

    dynstring_push_char(output, '[');

    while (list != NULL) {
      TypeDescriptor new_type = {.kind = type.list_inner->kind,
                                 .list_inner = type.list_inner->list_inner,
                                 .ptr_count = 0};

      to_string(new_type, list->value);

      list = list->next;
      if (list != NULL) {
        dynstring_push_string(output, ", ");
      }
    }

    dynstring_push_char(output, ']');

    break;
  }
  case TYPE_OBJECT: {
    HashMap *map = *(HashMap **)value;

    ListNode *keys = hashmap_keys(map);

    dynstring_push_string(output, "Objekt {\n");

    while (keys != NULL) {
      char *key = keys->value;
      MapGetResult res = hashmap_get(map, key);
      assert(res.found);

      MapGetResult type_res = hashmap_get(type.obj_fields, key);
      assert(type_res.found);

      TypeDescriptor type_descriptor = *(TypeDescriptor *)type_res.value;

      for (int i = 0; i < indent_str; i++) {
        dynstring_push_char(output, ' ');
      }

      dynstring_push_fmt(output, "%s: ", key);
      indent_str += 4;
      to_string(type_descriptor, res.value);
      indent_str -= 4;

      if (keys->next != NULL) {
        dynstring_push_string(output, ",\n");
      }

      keys = keys->next;
    }

    dynstring_push_char(output, '\n');
    for (int i = 0; i < indent_str - 4; i++) {
      dynstring_push_char(output, ' ');
    }

    dynstring_push_char(output, '}');

    break;
  }
  case TYPE_STRING: {
    char *string_raw = dynstring_as_cstr(*(DynString **)value);

    dynstring_push_string(output, string_raw);

    free(string_raw);
    break;
  }
  }

  return output;
}
