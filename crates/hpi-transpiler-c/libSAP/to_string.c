#include "./libSAP.h"
#include "/home/mik/Coding/hpi/hpi-c-tests/dynstring/dynstring.h"
#include "/home/mik/Coding/hpi/hpi-c-tests/vec/vec.h"
#include "reflection.h"

int indent_str = 4;

DynString *to_string(TypeDescriptor type, void *value) {
  DynString *output = dynstring_new();

  switch (type.kind) {
  case TYPE_NONE:
    dynstring_push_string(output, "Nichts");
    break;
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
    dynstring_push_char(output, '[');

    ListNode *list = *(ListNode **)value;

    while (list != NULL) {
      TypeDescriptor new_type = {.kind = type.list_inner->kind,
                                 .list_inner = type.list_inner->list_inner,
                                 .ptr_count = 0};

      dynstring_push(output, to_string(new_type, list->value));

      list = list->next;
      if (list != NULL) {
        dynstring_push_string(output, ", ");
      }
    }

    dynstring_push_char(output, ']');

    break;
  }
  case TYPE_OBJECT: {
    dynstring_push_string(output, "Objekt {\n");

    HashMap *map = *(HashMap **)value;
    ListNode *keys = hashmap_keys(map);

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
      dynstring_push(output, to_string(type_descriptor, res.value));
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
  case TYPE_ANY_OBJECT: {
    dynstring_push_string(output, "Speicherbox {\n");

    AnyObject *obj = *(AnyObject **)value;
    ListNode *keys = hashmap_keys(obj->fields);
    ssize_t keys_len = list_len(keys);

    for (int i = 0; i < keys_len; i++) {
      char *key = (char *)list_at(keys, i).value;
      MapGetResult res = hashmap_get(obj->fields, key);
      assert(res.found);

      AnyValue *item = res.value;

      for (int i = 0; i < indent_str; i++) {
        dynstring_push_char(output, ' ');
      }

      dynstring_push_string(output, key);
      indent_str += 4;
      dynstring_push_string(output, ": ");
      dynstring_push(output, to_string(item->type, item->value));
      indent_str -= 4;

      if (i + 1 < keys_len) {
        dynstring_push_string(output, ",\n");
      }
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
