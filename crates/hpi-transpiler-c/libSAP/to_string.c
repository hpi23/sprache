#include "./libSAP.h"
#include "vec/vec.h"
#include "dynstring/dynstring.h"
#include "list/list.h"
#include "reflection.h"
#include <sys/types.h>

int indent_str = 4;

DynString *to_string(TypeDescriptor type, void *value) {
  assert(value != NULL);
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

    ssize_t len = list_len(list);
    for (int i = 0; i < len; i++) {
      ListGetResult res =
          list_at(list, i); // TODO: list has the same value for each member
      assert(res.found);
      assert(type.list_inner != NULL);
      DynString * elem_str = to_string(*type.list_inner, res.value);
      dynstring_push(output, elem_str);
      dynstring_free(elem_str);

      if (i + 1 < len) {
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
    ListNode *keys_first = keys;

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
      DynString *field_str = to_string(type_descriptor, res.value);
      dynstring_push(output, field_str);
      dynstring_free(field_str);
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

    list_free(keys_first);
    dynstring_push_char(output, '}');
    break;
  }
  case TYPE_ANY_OBJECT: {
    dynstring_push_string(output, "Speicherbox {\n");

    printf("Object addr: %p\n", value);
    AnyObject *obj = *(AnyObject **)value;
    assert(obj != NULL);
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

      DynString * field_str = to_string(item->type, item->value);
      dynstring_push(output, field_str);
      dynstring_free(field_str);

      indent_str -= 4;

      if (i + 1 < keys_len) {
        dynstring_push_string(output, ",\n");
      }
    }

    list_free(keys);

    dynstring_push_char(output, '\n');
    for (int i = 0; i < indent_str - 4; i++) {
      dynstring_push_char(output, ' ');
    }

    dynstring_push_char(output, '}');

    break;
  }
  case TYPE_STRING: {
    {
      DynString *str = *(DynString **)value;
      assert(str != NULL);
      char *string_raw = dynstring_as_cstr(str);

      dynstring_push_string(output, string_raw);

      free(string_raw);
      break;
    }
  }
  }

  return output;
}
