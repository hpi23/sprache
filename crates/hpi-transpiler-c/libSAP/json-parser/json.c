#include "./json.h"
#include "../dynstring/dynstring.h"
#include "../hashmap/map.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

char *__internal_json_value_to_string(JsonValue value, ssize_t indent);

void json_print_value(JsonValue value) {
  char *res = __internal_json_value_to_string(value, 0);
  printf("%s\n", res);
  free(res);
}

char *json_object_to_string(JsonValueObject object, ssize_t indent) {
  assert(object.fields != NULL);
  DynString *buf = dynstring_from("{\n");

  ListNode *keys_start = hashmap_keys(object.fields);
  int len = list_len(keys_start);

  ListNode *keys = keys_start;
  for (int i = 0; i < len; i++) {
    ListGetResult key = list_at(keys, i);
    assert(key.found);

    MapGetResult value = hashmap_get(object.fields, (char *)key.value);
    assert(value.found);

    JsonValue val = *(JsonValue *)value.value;
    char *value_buf = __internal_json_value_to_string(val, indent + 4);

    DynString *indent_buf = dynstring_from(" ");
    dynstring_repeat(indent_buf, indent);

    char *indent = dynstring_as_cstr(indent_buf);
    dynstring_push_fmt(buf, "%s\"%s\": %s", indent, (char *)key.value, value_buf); // A segfault lies here

    if (val.type != JSON_TYPE_BOOL && val.type != JSON_TYPE_NULL) {
      free(value_buf);
    }

    free(indent);
    dynstring_free(indent_buf);

    // prevent trailing comma
    if (i + 1 < len) {
      dynstring_push_char(buf, ',');
    }

    dynstring_push_char(buf, '\n');
  }

  list_free(keys_start);

  DynString *indent_buf = dynstring_from(" ");
  dynstring_repeat(indent_buf, indent - 4);

  char *indent_str = dynstring_as_cstr(indent_buf);

  dynstring_push_fmt(buf, "%s}", indent_str);
  free(indent_str);
  dynstring_free(indent_buf);

  char *c_str = dynstring_as_cstr(buf);
  dynstring_free(buf);
  return c_str;
}

char *json_array_to_string(JsonValueArray array, ssize_t indent) {
  ListNode *list = array.fields;
  ssize_t len = list_len(list);

  // detect if the formatting should be multiline
  bool multiline = false;

  for (int i = 0; i < len; i++) {
    ListGetResult curr = list_at(list, i);
    assert(curr.found);

    JsonValue *val = (JsonValue *)curr.value;
    if (val->type == JSON_TYPE_OBJECT || val->type == JSON_TYPE_ARRAY) {
      multiline = true;
      break;
    }
  }

  DynString *indent_buf = dynstring_from(" ");
  if (multiline) {
    dynstring_repeat(indent_buf, indent);
  } else {
    dynstring_clear(indent_buf);
  }
  char *indent_str = dynstring_as_cstr(indent_buf);

  DynString *buf = dynstring_from("[");
  if (multiline) {
    dynstring_push_char(buf, '\n');
  }

  for (int i = 0; i < len; i++) {
    ListGetResult curr = list_at(list, i);
    assert(curr.found);

    JsonValue *value = (JsonValue *)curr.value;
    char *value_buf = __internal_json_value_to_string(*value, indent + 4);

    dynstring_push_fmt(buf, "%s%s", indent_str, value_buf);
    free(value_buf);
    if (i + 1 < len) {
      if (multiline) {
        dynstring_push_string(buf, ",\n");
      } else {
        dynstring_push_string(buf, ", ");
      }
    }
  }

  if (multiline) {
    dynstring_push_char(buf, '\n');
    DynString *indent_buf = dynstring_from(" ");
    dynstring_repeat(indent_buf, indent - 4);
    char *indent_str = dynstring_as_cstr(indent_buf);
    dynstring_push_string(buf, indent_str);
    free(indent_str);
    dynstring_free(indent_buf);
  }
  dynstring_push_char(buf, ']');

  char *c_str = dynstring_as_cstr(buf);
  dynstring_free(buf);

  dynstring_free(indent_buf);
  free(indent_str);
  return c_str;
}

char *json_value_to_string(JsonValue value) { return __internal_json_value_to_string(value, 4); }

// TODO: implement memory free / allocation more elegantly
char *__internal_json_value_to_string(JsonValue value, ssize_t indent) {
  switch (value.type) {
  case JSON_TYPE_NULL: {
    return "Nichts";
  }
  case JSON_TYPE_OBJECT:
    return json_object_to_string(value.object, indent);
  case JSON_TYPE_ARRAY:
    return json_array_to_string(value.array, indent);
  case JSON_TYPE_INT: {
    char *buf;
    asprintf(&buf, "%ld", value.num_int);
    return buf;
  }
  case JSON_TYPE_FLOAT: {
    char *buf;
    asprintf(&buf, "%f", value.num_float);
    return buf;
  }
  case JSON_TYPE_BOOL: {
    if (value.boolean) {
      return "true";
    } else {
      return "false";
    }
  }
  case JSON_TYPE_STRING: {
    char *buf;
    asprintf(&buf, "\"%s\"", value.string);
    return buf;
  }
  }

  printf("Unreachable: every value case is handled above. | %d\n", value.type);
  assert(0);
}

//
// Helper functions
//

void json_value_object_free(JsonValueObject obj) {
  ListNode *keys_start = hashmap_keys(obj.fields);

  ListNode *keys = keys_start;
  while (keys != NULL) {
    char *key = (char *)keys->value;
    assert(key != NULL);

    MapGetResult value = hashmap_get(obj.fields, key);
    assert(value.found);

    JsonValue *json_value = (JsonValue *)value.value;
    json_value_free(*json_value);

    free(json_value);

    keys = keys->next;
  }

  list_free(keys_start);

  hashmap_free(obj.fields);
}

void json_value_array_free(JsonValueArray value) {
  ListNode *elements = value.fields;

  ssize_t len = list_len(elements);

  for (int i = 0; i < len; i++) {
    ListGetResult res = list_at(elements, i);

    JsonValue *current = (JsonValue *)res.value;
    json_value_free(*current);
    free(current);
  }

  list_free(value.fields);
}

void json_value_free(JsonValue value) {
  switch (value.type) {
  case JSON_TYPE_OBJECT:
    json_value_object_free(value.object);
    break;
  case JSON_TYPE_ARRAY:
    json_value_array_free(value.array);
    break;
  case JSON_TYPE_STRING:
    free(value.string);
  default:
    // These types do not need to be freed
    return;
  }
}
