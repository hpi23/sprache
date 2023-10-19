#include "./libAnyObj.h"
#include "/home/mik/Coding/hpi/hpi-c-tests/dynstring/dynstring.h"
#include "/home/mik/Coding/hpi/hpi-c-tests/json-parser/parser.h"
#include "libSAP.h"
#include "reflection.h"
#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>

AnyValue __hpi_internal_anyvalue_from_json(JsonValue value) {
  TypeDescriptor res_type = {
      .ptr_count = 0, .list_inner = NULL, .obj_fields = NULL};

  AnyValue res = {.value = NULL, .type = res_type};

  switch (value.type) {
  case JSON_TYPE_OBJECT: {
    res.type.kind = TYPE_ANY_OBJECT;

    AnyObject *any_obj = anyobj_new();

    ListNode *keys = hashmap_keys(value.object.fields);
    int64_t key_len = list_len(keys);

    for (int i = 0; i < key_len; i++) {
      ListGetResult key_res = list_at(keys, i);
      assert(key_res.found);

      MapGetResult value_res = hashmap_get(value.object.fields, key_res.value);
      assert(value_res.found);

      AnyValue *value_ptr = malloc(sizeof(AnyValue));
      *value_ptr =
          __hpi_internal_anyvalue_from_json(*(JsonValue *)value_res.value);

      hashmap_insert(any_obj->fields, key_res.value, value_ptr);
    }

    AnyObject **obj_temp = (AnyObject **)malloc(sizeof(AnyObject *));
    *obj_temp = any_obj;
    res.value = obj_temp;

    break;
  }
  case JSON_TYPE_ARRAY: {
    res.type.kind = TYPE_LIST;
    TypeDescriptor inner = {
        .obj_fields = NULL, .list_inner = NULL, .ptr_count = 0};

    ListNode *list_temp = list_new();

    ssize_t len = list_len(value.array.fields);
    for (int i = 0; i < len; i++) {
      ListGetResult curr = list_at(value.array.fields, i);
      assert(curr.found);

      AnyValue *converted_ptr = malloc(sizeof(AnyValue));

      JsonValue inner_json = *(JsonValue *)curr.value;

      *converted_ptr = __hpi_internal_anyvalue_from_json(inner_json);

      // TODO: this expects a known inner type,
      // must convert the any type to a known one
      // BUG: this will fail because other functions, such as print will expect
      // the inner type of the list to be e.g. String. However, the real type is
      // AnyValue. We will need to cast the inner type to the expected one In
      // the ideal case, this is not even handled by this function Instead, use
      // a different cast function
      list_append(list_temp, converted_ptr);

      inner = converted_ptr->type;
      // TODO: implement extensive runtime type checking
    }

    TypeDescriptor *inner_ptr = malloc(sizeof(TypeDescriptor));
    *inner_ptr = inner;
    res.type.list_inner = inner_ptr;

    ListNode **temp_list_ptr = malloc(sizeof(ListNode **));
    *temp_list_ptr = list_temp;
    res.value = temp_list_ptr;

    break;
  }
  case JSON_TYPE_INT: {
    res.type.kind = TYPE_INT;
    res.value = malloc(sizeof(int64_t));
    *(int64_t *)res.value = value.num_int;
    break;
  }
  case JSON_TYPE_FLOAT: {
    res.type.kind = TYPE_FLOAT;
    res.value = malloc(sizeof(double));
    *(double *)res.value = value.num_int;
    break;
  }
  case JSON_TYPE_BOOL: {
    res.type.kind = TYPE_BOOL;
    res.value = malloc(sizeof(bool));
    *(bool *)res.value = value.boolean;
    break;
  }
  case JSON_TYPE_STRING: {
    res.type.kind = TYPE_STRING;
    DynString **ptr_temp = malloc(sizeof(DynString *));
    DynString *str = dynstring_from(value.string);
    *ptr_temp = str;
    res.value = ptr_temp;
    break;
  case JSON_TYPE_NULL:
    res.type.kind = TYPE_NONE;
    break;
  default:
    assert("Unhandled type" && 0);
    exit(-1);
  }
  }

  return res;
}

AnyValue __hpi_internal_parse_json(DynString *input) {
  char *input_cstr = dynstring_as_cstr(input);
  NewJsonParserResult create_res = parser_new(input_cstr);
  JsonParser parser = create_res.parser;
  if (create_res.error != NULL) {
    printf("Runtime JSON parse error: `%s`\n", create_res.error);
    exit(-1);
  }

  JsonParseResult parse_res = parse_json(&parser);
  if (parse_res.error != NULL) {
    printf("Runtime JSON parse error: `%s`\n", parse_res.error);
    exit(-1);
  }

  parser_free(&parser);
  // printf("JSON: RES: %s\n", json_value_to_string(parse_res.value));

  // convert JSON value to anyvalue
  return __hpi_internal_anyvalue_from_json(parse_res.value);
}

JsonValue __hpi_internal_json_value_from_void(TypeDescriptor type,
                                              void *value) {
  JsonValue res = {};
  switch (type.kind) {
  case TYPE_NONE:
    res.type = JSON_TYPE_NULL;
    break;
  case TYPE_INT:
    res.type = JSON_TYPE_INT;
    res.num_int = *(int64_t *)value;
    break;
  case TYPE_FLOAT:
    res.type = JSON_TYPE_FLOAT;
    res.num_int = *(double *)value;
    break;
  case TYPE_CHAR:
    res.type = JSON_TYPE_STRING;
    res.string = malloc(2);
    res.string[0] = *(char *)value;
    res.string[1] = '\0';
    break;
  case TYPE_BOOL:
    res.type = JSON_TYPE_BOOL;
    res.boolean = *(bool *)value;
    break;
  case TYPE_LIST: {
    ListNode *list = *(ListNode **)value;
    ssize_t len = list_len(list);

    JsonValueArray arr = {.fields = list_new()};

    for (int i = 0; i < len; i++) {
      ListGetResult curr = list_at(list, i);
      assert(curr.found);

      JsonValue converted =
          __hpi_internal_json_value_from_void(*type.list_inner, curr.value);
      JsonValue *converted_ptr = malloc(sizeof(JsonValue));
      *converted_ptr = converted;
      list_append(arr.fields, converted_ptr);
    }

    res.type = JSON_TYPE_ARRAY;
    res.array = arr;
    break;
  }
  case TYPE_OBJECT: {
    JsonValueObject obj = {.fields = hashmap_new()};

    HashMap *map = *(HashMap **)value;

    ListNode *keys = hashmap_keys(map);
    size_t key_len = list_len(keys);

    for (int i = 0; i < key_len; i++) {
      ListGetResult key = list_at(keys, i);
      assert(key.found);

      MapGetResult value = hashmap_get(map, key.value);
      assert(value.found);

      MapGetResult type_res = hashmap_get(type.obj_fields, key.value);
      assert(type_res.found);

      JsonValue converted = __hpi_internal_json_value_from_void(
          *(TypeDescriptor *)type_res.value, value.value);

      JsonValue *converted_ptr = malloc(sizeof(JsonValue));
      *converted_ptr = converted;

      hashmap_insert(obj.fields, key.value, converted_ptr);
    }

    res.type = JSON_TYPE_OBJECT;
    res.object = obj;
    break;
  }
  case TYPE_ANY_OBJECT: {
    HashMap *src = *(HashMap **)value;
    JsonValueObject obj = {.fields = hashmap_new()};

    ListNode *keys = hashmap_keys(obj.fields);
    ssize_t key_len = list_len(keys);

    for (int i = 0; i < key_len; i++) {
      ListGetResult key = list_at(keys, i);
      assert(key.found);

      MapGetResult value_res = hashmap_get(src, key.value);
      assert(value_res.found);

      AnyValue value = *(AnyValue *)value_res.value;

      JsonValue converted =
          __hpi_internal_json_value_from_void(value.type, value.value);
      JsonValue *converted_ptr = malloc(sizeof(JsonValue));
      *converted_ptr = converted;
      hashmap_insert(obj.fields, key.value, converted_ptr);
    }

    res.type = JSON_TYPE_OBJECT;
    res.object = obj;
    break;
  }
  case TYPE_STRING:
    res.type = JSON_TYPE_STRING;
    DynString *buf_copy = dynstring_clone(*(DynString **)value);

    DynString *what = dynstring_from("\n");
    DynString *with = dynstring_from("\\n");

    dynstring_replace(buf_copy, what, with);

    dynstring_free(what);
    dynstring_free(with);

    res.string = dynstring_as_cstr(buf_copy);
    dynstring_free(buf_copy);
    break;
  }

  return res;
}

DynString *__hpi_internal_marshal_json(TypeDescriptor type, void *value) {
  JsonValue val = __hpi_internal_json_value_from_void(type, value);
  char *output = json_value_to_string(val);
  DynString *res = dynstring_from(output);
  free(output);
  return res;
}
