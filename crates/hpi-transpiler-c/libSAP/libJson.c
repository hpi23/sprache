#include "dynstring/dynstring.h"
#include "json-parser/parser.h"
#include "libAnyObj.h"
#include "list/list.h"
#include "reflection.h"
#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

AnyValue __hpi_internal_anyvalue_from_json(JsonValue value, void *(allocator)(TypeDescriptor),
                                           void(trace_allocation)(void *addr, TypeDescriptor type, TypeDescriptor *type_ptr)) {
  TypeDescriptor res_type = {.ptr_count = 0, .list_inner = NULL, .obj_fields = NULL};

  AnyValue res = {.value = NULL, .type = res_type};

  switch (value.type) {
  case JSON_TYPE_OBJECT: {
    res.type.kind = TYPE_ANY_OBJECT;

    TypeDescriptor type_descriptor_anyobj = {.ptr_count = 0, .list_inner = NULL, .kind = TYPE_ANY_OBJECT, .obj_fields = NULL};

    AnyObject *any_obj = allocator(type_descriptor_anyobj);
    // JUST TO TEST:

    printf("======%p\n", any_obj->fields->buckets->values);

    printf("ALLOCATED ANY_OBJ (leak): %p\n", any_obj);
    // TODO: remove
    // AnyObject *any_obj = anyobj_new();

    ListNode *keys = hashmap_keys(value.object.fields);
    int64_t key_len = list_len(keys);

    for (int i = 0; i < key_len; i++) {
      ListGetResult key_res = list_at(keys, i);
      assert(key_res.found);

      MapGetResult value_res = hashmap_get(value.object.fields, key_res.value);
      assert(value_res.found);

      // TODO: is this a memory leak?
      // AnyValue *value_ptr = (AnyValue *)malloc(sizeof(AnyValue));
      AnyValue *value_ptr = allocator((TypeDescriptor){
          .ptr_count = 0,
          .obj_fields = NULL,
          .kind = TYPE_ANY_VALUE,
          .list_inner = NULL,
      });

      assert(value_ptr != NULL);

      // TODO: this is broken for sure
      *value_ptr = __hpi_internal_anyvalue_from_json(*(JsonValue *)value_res.value, allocator, trace_allocation);

      hashmap_insert(any_obj->fields, key_res.value, value_ptr);
    }

    list_free(keys);

    TypeDescriptor ptr_to_anyobj = type_descriptor_anyobj;
    ptr_to_anyobj.ptr_count = 1;
    AnyObject **obj_temp = (AnyObject **)allocator(ptr_to_anyobj);
    *obj_temp = any_obj;
    res.value = obj_temp;

    break;
  }
  case JSON_TYPE_ARRAY: {
    res.type.kind = TYPE_LIST;

    // Type setup
    TypeDescriptor *inner_heap = malloc(sizeof(TypeDescriptor));
    // TypeDescriptor inner = {.obj_fields = NULL, .list_inner = NULL, .ptr_count = 0};
    inner_heap->kind = TYPE_NONE;
    inner_heap->obj_fields = NULL;
    inner_heap->list_inner = NULL;
    inner_heap->ptr_count = 0;

    TypeDescriptor list_type = {.obj_fields = NULL, .ptr_count = 0, .kind = TYPE_LIST, .list_inner = inner_heap};

    ListNode *list_temp = list_new();

    ssize_t len = list_len(value.array.fields);
    for (int i = 0; i < len; i++) {
      ListGetResult curr = list_at(value.array.fields, i);
      assert(curr.found);

      // AnyValue *converted_ptr = malloc(sizeof(AnyValue));
      AnyValue *converted_ptr = allocator((TypeDescriptor){.kind = TYPE_ANY_VALUE, .list_inner = NULL, .ptr_count = 0, .obj_fields = NULL});

      JsonValue inner_json = *(JsonValue *)curr.value;

      *converted_ptr = __hpi_internal_anyvalue_from_json(inner_json, allocator, trace_allocation);

      // TODO: this expects a known inner type,
      // must convert the any type to a known one
      // BUG: this will fail because other functions, such as print will expect
      // the inner type of the list to be e.g. String. However, the real type is
      // AnyValue. We will need to cast the inner type to the expected one In
      // the ideal case, this is not even handled by this function Instead, use
      // a different cast function
      list_append(list_temp, converted_ptr);
      *inner_heap = converted_ptr->type;
      // TODO: implement extensive runtime type checking
    }

    if (trace_allocation != NULL) {
      // TODO: clone the inner_heap type spec

      TypeDescriptor *inner_ptr = malloc(sizeof(TypeDescriptor));
      *inner_ptr = clone_type(*inner_heap);

      trace_allocation(list_temp, list_type, inner_ptr);
    }

    // TODO: remov all of this!
    // NOTE: this is needed because if an `AnyValue` is freed, its type is also freed as well.
    // So if every list element shared the same pointer, there would be double frees.
    // TypeDescriptor *inner_ptr = malloc(sizeof(TypeDescriptor));
    // *inner_ptr = clone_type(*inner_heap);
    res.type.list_inner = inner_heap;

    // ListNode **temp_list_ptr = allocator((TypeDescriptor){.kind = TYPE_LIST, .ptr_count = 1, .list_inner = inner_heap, .obj_fields = NULL});
    ListNode **temp_list_ptr = malloc(sizeof(ListNode *));
    trace_allocation(temp_list_ptr, (TypeDescriptor){.kind = TYPE_LIST, .ptr_count = 1, .list_inner = inner_heap, .obj_fields = NULL}, inner_heap);

    // ListNode **temp_list_ptr = malloc(sizeof(ListNode **));
    *temp_list_ptr = list_temp;
    res.value = temp_list_ptr;

    break;
  }
  case JSON_TYPE_INT: {
    res.type.kind = TYPE_INT;
    res.value = allocator((TypeDescriptor){.kind = TYPE_INT, .list_inner = NULL, .ptr_count = 1, .obj_fields = NULL});
    *(int64_t *)res.value = value.num_int;
    break;
  }
  case JSON_TYPE_FLOAT: {
    res.type.kind = TYPE_FLOAT;
    res.value = allocator((TypeDescriptor){.kind = TYPE_FLOAT, .list_inner = NULL, .ptr_count = 1, .obj_fields = NULL});
    *(double *)res.value = value.num_float;
    break;
  }
  case JSON_TYPE_BOOL: {
    res.type.kind = TYPE_BOOL;
    // res.value = malloc(sizeof(bool));
    res.value = allocator((TypeDescriptor){.kind = TYPE_BOOL, .list_inner = NULL, .ptr_count = 1, .obj_fields = NULL});
    *(bool *)res.value = value.boolean;
    break;
  }
  case JSON_TYPE_STRING: {
    res.type.kind = TYPE_STRING;

    TypeDescriptor dynstring_type = {
        .ptr_count = 0,
        .kind = TYPE_STRING,
        .list_inner = NULL,
        .obj_fields = NULL,
    };

    // DynString **ptr_temp = malloc(sizeof(DynString *));
    TypeDescriptor dynstring_double_pointer_type = dynstring_type;
    dynstring_double_pointer_type.ptr_count = 1;
    DynString **ptr_temp = allocator(dynstring_double_pointer_type);

    // NOTE: must trace allocation since it was not aallocated using the built-in allocator.
    DynString *str = dynstring_from(value.string);
    if (trace_allocation != NULL)
      trace_allocation(str, dynstring_type, NULL);

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

AnyValue __hpi_internal_parse_json(DynString *input, void *(allocator)(), void(trace_allocation)(void *, TypeDescriptor, TypeDescriptor *)) {
  char *input_cstr = dynstring_as_cstr(input);

  NewJsonParserResult create_res = parser_new(input_cstr);
  JsonParser parser = create_res.parser;
  if (create_res.error != NULL) {
    printf("Runtime JSON parser creation error: `%s`\n", create_res.error);
    abort();
  }

  JsonParseResult parse_res = parse_json(&parser, allocator);
  if (parse_res.error != NULL) {
    printf("Runtime JSON parse error: `%s`\n", parse_res.error);
    abort();
  }

  parser_free(&parser);

  // Convert JSON value to anyvalue.
  AnyValue anyval = __hpi_internal_anyvalue_from_json(parse_res.value, allocator, trace_allocation);

  json_parse_result_free(parse_res);

  return anyval;
}

JsonValue __hpi_internal_json_value_from_void(TypeDescriptor type, void *value) {
  puts("JSON marshaling is currently not supported");
  abort();

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

      JsonValue converted = __hpi_internal_json_value_from_void(*type.list_inner, curr.value);
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

      JsonValue converted = __hpi_internal_json_value_from_void(*(TypeDescriptor *)type_res.value, value.value);

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

      JsonValue converted = __hpi_internal_json_value_from_void(value.type, value.value);
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
