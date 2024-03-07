#include "./reflection.h"
#include "dynstring/dynstring.h"
#include "hashmap/map.h"
#include "list/list.h"
#include <assert.h>
#include <sys/types.h>

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
    assert(type.list_inner != NULL);
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
//

void free_type(TypeDescriptor *type) {
  assert(type != NULL);
  switch (type->kind) {
  case TYPE_NONE:
  case TYPE_INT:
  case TYPE_FLOAT:
  case TYPE_CHAR:
  case TYPE_BOOL:
  case TYPE_STRING:
  case TYPE_ANY_OBJECT:
  case TYPE_ANY_VALUE:
    break;
  case TYPE_LIST:
    free_type(type->list_inner);
    break;
  case TYPE_OBJECT: {
    ListNode *keys = hashmap_keys(type->obj_fields);
    int key_len = list_len(keys);
    for (int i = 0; i < key_len; i++) {
      ListGetResult curr_res = list_at(keys, i);
      assert(curr_res.found);

      TypeDescriptor *curr = (TypeDescriptor *)curr_res.value;
      // free_type(curr);
    }

    list_free(keys);
    hashmap_free(type->obj_fields);
    break;
  }
  default: {
    char *type_str = display_type(*type);
    printf("free_type(): (addr = %p) Unsupported type: %s (%d; %ld)\n", type, type_str, type->kind, type->ptr_count);
    free(type_str);
    abort();
  }
  }

  free(type);
}

TypeDescriptor clone_type(TypeDescriptor in) {
  switch (in.kind) {
  case TYPE_NONE:
  case TYPE_INT:
  case TYPE_FLOAT:
  case TYPE_CHAR:
  case TYPE_BOOL:
  case TYPE_STRING:
  case TYPE_ANY_OBJECT:
  case TYPE_ANY_VALUE:
    return in;
  case TYPE_LIST: {
    TypeDescriptor new = {.kind = in.kind, .ptr_count = in.ptr_count, .list_inner = NULL, .obj_fields = NULL};
    new.list_inner = malloc(sizeof(TypeDescriptor));
    *new.list_inner = clone_type(*in.list_inner);
    return new;
  }
  case TYPE_OBJECT: {
    TypeDescriptor new = {.kind = in.kind, .ptr_count = in.ptr_count, .list_inner = NULL, .obj_fields = NULL};
    new.obj_fields = hashmap_new();

    ListNode *keys = hashmap_keys(in.obj_fields);
    uint key_len = list_len(keys);

    for (int i = 0; i < key_len; i++) {
      ListGetResult key_res = list_at(keys, i);
      assert(key_res.found);

      MapGetResult value_res = hashmap_get(new.obj_fields, key_res.value);
      assert(value_res.found);

      TypeDescriptor *field_heap = malloc(sizeof(TypeDescriptor));
      *field_heap = clone_type(*(TypeDescriptor *)value_res.value);
      hashmap_insert(new.obj_fields, key_res.value, field_heap);
    }

    *new.list_inner = clone_type(*in.list_inner);
    return new;
  }
  }
}
