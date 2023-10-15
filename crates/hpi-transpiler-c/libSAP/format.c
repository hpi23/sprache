#include "./format.h"
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

void formatter_next(Formatter *format);
void formatter_start_escape(Formatter *format);

Formatter *formatter_new(char *fmt, ListNode *input_args) {
  Formatter *formatter = malloc(sizeof(Formatter));

  formatter->fmt = fmt;
  formatter->curr_char = -1;
  formatter->input_args = input_args;
  formatter->input_args_pos = 0;
  formatter->output_buf = dynstring_new();

  return formatter;
}

DynString *formatter_fmt(Formatter *fmt) {
  while (fmt->curr_char) {
    if (fmt->curr_char == '%') {
      formatter_start_escape(fmt);
      continue;
    }
    dynstring_push_char(fmt->output_buf, fmt->curr_char);
    formatter_next(fmt);
  }

  return fmt->output_buf;
}

void formatter_next(Formatter *fmt) {
  fmt->curr_char = fmt->fmt[fmt->fmt_idx++];
}

// void __hpi_internal_drucke(ssize_t num_args, ...) {
//   va_list args;
//
//   va_start(args, num_args);
//
//   for (int i = 0; i < num_args; i++) {
//     TypeDescriptor type = va_arg(args, TypeDescriptor);
//
//     switch (type.kind) {
//     case TYPE_INT: {
//       int64_t *number = va_arg(args, int64_t *);
//       printf("%ld", *number);
//       break;
//     }
//     case TYPE_FLOAT: {
//       double *number = va_arg(args, double *);
//       printf("%f", *number);
//       break;
//     }
//     case TYPE_CHAR: {
//       int *character = va_arg(args, int *);
//       printf("%c", *character);
//       break;
//     }
//     case TYPE_BOOL: {
//       int *bool_ = va_arg(args, int *);
//       if (*bool_) {
//         printf("%s", "true");
//       } else {
//         printf("%s", "false");
//       }
//       break;
//     }
//     case TYPE_LIST: {
//       ListNode **list_ptr = va_arg(args, ListNode **);
//       ListNode *list = *list_ptr;
//
//       printf("[");
//
//       while (list != NULL) {
//         TypeDescriptor new_type = {.kind = type.list_inner->kind,
//                                    .list_inner = type.list_inner->list_inner,
//                                    .ptr_count = 0};
//
//         bool old_newline = newline;
//         newline = false;
//         __hpi_internal_drucke(1, new_type, list->value);
//         newline = old_newline;
//
//         list = list->next;
//         if (list != NULL) {
//           printf(", ");
//         }
//       }
//
//       printf("]");
//
//       break;
//     }
//     case TYPE_OBJECT: {
//       HashMap **map_ptr = va_arg(args, HashMap **);
//       HashMap *map = *map_ptr;
//
//       ListNode *keys = hashmap_keys(map);
//
//       printf("Objekt {\n");
//
//       while (keys != NULL) {
//         char *key = keys->value;
//         MapGetResult res = hashmap_get(map, key);
//         assert(res.found);
//
//         MapGetResult type_res = hashmap_get(type.obj_fields, key);
//         assert(type_res.found);
//
//         TypeDescriptor type_descriptor = *(TypeDescriptor *)type_res.value;
//
//         for (int i = 0; i < indent; i++) {
//           printf(" ");
//         }
//
//         printf("%s: ", key);
//         bool old_newline = newline;
//         newline = false;
//         indent += 4;
//         __hpi_internal_drucke(1, type_descriptor, res.value);
//         indent -= 4;
//         newline = old_newline;
//
//         if (keys->next != NULL) {
//           printf(",");
//           printf("\n");
//         }
//
//         keys = keys->next;
//       }
//
//       printf("\n");
//       for (int i = 0; i < indent - 4; i++) {
//         printf(" ");
//       }
//
//       printf("}");
//
//       break;
//     }
//     case TYPE_STRING: {
//       DynString **string = va_arg(args, DynString **);
//       char *string_raw = dynstring_as_cstr(*string);
//
//       printf("%s", string_raw);
//
//       free(string_raw);
//       break;
//     }
//     }
//
//     if (i < num_args && num_args > 1) {
//       printf(" ");
//     }
//   }
//
//   va_end(args);
//   if (newline) {
//     printf("\n");
//   }
// }

void formatter_process_specifier(Formatter *fmt, ssize_t padding) {

  if (fmt->curr_char == '\0' || fmt->curr_char == -1) {
    // TODO: error, no format specification
    return;
  }

  ListGetResult arg_temp = list_at(fmt->input_args, fmt->input_args_pos++);
  assert(arg_temp.found);
  FmtArg arg = *(FmtArg *)arg_temp.value;

  switch (fmt->curr_char) {
  case 'd': {
    assert(arg.type.kind == TYPE_INT);
    assert(arg.type.ptr_count == 0);

    // TODO: format each argument depending on its type
    dynstring_push_fmt(fmt->output_buf, "%ld", *(int64_t *)arg.value);

    break;
  }
  case 'f': {
    assert(arg.type.kind == TYPE_FLOAT);
    assert(arg.type.ptr_count == 0);

    dynstring_push_fmt(fmt->output_buf, "%f", *(double *)arg.value);

    break;
  }
  case 't':
    assert(arg.type.kind == TYPE_BOOL);
    assert(arg.type.ptr_count == 0);

    if (*(bool *)arg.value) {
      dynstring_push_string(fmt->output_buf, "ja");
    } else {
      dynstring_push_string(fmt->output_buf, "nein");
    }

    break;
  case 's':
    assert(arg.type.kind == TYPE_STRING);
    assert(arg.type.ptr_count == 0);
    dynstring_push_string(fmt->output_buf, *(char **)arg.value);
    break;
  case 'v':
    // TODO: support this
    break;
  default: {
    if (false) {
      // TODO: error: illegal combination
    } else {
      // TODO: error: missing arg for specifier
    }
  }
  }

  formatter_next(fmt);
}

bool is_ascii_digit(char c) { return c >= 0x30 && c <= 0x39; }

void formatter_start_escape(Formatter *fmt) {
  formatter_next(fmt);

  ssize_t num_padding = 0;

  // check if in range '0' ..= '9'
  if (fmt->curr_char == '\0' || fmt->curr_char == -1) {
    // TODO: throw error
    assert(0);
  } else if (is_ascii_digit(fmt->curr_char)) {
    DynString *padding = dynstring_new();

    while (is_ascii_digit(fmt->curr_char)) {
      dynstring_push_char(padding, fmt->curr_char);
      formatter_next(fmt);
    }

    DynStringParseInt padding_res = dynstring_parse_int64(padding);
    if (padding_res.error != NULL) {
      printf("Formatierungsfehler: Konnte Pufferung nicht verarbeiten: %s\n",
             padding_res.error);
      exit(1);
    }

    num_padding = padding_res.num;
  }

  formatter_process_specifier(fmt, num_padding);
}
