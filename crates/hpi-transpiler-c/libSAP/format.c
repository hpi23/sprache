#include "./format.h"
#include "./to_string.h"
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

void formatter_next(Formatter *format);
void formatter_start_escape(Formatter *format);

Formatter *formatter_new(char *fmt, ListNode *input_args) {
  Formatter *formatter = malloc(sizeof(Formatter));

  formatter->fmt = fmt;
  formatter->fmt_idx = 0;
  formatter->curr_char = -1;
  formatter->input_args = input_args;
  formatter->input_args_pos = 0;
  formatter->output_buf = dynstring_new();

  formatter_next(formatter);

  return formatter;
}

DynString *formatter_fmt(Formatter *fmt) {
  while (fmt->curr_char != 0 && fmt->curr_char != -1) {
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
  if (strlen(fmt->fmt) <= fmt->fmt_idx) {
    fmt->curr_char = -1;
    return;
  }
  fmt->curr_char = fmt->fmt[fmt->fmt_idx++];
}

void formatter_process_specifier(Formatter *fmt, ssize_t padding) {
  if (fmt->curr_char <= 1) {
    // TODO: error, no format specification
    assert(1);
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

    DynString *fmt_specifier = dynstring_from("%.xf");

    DynString *what;
    DynString *with;
    if (padding >= 0) {
      what = dynstring_from("x");
      with = dynstring_new();
      dynstring_push_fmt(with, "%ld", padding);
    } else {
      what = dynstring_from(".x");
      with = dynstring_new();
    }

    dynstring_replace(fmt_specifier, what, with);
    dynstring_free(what);
    dynstring_free(with);

    char *format_c_str = dynstring_as_cstr(fmt_specifier);

    dynstring_push_fmt(fmt->output_buf, format_c_str, *(double *)arg.value);
    free(format_c_str);

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
    dynstring_push(fmt->output_buf, *(DynString **)arg.value);
    break;
  case 'v':
    // TODO: Does this work?
    dynstring_push(fmt->output_buf, to_string(arg.type, arg.value));
    break;
  default: {
    if (false) {
      // TODO: error: illegal combination
      assert(0);
    } else {
      // TODO: error: missing arg for specifier
      assert(0);
    }
  }
  }

  formatter_next(fmt);
}

bool is_ascii_digit(char c) { return c >= 0x30 && c <= 0x39; }

void formatter_start_escape(Formatter *fmt) {
  formatter_next(fmt);

  ssize_t num_padding = -1;

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
      assert(0);
    }

    num_padding = padding_res.num;
  } else if (fmt->curr_char == '.') {
    formatter_next(fmt);

    DynString *padding = dynstring_new();

    while (is_ascii_digit(fmt->curr_char)) {
      dynstring_push_char(padding, fmt->curr_char);
      formatter_next(fmt);
    }

    DynStringParseInt padding_res = dynstring_parse_int64(padding);
    if (padding_res.error != NULL) {
      printf("Formatierungsfehler: Konnte Pufferung nicht verarbeiten: %s\n",
             padding_res.error);
      assert(0);
    }

    num_padding = padding_res.num;
  }

  formatter_process_specifier(fmt, num_padding);
}
