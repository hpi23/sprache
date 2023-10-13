#include "./format.h"
#include <stdbool.h>

void formatter_next(Formatter *format);
void formatter_start_escape(Formatter *format);

Formatter *formatter_new(char *fmt, Vec *input_args) {
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
    switch (fmt->curr_char) {
    case '%':
      formatter_start_escape(fmt);
      break;
    default: {
      formatter_next(fmt);
      dynstring_push_char(fmt->output_buf, fmt->curr_char);
    }
    }
  }

  return fmt->output_buf;
}

void formatter_next(Formatter *fmt) {
  fmt->curr_char = fmt->fmt[fmt->fmt_idx++];
}

void formatter_process_specifier(Formatter *fmt, ssize_t padding) {
  if (fmt->curr_char == '\0' || fmt->curr_char == -1) {
    // TODO: error, no format specification
    return;
  }

  ListGetResult arg = list_at(fmt->input_args, fmt->input_args_pos++);

  switch (fmt->curr_char) {
  case 'd':
    assert(arg.found);

    // TODO: format each argument depending on its type

    break;
  case 'f':
    assert(arg.found);
    break;
  case 't':
    assert(arg.found);
    break;
  case 's':
    assert(arg.found);
    break;
  case 'v':
    assert(arg.found);
    break;
  default: {
    if (arg.found) {
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

  // check if in range '0' ..= '9'
  if (fmt->curr_char == '\0' || fmt->curr_char == -1) {
    // TODO: throw error
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

    formatter_process_specifier(fmt, padding_res.num);
  }
}
