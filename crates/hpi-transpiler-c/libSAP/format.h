#include "./reflection.h"
#include "../hpi-c-tests/dynstring/dynstring.h"
#include "../hpi-c-tests/vec/vec.h"

typedef struct {
  TypeDescriptor type;
  void *value;
} FmtArg;

typedef struct {
  char *fmt;
  ssize_t fmt_idx;
  ListNode *input_args; // inner type: FmtArg
  ssize_t input_args_pos;
  char curr_char;
  DynString *output_buf;
} Formatter;

Formatter *formatter_new(char *fmt, ListNode *input_args);
DynString *formatter_fmt(Formatter *formatter);
