#include "../list/list.h"
#include <stdio.h>

#define NUM_CELLS 30000

typedef struct {
  ListNode *instruction_list;
  ssize_t *cells;
  ssize_t memory_pointer;
} Interpreter;

typedef enum {
  EXITCODE_ERROR_ILLEGAL_MEMORY_POINTER,
  EXITCODE_SUCCESS
} InterpreterExitCode;

Interpreter *interpreter_new(ListNode *instruction_list);
InterpreterExitCode interpreter_execute(Interpreter *interpreter);
