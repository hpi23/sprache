#include "./interpreter.h"
#include "./lexer.h"
#include <stdio.h>

Interpreter *interpreter_new(ListNode *instruction_list) {
  ssize_t *cells = (ssize_t *)malloc(sizeof(ssize_t) * NUM_CELLS);

  Interpreter interpreter = {.instruction_list = instruction_list,
                             .cells = cells,
                             .memory_pointer = 0};
  Interpreter *interpreter_temp = malloc(sizeof(Interpreter));
  return interpreter_temp;
}

TokenKind fetch_instruction(Interpreter *interpreter) {
  TokenKind *tokenkind = (TokenKind *)interpreter->instruction_list->value;
  if (interpreter->instruction_list != NULL) {
    interpreter->instruction_list = interpreter->instruction_list->next;
  }
  return *tokenkind;
}

InterpreterExitCode interpreter_execute(Interpreter *interpreter) {
  TokenKind tokenkind = fetch_instruction(interpreter);

  while (tokenkind != TOKENKIND_EOF) {
    switch (tokenkind) {
    case TOKENKIND_GREATER_THAN:
        interpreter->memory_pointer++;
        if (interpreter->memory_pointer >= NUM_CELLS) {
            return EXITCODE_ERROR_ILLEGAL_MEMORY_POINTER;
        }
        break;
    case TOKENKIND_LESS_THAN:
        interpreter->memory_pointer--;
        if (interpreter->memory_pointer < 0) {
            return EXITCODE_ERROR_ILLEGAL_MEMORY_POINTER;
        }
        break;
    case TOKENKIND_PLUS:
        interpreter->cells[interpreter->memory_pointer]++;
        break;
    case TOKENKIND_MINUS:
        interpreter->cells[interpreter->memory_pointer]--;
        break;
    case TOKENKIND_LBRACKET: // begin loop
    case TOKENKIND_RBRACKET:
    case TOKENKIND_COMMA:
    case TOKENKIND_DOT:
    case TOKENKIND_EOF:
      goto after_loop;
    }

    tokenkind = fetch_instruction(interpreter);
  }

after_loop:
  printf("Interpreter has ended\n");
  return EXITCODE_SUCCESS;
}
