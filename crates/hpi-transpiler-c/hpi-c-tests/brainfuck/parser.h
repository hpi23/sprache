#include <stdio.h>
#include "./lexer.h"

typedef enum {
  INSTRUCTION_MP_DECREASE,
  INSTRUCTION_MP_INCREASE,
  INSTRUCTION_VALUE_INCREASE,
  INSTRUCTION_VALUE_DECREASE,
  INSTRUCTION_GETCHAR,
  INSTRUCTION_PUTCHAR,
} Instruction;

typedef struct {
  ssize_t len;
  Instruction *instructions;
} InstructionList;

typedef struct {
  Instruction instruction;
  InstructionList *loop;
} ProgramSegment;

// Parser

typedef struct {
    InstructionList instructions;
    char * error;
} ParseResult ;

typedef struct {
    Lexer * lexer;
    TokenKind curr_tok;
} Parser;

Parser * parser_new(char * input);
ParseResult parser_parse(Parser * parser);
