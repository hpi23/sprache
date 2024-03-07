#include <stdio.h>

typedef struct {
  char *input;
  ssize_t curr_index;
  char curr_char;
} Lexer;

Lexer *lexer_new(char *input);

typedef enum {
  TOKENKIND_GREATER_THAN,
  TOKENKIND_LESS_THAN,
  TOKENKIND_PLUS,
  TOKENKIND_MINUS,
  TOKENKIND_LBRACKET,
  TOKENKIND_RBRACKET,
  TOKENKIND_COMMA,
  TOKENKIND_DOT,
  TOKENKIND_EOF
} TokenKind;

typedef struct {
  char *error;
  TokenKind token;
} TokenResult;

TokenResult lexer_next(Lexer * lexer);
