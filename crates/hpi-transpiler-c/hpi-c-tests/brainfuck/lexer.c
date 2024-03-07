#include "./lexer.h"
#include <stdio.h>
#include <stdlib.h>

Lexer *lexer_new(char *input) {
  Lexer lexer = {.input = input, .curr_char = -1, .curr_index = 0};
  Lexer *result = malloc(sizeof(Lexer));
  *result = lexer;
  return result;
}

void lexer_free(Lexer *lexer) {
  free(lexer->input);
  free(lexer);
}

TokenResult lexer_next(Lexer *lexer) {
  TokenKind token = TOKENKIND_EOF;
  TokenResult result = {.error = NULL, .token = token};

  switch (lexer->curr_char) {
  case '>': {
    result.token = TOKENKIND_GREATER_THAN;
    break;
  }
  case '<': {
    result.token = TOKENKIND_LESS_THAN;
    break;
  }
  case '+': {
    result.token = TOKENKIND_PLUS;
    break;
  }
  case '-': {
    result.token = TOKENKIND_MINUS;
    break;
  }
  case '[': {
    result.token = TOKENKIND_LBRACKET;
    break;
  }
  case ']': {
    result.token = TOKENKIND_RBRACKET;
    break;
  }
  case ',': {
    result.token = TOKENKIND_COMMA;
    break;
  }
  case '.': {
    result.token = TOKENKIND_DOT;
    break;
  default:
    asprintf(&result.error, "Error: Illegal character: `%c`", lexer->curr_char);
    return result;
  }
  }

  lexer->curr_index++;
  lexer->curr_char = lexer->input[lexer->curr_char];

  return result;
}
