#include "./token.h"
#include <stdio.h>
#include <stdlib.h>

char *display_tokenkind(TokenKind kind) {
  switch (kind) {
  case TOKENKIND_NULL:
    return "NULL";
  case TOKENKIND_LBRACE:
    return "{";
  case TOKENKIND_RBRACE:
    return "}";
  case TOKENKIND_EOF:
    return "EOF";
  case TOKENKIND_LBRACKET:
    return "[";
  case TOKENKIND_RBRACKET:
    return "]";
  case TOKENKIND_STRING:
    return "STRING";
  case TOKENKIND_BOOL:
    return "BOOL";
  case TOKENKIND_COLON:
    return ":";
  case TOKENKIND_COMMA:
    return ",";
  case TOKENKIND_INT:
    return "INT";
  case TOKENKIND_FLOAT:
    return "FLOAT";
  }
  printf("Unreachable: Illegal tokenkind\n");
  exit(1);
}

Token token_new_eof() {
  Token token = {.value = "EOF", .kind = TOKENKIND_EOF};
  return token;
}
