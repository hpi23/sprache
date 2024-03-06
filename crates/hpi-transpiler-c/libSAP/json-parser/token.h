#pragma once

typedef enum {
  TOKENKIND_LBRACE,
  TOKENKIND_RBRACE,
  TOKENKIND_LBRACKET,
  TOKENKIND_RBRACKET,
  TOKENKIND_STRING,
  TOKENKIND_BOOL,
  TOKENKIND_NULL,
  TOKENKIND_INT,
  TOKENKIND_FLOAT,
  TOKENKIND_COLON,
  TOKENKIND_COMMA,
  TOKENKIND_EOF
} TokenKind;

typedef struct {
  TokenKind kind;
  char *value;
} Token;

char *display_tokenkind(TokenKind kind);
Token token_new_eof();
