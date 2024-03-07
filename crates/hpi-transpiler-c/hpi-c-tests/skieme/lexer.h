#pragma once
#include "../dynstring/dynstring.h"
#include "./span.h"
#include <sys/types.h>

typedef struct {
  Loc loc;
  char *input;
  ListNode *output;
} Lexer;

Lexer *lexer_new(char *input);
void lexer_scan(Lexer *self);
