#pragma once
#include "../list/list.h"
#include "./token.h"

typedef struct {
  Token curr;
  Token next;
  ListNode *node_next;
  ListNode *node_start;
} Parser;

#define PARSER_AS_TOKEN(from) *(Token *)from

void parser_next(Parser *next);
Parser *parser_new(ListNode *input);
void parser_free(Parser *self);
