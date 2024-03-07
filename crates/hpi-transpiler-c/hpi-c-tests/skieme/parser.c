#include "./parser.h"
#include <stdio.h>

void parser_next(Parser *self) {
    self->curr = self->next;
    if (self->node_next == NULL) {
        // TODO: do a free here
        self->next.kind = EOF;
        return;
    }
    self->node_next = self->node_next->value;
    self->next = PARSER_AS_TOKEN(self->node_next->value);
}

Parser *parser_new(ListNode *input) {
  Parser *parser = (Parser *)malloc(sizeof(Parser));
  parser->node_next = input;
  parser->node_start = input;
  parser->next = PARSER_AS_TOKEN(parser->node_next->value);

  parser_next(parser);
  parser_next(parser);

  return parser;
}

void parser_free(Parser *self) {
  list_free(self->node_start);
  free(self);
}

void parser_parse() {

}
