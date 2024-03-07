#include "./span.h"
#include <stdbool.h>

void loc_next(Loc *self, bool newline) {
  self->index++;
  if (newline) {
    self->line++;
    self->column = 1;
    return;
  }

  self->column++;
}

Loc loc_default() {
  Loc l = {.column = 1, .line = 1, .index = 0};
  return l;
}
