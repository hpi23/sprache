#pragma once
#include <stdbool.h>
#include <sys/types.h>

typedef struct {
  uint line;
  uint column;
  uint index;
} Loc;

void loc_next(Loc *self, bool newline);
Loc loc_default();
