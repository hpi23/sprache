#include "libFloat.h"
#include <math.h>
#include <stdint.h>

int64_t __hpi_internal_float_round_to_int_up(double self) { return (int64_t)round(self); }
int64_t __hpi_internal_float_round_to_int_down(double self) { return (int64_t)trunc(self); }
