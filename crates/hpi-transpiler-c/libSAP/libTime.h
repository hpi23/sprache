#pragma once
#include "hashmap/map.h"
#include "reflection.h"
#include <stdint.h>

typedef struct {
  int16_t year;
  int8_t month;
  int8_t calendar_day;
  int8_t week_day;
  int8_t hour;
  int8_t minute;
  int8_t second;
  int64_t unix_time;
} TimeStruct;

TimeStruct __hpi_internal_time_provider();
HashMap *__hpi_internal_time(void(tracer)(void *addr, TypeDescriptor type, TypeDescriptor *type_heap));
