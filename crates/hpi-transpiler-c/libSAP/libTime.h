#pragma once
#include "hashmap/map.h"
#include "reflection.h"

typedef struct {
    int64_t year;
    int64_t month;
    int64_t calendar_day;
    int64_t week_day;
    int64_t hour;
    int64_t minute;
    int64_t second;
} TimeStruct;

TimeStruct __hpi_internal_time_provider();
HashMap *__hpi_internal_time(void(tracer)(void *addr, TypeDescriptor type, TypeDescriptor *type_heap));
