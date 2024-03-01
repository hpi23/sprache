#include "./libTime.h"
#include "../hpi-c-tests/hashmap/map.h"
#include "reflection.h"
#include <stdint.h>
#include <time.h>

TimeStruct current;

TimeStruct __hpi_internal_time_provider() {
  int64_t raw_time = time(NULL);
  struct tm *local = localtime(&raw_time);

  TimeStruct res = {
      .year = local->tm_year + 1900,
      .month = local->tm_mon + 1,
      .calendar_day = local->tm_mday,
      .week_day = local->tm_wday,
      .hour = local->tm_hour,
      .minute = local->tm_min,
      .second = local->tm_sec,
  };

  return res;
}

// Construcs a runtime object from the time data
HashMap *__hpi_internal_time() {
  current = __hpi_internal_time_provider();

  // TODO: how to handle this better, meaning correct garbage collection?
  // FIX: rewrite this function in HPI lang for fun and profit

  HashMap *map = hashmap_new();

  hashmap_insert(map, "Jahr", &current.year); // TODO: fix year
  hashmap_insert(map, "Monat", &current.month);
  hashmap_insert(map, "Kalendar_Tag", &current.calendar_day);
  hashmap_insert(map, "Wochentag", &current.week_day);
  hashmap_insert(map, "Stunde", &current.hour);
  hashmap_insert(map, "Minute", &current.minute);
  hashmap_insert(map, "Sekunde", &current.second);

  return map;
};
