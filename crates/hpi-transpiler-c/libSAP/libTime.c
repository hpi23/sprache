#include "./libTime.h"
#include "hashmap/map.h"
#include "reflection.h"
#include <stdint.h>
#include <stdio.h>
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
// HashMap *__hpi_internal_time(void(tracer)(void *addr, TypeDescriptor type, TypeDescriptor *type_heap)) {
//   current = __hpi_internal_time_provider();
//
//   TypeDescriptor *obj = malloc(sizeof(TypeDescriptor));
//   obj->kind = TYPE_OBJECT;
//   obj->ptr_count = 0;
//   obj->list_inner = NULL;
//   obj->obj_fields = hashmap_new();
//
//   const int len = 7;
//   char *keys[7] = {"Jahr", "Monat", "Kalendar_Tag", "Wochentag", "Stunde", "Minute", "Sekunde"};
//   int64_t *ptrs[7] = {NULL, NULL, NULL, NULL, NULL, NULL, NULL};
//
//   HashMap *map = hashmap_new();
//
//   for (int i = 0; i < len; i++) {
//     printf("inserting time %s...\n", keys[i]);
//
//     int64_t *ptr = malloc(sizeof(int64_t));
//     switch (i) {
//     case 0:
//       *ptr = current.year;
//       break;
//     case 1:
//       *ptr = current.month;
//       break;
//     case 2:
//       *ptr = current.calendar_day;
//       break;
//     case 3:
//       *ptr = current.week_day;
//       break;
//     case 4:
//       *ptr = current.hour;
//       break;
//     case 5:
//       *ptr = current.minute;
//       break;
//     case 6:
//       *ptr = current.second;
//       break;
//     }
//
//     hashmap_insert(map, keys[i], ptr);
//
//     TypeDescriptor *type_descriptor_Zahl = malloc(sizeof(TypeDescriptor));
//     type_descriptor_Zahl->kind = TYPE_INT;
//     type_descriptor_Zahl->ptr_count = 1;
//     type_descriptor_Zahl->list_inner = NULL;
//     type_descriptor_Zahl->obj_fields = NULL;
//
//     hashmap_insert(obj->obj_fields, keys[i], type_descriptor_Zahl);
//
//     if (tracer != NULL) {
//       tracer(ptrs[i], *type_descriptor_Zahl, NULL);
//     }
//   }
//
//   if (tracer != NULL) {
//     tracer(map, *obj, obj);
//   }
//
//   return map;
// };
//
HashMap *__hpi_internal_time(void(tracer)(void *addr, TypeDescriptor type, TypeDescriptor *type_heap)) {
  current = __hpi_internal_time_provider();

  TypeDescriptor *obj = malloc(sizeof(TypeDescriptor));
  obj->kind = TYPE_OBJECT;
  obj->ptr_count = 0;
  obj->list_inner = NULL;
  obj->obj_fields = hashmap_new();

#define len 7
  char *keys[len] = {"Sekunde", "Minute", "Stunde", "Wochentag", "Kalendar_Tag", "Monat", "Jahr"};

  HashMap *map = hashmap_new();

  for (int i = 0; i < len; i++) {
    int64_t *ptr = malloc(sizeof(int64_t));
    switch (i) {
    case 0:
      *ptr = current.second;
      break;
    case 1:
      *ptr = current.month;
      break;
    case 2:
      *ptr = current.calendar_day;
      break;
    case 3:
      *ptr = current.week_day;
      break;
    case 4:
      *ptr = current.hour;
      break;
    case 5:
      *ptr = current.minute;
      break;
    case 6:
      *ptr = current.second;
      break;
    }
    hashmap_insert(map, keys[i], ptr);

    TypeDescriptor *type_descriptor_Zahl = malloc(sizeof(TypeDescriptor));
    *type_descriptor_Zahl = (TypeDescriptor){.kind = TYPE_INT, .ptr_count = 1, .list_inner = NULL, .obj_fields = NULL};
    hashmap_insert(obj->obj_fields, keys[i], type_descriptor_Zahl);

    if (tracer != NULL) {
      TypeDescriptor *type_descriptor_Zahl2 = malloc(sizeof(TypeDescriptor));
      type_descriptor_Zahl2->kind = TYPE_INT;
      type_descriptor_Zahl2->ptr_count = 1;
      type_descriptor_Zahl2->list_inner = NULL;
      type_descriptor_Zahl2->obj_fields = NULL;

      tracer(ptr, *type_descriptor_Zahl2, type_descriptor_Zahl2);
    }
  }

  if (tracer != NULL) {
    tracer(map, *obj, obj);
  }

  return map;
};
