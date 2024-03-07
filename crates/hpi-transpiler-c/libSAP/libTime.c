#include "./libTime.h"
#include "hashmap/map.h"
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
HashMap *__hpi_internal_time(void(tracer)(void *addr, TypeDescriptor type, TypeDescriptor *type_heap)) {
  current = __hpi_internal_time_provider();

  //
  // BEGIN auto generated code
  //

  // Type descriptor `Zeichenkette`
  TypeDescriptor type_descriptor_Zeichenkette;
  TypeDescriptor type_descriptor_Objekt_BEGIN__END;
  TypeDescriptor type_descriptor_Zeiger_auf_Zahl;
  TypeDescriptor
      type_descriptor_Objekt_BEGIN_Zahl_Jahr_DELIM_Zahl_Monat_DELIM_Zahl_Kalendar_Tag_DELIM_Zahl_Wochentag_DELIM_Zahl_Stunde_DELIM_Zahl_Minute_DELIM_Zahl_Sekunde_END;
  TypeDescriptor type_descriptor_Zahl;
  type_descriptor_Zeichenkette.kind = TYPE_STRING;
  type_descriptor_Zeichenkette.ptr_count = 0;
  type_descriptor_Zeichenkette.list_inner = NULL;
  // Type descriptor `Objekt_BEGIN__END`
  type_descriptor_Objekt_BEGIN__END.kind = TYPE_OBJECT;
  type_descriptor_Objekt_BEGIN__END.ptr_count = 0;
  type_descriptor_Objekt_BEGIN__END.list_inner = NULL;
  type_descriptor_Objekt_BEGIN__END.obj_fields = hashmap_new();
  // Type descriptor `Zeiger_auf_Zahl`
  type_descriptor_Zeiger_auf_Zahl.kind = TYPE_INT;
  type_descriptor_Zeiger_auf_Zahl.ptr_count = 1;
  type_descriptor_Zeiger_auf_Zahl.list_inner = NULL;
  // Type descriptor
  // `Objekt_BEGIN_Zahl_Jahr_DELIM_Zahl_Monat_DELIM_Zahl_Kalendar_Tag_DELIM_Zahl_Wochentag_DELIM_Zahl_Stunde_DELIM_Zahl_Minute_DELIM_Zahl_Sekunde_END`
  type_descriptor_Objekt_BEGIN_Zahl_Jahr_DELIM_Zahl_Monat_DELIM_Zahl_Kalendar_Tag_DELIM_Zahl_Wochentag_DELIM_Zahl_Stunde_DELIM_Zahl_Minute_DELIM_Zahl_Sekunde_END
      .kind = TYPE_OBJECT;
  type_descriptor_Objekt_BEGIN_Zahl_Jahr_DELIM_Zahl_Monat_DELIM_Zahl_Kalendar_Tag_DELIM_Zahl_Wochentag_DELIM_Zahl_Stunde_DELIM_Zahl_Minute_DELIM_Zahl_Sekunde_END
      .ptr_count = 0;
  type_descriptor_Objekt_BEGIN_Zahl_Jahr_DELIM_Zahl_Monat_DELIM_Zahl_Kalendar_Tag_DELIM_Zahl_Wochentag_DELIM_Zahl_Stunde_DELIM_Zahl_Minute_DELIM_Zahl_Sekunde_END
      .list_inner = NULL;
  type_descriptor_Objekt_BEGIN_Zahl_Jahr_DELIM_Zahl_Monat_DELIM_Zahl_Kalendar_Tag_DELIM_Zahl_Wochentag_DELIM_Zahl_Stunde_DELIM_Zahl_Minute_DELIM_Zahl_Sekunde_END
      .obj_fields = hashmap_new();
  // Type descriptor `Zahl`
  type_descriptor_Zahl.kind = TYPE_INT;
  type_descriptor_Zahl.ptr_count = 0;
  type_descriptor_Zahl.list_inner = NULL;
  hashmap_insert(
      type_descriptor_Objekt_BEGIN_Zahl_Jahr_DELIM_Zahl_Monat_DELIM_Zahl_Kalendar_Tag_DELIM_Zahl_Wochentag_DELIM_Zahl_Stunde_DELIM_Zahl_Minute_DELIM_Zahl_Sekunde_END
          .obj_fields,
      "Jahr", &type_descriptor_Zahl);
  hashmap_insert(
      type_descriptor_Objekt_BEGIN_Zahl_Jahr_DELIM_Zahl_Monat_DELIM_Zahl_Kalendar_Tag_DELIM_Zahl_Wochentag_DELIM_Zahl_Stunde_DELIM_Zahl_Minute_DELIM_Zahl_Sekunde_END
          .obj_fields,
      "Monat", &type_descriptor_Zahl);
  hashmap_insert(
      type_descriptor_Objekt_BEGIN_Zahl_Jahr_DELIM_Zahl_Monat_DELIM_Zahl_Kalendar_Tag_DELIM_Zahl_Wochentag_DELIM_Zahl_Stunde_DELIM_Zahl_Minute_DELIM_Zahl_Sekunde_END
          .obj_fields,
      "Kalendar_Tag", &type_descriptor_Zahl);
  hashmap_insert(
      type_descriptor_Objekt_BEGIN_Zahl_Jahr_DELIM_Zahl_Monat_DELIM_Zahl_Kalendar_Tag_DELIM_Zahl_Wochentag_DELIM_Zahl_Stunde_DELIM_Zahl_Minute_DELIM_Zahl_Sekunde_END
          .obj_fields,
      "Wochentag", &type_descriptor_Zahl);
  hashmap_insert(
      type_descriptor_Objekt_BEGIN_Zahl_Jahr_DELIM_Zahl_Monat_DELIM_Zahl_Kalendar_Tag_DELIM_Zahl_Wochentag_DELIM_Zahl_Stunde_DELIM_Zahl_Minute_DELIM_Zahl_Sekunde_END
          .obj_fields,
      "Stunde", &type_descriptor_Zahl);
  hashmap_insert(
      type_descriptor_Objekt_BEGIN_Zahl_Jahr_DELIM_Zahl_Monat_DELIM_Zahl_Kalendar_Tag_DELIM_Zahl_Wochentag_DELIM_Zahl_Stunde_DELIM_Zahl_Minute_DELIM_Zahl_Sekunde_END
          .obj_fields,
      "Minute", &type_descriptor_Zahl);
  hashmap_insert(
      type_descriptor_Objekt_BEGIN_Zahl_Jahr_DELIM_Zahl_Monat_DELIM_Zahl_Kalendar_Tag_DELIM_Zahl_Wochentag_DELIM_Zahl_Stunde_DELIM_Zahl_Minute_DELIM_Zahl_Sekunde_END
          .obj_fields,
      "Sekunde", &type_descriptor_Zahl);

  //
  // END auto generated code
  //

  TypeDescriptor *obj_heap = malloc(sizeof(TypeDescriptor));
  *obj_heap =
      type_descriptor_Objekt_BEGIN_Zahl_Jahr_DELIM_Zahl_Monat_DELIM_Zahl_Kalendar_Tag_DELIM_Zahl_Wochentag_DELIM_Zahl_Stunde_DELIM_Zahl_Minute_DELIM_Zahl_Sekunde_END;

  HashMap *map = hashmap_new();
  if (tracer != NULL)
    tracer(
        map,
        type_descriptor_Objekt_BEGIN_Zahl_Jahr_DELIM_Zahl_Monat_DELIM_Zahl_Kalendar_Tag_DELIM_Zahl_Wochentag_DELIM_Zahl_Stunde_DELIM_Zahl_Minute_DELIM_Zahl_Sekunde_END,
        obj_heap);

  // TODO: also put the fields on the heap!
  hashmap_insert(map, "Jahr", &current.year); // TODO: fix year
  hashmap_insert(map, "Monat", &current.month);
  hashmap_insert(map, "Kalendar_Tag", &current.calendar_day);
  hashmap_insert(map, "Wochentag", &current.week_day);
  hashmap_insert(map, "Stunde", &current.hour);
  hashmap_insert(map, "Minute", &current.minute);
  hashmap_insert(map, "Sekunde", &current.second);

  return map;
};
