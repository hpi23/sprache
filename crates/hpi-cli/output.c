#include "./libSAP/libJson.h"
#include "./libSAP/libGC.h"
#include "./libSAP/dynstring/dynstring.h"
#include "./libSAP/libList.h"
#include <assert.h>
#include <stdbool.h>
#include "./libSAP/libSAP.h"
#include "./libSAP/libHttp.h"
#include "./libSAP/libTime.h"
#include "./libSAP/libString.h"

// Type definitions for runtime 'reflection'
TypeDescriptor type_descriptor_Zeichenkette;
TypeDescriptor type_descriptor_Zahl;
TypeDescriptor type_descriptor_Liste_von_Objekt_BEGIN_Zeichenkette_Schlüssel_DELIM_Zeichenkette_Wert_END;
TypeDescriptor type_descriptor_Objekt_BEGIN_Zeichenkette_Schlüssel_DELIM_Zeichenkette_Wert_END;
TypeDescriptor type_descriptor_Objekt_BEGIN__END;
TypeDescriptor type_descriptor_Zeiger_auf_Zeichenkette;
TypeDescriptor type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_Schlüssel_DELIM_Zeichenkette_Wert_END;
TypeDescriptor type_descriptor_Speicherbox;
TypeDescriptor type_descriptor_Liste_von_Speicherbox;
TypeDescriptor type_descriptor_Liste_von_Unbekannt;
TypeDescriptor type_descriptor_Unbekannt;
TypeDescriptor type_descriptor_Fliesskommazahl;
TypeDescriptor type_descriptor_Zeiger_auf_Fliesskommazahl;
TypeDescriptor type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_ZeitSlot_DELIM_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END;
TypeDescriptor type_descriptor_Liste_von_Speise;
TypeDescriptor type_descriptor_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END;
TypeDescriptor type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END;
TypeDescriptor type_descriptor_Liste_von_Zeichenkette;

int64_t MENSA_FILMUNI0 = 9603;
int64_t MENSA_GRIEBNITZSEE1 = 9601;
DynString* MENSA_URL_GENERISCH3;
DynString* KEINE_DATEN5;
bool ZEIGE_MEHR_NACHRICHTEN6 = false;

ListNode* Lade_Speiseangebot0(int64_t);
void Aktualisiere1(int64_t);
void Gericht_Liste_Drucken2(ListNode*);
DynString* bewerbung();
void einschreibung(int64_t);
void studium();
void type_descriptor_setup();
void type_descriptor_teardown();
void global_variable_setup();
void cexit(int);
int main(int, char**);

ListNode* Lade_Speiseangebot0(int64_t Mensa_Ort7) {
    DynString* string_temp8 = dynstring_from("");
    gc_add_to_trace(string_temp8, type_descriptor_Zeichenkette, NULL);
    DynString* gc_root_temp10 = string_temp8;
    gc_add_root(gc_root_temp10, type_descriptor_Zeichenkette, "string_temp8");
    DynString* Körper9 = gc_root_temp10;
    int64_t fmt_ptr_11 = Mensa_Ort7;
    __hpi_internal_libSAP_reset();
    DynString* gc_root_temp13 = __hpi_internal_fmt(1, MENSA_URL_GENERISCH3, gc_add_to_trace, type_descriptor_Zahl, &fmt_ptr_11);
    gc_add_root(gc_root_temp13, type_descriptor_Zeichenkette, "__hpi_internal_fmt(1, MENSA_URL_GENERISCH3, gc_add_to_trace, type_descriptor_Zahl, &fmt_ptr_11)");
    DynString* URL12 = gc_root_temp13;
    DynString* string_temp14 = dynstring_from("GET");
    gc_add_to_trace(string_temp14, type_descriptor_Zeichenkette, NULL);
    DynString* string_temp15 = dynstring_from("");
    gc_add_to_trace(string_temp15, type_descriptor_Zeichenkette, NULL);
    ListNode* list_temp16 = gc_alloc(type_descriptor_Liste_von_Objekt_BEGIN_Zeichenkette_Schlüssel_DELIM_Zeichenkette_Wert_END);
    HashMap* object_temp17 = gc_alloc(type_descriptor_Objekt_BEGIN__END);
    DynString* string_temp19 = dynstring_from("Referer");
    gc_add_to_trace(string_temp19, type_descriptor_Zeichenkette, NULL);
    DynString** object_member_Schlüssel_n18 = gc_alloc(type_descriptor_Zeiger_auf_Zeichenkette);
    *object_member_Schlüssel_n18 = string_temp19;
    hashmap_insert(object_temp17, "Schlüssel", object_member_Schlüssel_n18);
    DynString* string_temp22 = dynstring_from("https://swp.webspeiseplan.de/menu");
    gc_add_to_trace(string_temp22, type_descriptor_Zeichenkette, NULL);
    DynString** object_member_Wert_n21 = gc_alloc(type_descriptor_Zeiger_auf_Zeichenkette);
    *object_member_Wert_n21 = string_temp22;
    hashmap_insert(object_temp17, "Wert", object_member_Wert_n21);
    HashMap** list_idx_0_n17 = gc_alloc(type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_Schlüssel_DELIM_Zeichenkette_Wert_END);
    *list_idx_0_n17 = object_temp17;
    list_append(list_temp16, list_idx_0_n17);
    int64_t Antwort_Zahl25 = __hpi_internal_http(string_temp14, URL12, string_temp15, list_temp16, &Körper9, gc_add_to_trace);
    void* if_res27;
    if (Antwort_Zahl25 != 200) {
        // begin block
        cexit(1);
        gc_remove_roots(0, (void*[0]){});
        // end block
    }
    if_res27;
    AnyValue runtime_cast_from27 = __hpi_internal_parse_json(Körper9, gc_alloc, gc_add_to_trace);
    AnyObject* gc_root_temp29 = *(AnyObject**) __hpi_internal_runtime_cast(runtime_cast_from27, type_descriptor_Speicherbox, gc_alloc);
    gc_add_root(gc_root_temp29, type_descriptor_Speicherbox, "*(AnyObject**) __hpi_internal_runtime_cast(runtime_cast_from27, type_descriptor_Speicherbox, gc_alloc)");
    AnyObject* Res28 = gc_root_temp29;
    DynString* string_temp30 = dynstring_from("content");
    gc_add_to_trace(string_temp30, type_descriptor_Zeichenkette, NULL);
    AnyValue runtime_cast_from31 = __hpi_internal_anyobj_take(Res28, string_temp30);
    ListNode* gc_root_temp33 = *(ListNode**) __hpi_internal_runtime_cast(runtime_cast_from31, type_descriptor_Liste_von_Speicherbox, gc_alloc);
    gc_add_root(gc_root_temp33, type_descriptor_Liste_von_Speicherbox, "*(ListNode**) __hpi_internal_runtime_cast(runtime_cast_from31, type_descriptor_Liste_von_Speicherbox, gc_alloc)");
    ListNode* ContentRes32 = gc_root_temp33;
    int64_t Zähler34 = 0;
    ListNode* list_temp35 = gc_alloc(type_descriptor_Liste_von_Unbekannt);
    ListNode* gc_root_temp37 = list_temp35;
    gc_add_root(gc_root_temp37, type_descriptor_Liste_von_Unbekannt, "list_temp35");
    ListNode* Resultat36 = gc_root_temp37;
    // while
    head_0:;
    if (!(Zähler34 < __hpi_internal_list_len(ContentRes32))) {
        goto break_0;
    }
    // begin block
    AnyObject* gc_root_temp39 = *(AnyObject**) __hpi_internal_list_index(ContentRes32, Zähler34);
    gc_add_root(gc_root_temp39, type_descriptor_Speicherbox, "*(AnyObject**) __hpi_internal_list_index(ContentRes32, Zähler34)");
    AnyObject* ResTemp38 = gc_root_temp39;
    DynString* string_temp40 = dynstring_from("speiseplanGerichtData");
    gc_add_to_trace(string_temp40, type_descriptor_Zeichenkette, NULL);
    AnyValue runtime_cast_from41 = __hpi_internal_anyobj_take(ResTemp38, string_temp40);
    ListNode* gc_root_temp43 = *(ListNode**) __hpi_internal_runtime_cast(runtime_cast_from41, type_descriptor_Liste_von_Speicherbox, gc_alloc);
    gc_add_root(gc_root_temp43, type_descriptor_Liste_von_Speicherbox, "*(ListNode**) __hpi_internal_runtime_cast(runtime_cast_from41, type_descriptor_Liste_von_Speicherbox, gc_alloc)");
    ListNode* Gerichte42 = gc_root_temp43;
    DynString* string_temp44 = dynstring_from("speiseplanAdvanced");
    gc_add_to_trace(string_temp44, type_descriptor_Zeichenkette, NULL);
    AnyValue runtime_cast_from45 = __hpi_internal_anyobj_take(ResTemp38, string_temp44);
    AnyObject* gc_root_temp47 = *(AnyObject**) __hpi_internal_runtime_cast(runtime_cast_from45, type_descriptor_Speicherbox, gc_alloc);
    gc_add_root(gc_root_temp47, type_descriptor_Speicherbox, "*(AnyObject**) __hpi_internal_runtime_cast(runtime_cast_from45, type_descriptor_Speicherbox, gc_alloc)");
    AnyObject* SpeiseplanAdvanced46 = gc_root_temp47;
    DynString* string_temp48 = dynstring_from("titel");
    gc_add_to_trace(string_temp48, type_descriptor_Zeichenkette, NULL);
    AnyValue runtime_cast_from49 = __hpi_internal_anyobj_take(SpeiseplanAdvanced46, string_temp48);
    DynString* gc_root_temp51 = *(DynString**) __hpi_internal_runtime_cast(runtime_cast_from49, type_descriptor_Zeichenkette, gc_alloc);
    gc_add_root(gc_root_temp51, type_descriptor_Zeichenkette, "*(DynString**) __hpi_internal_runtime_cast(runtime_cast_from49, type_descriptor_Zeichenkette, gc_alloc)");
    DynString* ZeitSlot50 = gc_root_temp51;
    int64_t Gerichtzähler52 = 0;
    // while
    head_1:;
    if (!((Gerichtzähler52 < __hpi_internal_list_len(Gerichte42)))) {
        goto break_1;
    }
    // begin block
    AnyObject* gc_root_temp54 = *(AnyObject**) __hpi_internal_list_index(Gerichte42, Gerichtzähler52);
    gc_add_root(gc_root_temp54, type_descriptor_Speicherbox, "*(AnyObject**) __hpi_internal_list_index(Gerichte42, Gerichtzähler52)");
    AnyObject* Gericht53 = gc_root_temp54;
    Gerichtzähler52 += 1;
    DynString* string_temp55 = dynstring_from("zusatzinformationen");
    gc_add_to_trace(string_temp55, type_descriptor_Zeichenkette, NULL);
    AnyValue runtime_cast_from56 = __hpi_internal_anyobj_take(Gericht53, string_temp55);
    AnyObject* gc_root_temp58 = *(AnyObject**) __hpi_internal_runtime_cast(runtime_cast_from56, type_descriptor_Speicherbox, gc_alloc);
    gc_add_root(gc_root_temp58, type_descriptor_Speicherbox, "*(AnyObject**) __hpi_internal_runtime_cast(runtime_cast_from56, type_descriptor_Speicherbox, gc_alloc)");
    AnyObject* ZusatzInfos57 = gc_root_temp58;
    DynString* string_temp59 = dynstring_from("mitarbeiterpreisDecimal2");
    gc_add_to_trace(string_temp59, type_descriptor_Zeichenkette, NULL);
    AnyValue runtime_cast_from60 = __hpi_internal_anyobj_take(ZusatzInfos57, string_temp59);
    double Preis61 = *(double*) __hpi_internal_runtime_cast(runtime_cast_from60, type_descriptor_Fliesskommazahl, gc_alloc);
    DynString* string_temp62 = dynstring_from("nweiweissDecimal1");
    gc_add_to_trace(string_temp62, type_descriptor_Zeichenkette, NULL);
    AnyValue runtime_cast_from63 = __hpi_internal_anyobj_take(ZusatzInfos57, string_temp62);
    double Protein64 = *(double*) __hpi_internal_runtime_cast(runtime_cast_from63, type_descriptor_Fliesskommazahl, gc_alloc);
    DynString* string_temp65 = dynstring_from("nwkcalInteger");
    gc_add_to_trace(string_temp65, type_descriptor_Zeichenkette, NULL);
    AnyValue runtime_cast_from66 = __hpi_internal_anyobj_take(ZusatzInfos57, string_temp65);
    double Kcal67 = *(double*) __hpi_internal_runtime_cast(runtime_cast_from66, type_descriptor_Fliesskommazahl, gc_alloc);
    DynString* string_temp68 = dynstring_from("nwfettDecimal1");
    gc_add_to_trace(string_temp68, type_descriptor_Zeichenkette, NULL);
    AnyValue runtime_cast_from69 = __hpi_internal_anyobj_take(ZusatzInfos57, string_temp68);
    double Fett70 = *(double*) __hpi_internal_runtime_cast(runtime_cast_from69, type_descriptor_Fliesskommazahl, gc_alloc);
    DynString* string_temp71 = dynstring_from("nwzuckerDecimal1");
    gc_add_to_trace(string_temp71, type_descriptor_Zeichenkette, NULL);
    AnyValue runtime_cast_from72 = __hpi_internal_anyobj_take(ZusatzInfos57, string_temp71);
    double Zucker73 = *(double*) __hpi_internal_runtime_cast(runtime_cast_from72, type_descriptor_Fliesskommazahl, gc_alloc);
    DynString* string_temp74 = dynstring_from("nwzuckerDecimal1");
    gc_add_to_trace(string_temp74, type_descriptor_Zeichenkette, NULL);
    AnyValue runtime_cast_from75 = __hpi_internal_anyobj_take(ZusatzInfos57, string_temp74);
    double Kohlenhydrate76 = *(double*) __hpi_internal_runtime_cast(runtime_cast_from75, type_descriptor_Fliesskommazahl, gc_alloc);
    DynString* string_temp77 = dynstring_from("speiseplanAdvancedGericht");
    gc_add_to_trace(string_temp77, type_descriptor_Zeichenkette, NULL);
    AnyValue runtime_cast_from78 = __hpi_internal_anyobj_take(Gericht53, string_temp77);
    AnyObject* gc_root_temp80 = *(AnyObject**) __hpi_internal_runtime_cast(runtime_cast_from78, type_descriptor_Speicherbox, gc_alloc);
    gc_add_root(gc_root_temp80, type_descriptor_Speicherbox, "*(AnyObject**) __hpi_internal_runtime_cast(runtime_cast_from78, type_descriptor_Speicherbox, gc_alloc)");
    AnyObject* GerichtInfos79 = gc_root_temp80;
    DynString* string_temp81 = dynstring_from("datum");
    gc_add_to_trace(string_temp81, type_descriptor_Zeichenkette, NULL);
    AnyValue runtime_cast_from82 = __hpi_internal_anyobj_take(GerichtInfos79, string_temp81);
    DynString* gc_root_temp84 = *(DynString**) __hpi_internal_runtime_cast(runtime_cast_from82, type_descriptor_Zeichenkette, gc_alloc);
    gc_add_root(gc_root_temp84, type_descriptor_Zeichenkette, "*(DynString**) __hpi_internal_runtime_cast(runtime_cast_from82, type_descriptor_Zeichenkette, gc_alloc)");
    DynString* GerichtDatumRoh83 = gc_root_temp84;
    DynString* string_temp85 = dynstring_from("T");
    gc_add_to_trace(string_temp85, type_descriptor_Zeichenkette, NULL);
    DynString* gc_root_temp87 = *(DynString**) __hpi_internal_list_index(__hpi_internal_string_split(GerichtDatumRoh83, string_temp85, gc_add_to_trace), 0);
    gc_add_root(gc_root_temp87, type_descriptor_Zeichenkette, "*(DynString**) __hpi_internal_list_index(__hpi_internal_string_split(GerichtDatumRoh83, string_temp85, gc_add_to_trace), 0)");
    DynString* GerichtDatum86 = gc_root_temp87;
    DynString* string_temp88 = dynstring_from("%d-%2d-%2d");
    gc_add_to_trace(string_temp88, type_descriptor_Zeichenkette, NULL);
    MapGetResult m_Jahr_89 = hashmap_get(__hpi_internal_time(gc_add_to_trace), "Jahr");
    assert(m_Jahr_89.found);
    MapGetResult m_Monat_90 = hashmap_get(__hpi_internal_time(gc_add_to_trace), "Monat");
    assert(m_Monat_90.found);
    MapGetResult m_Kalendar_Tag_91 = hashmap_get(__hpi_internal_time(gc_add_to_trace), "Kalendar_Tag");
    assert(m_Kalendar_Tag_91.found);
    int64_t fmt_ptr_92 = *(int64_t*) m_Jahr_89.value;
    __hpi_internal_libSAP_reset();
    int64_t fmt_ptr_93 = *(int64_t*) m_Monat_90.value;
    __hpi_internal_libSAP_reset();
    int64_t fmt_ptr_94 = *(int64_t*) m_Kalendar_Tag_91.value;
    __hpi_internal_libSAP_reset();
    DynString* gc_root_temp96 = __hpi_internal_fmt(3, string_temp88, gc_add_to_trace, type_descriptor_Zahl, &fmt_ptr_92, type_descriptor_Zahl, &fmt_ptr_93, type_descriptor_Zahl, &fmt_ptr_94);
    gc_add_root(gc_root_temp96, type_descriptor_Zeichenkette, "__hpi_internal_fmt(3, string_temp88, gc_add_to_trace, type_descriptor_Zahl, &fmt_ptr_92, type_descriptor_Zahl, &fmt_ptr_93, type_descriptor_Zahl, &fmt_ptr_94)");
    DynString* HeuteDatum95 = gc_root_temp96;
    DynString* string_temp97 = dynstring_from("gerichtname");
    gc_add_to_trace(string_temp97, type_descriptor_Zeichenkette, NULL);
    AnyValue runtime_cast_from98 = __hpi_internal_anyobj_take(GerichtInfos79, string_temp97);
    DynString* gc_root_temp100 = *(DynString**) __hpi_internal_runtime_cast(runtime_cast_from98, type_descriptor_Zeichenkette, gc_alloc);
    gc_add_root(gc_root_temp100, type_descriptor_Zeichenkette, "*(DynString**) __hpi_internal_runtime_cast(runtime_cast_from98, type_descriptor_Zeichenkette, gc_alloc)");
    DynString* Gerichtname99 = gc_root_temp100;
    DynString* string_temp101 = dynstring_from("B");
    gc_add_to_trace(string_temp101, type_descriptor_Zeichenkette, NULL);
    DynString* string_temp102 = dynstring_from("a");
    gc_add_to_trace(string_temp102, type_descriptor_Zeichenkette, NULL);
    Gerichtname99 = __hpi_internal_string_replace(Gerichtname99, string_temp101, string_temp102);
    bool logical_res105;
    if ((!dynstring_strcmp(GerichtDatum86, HeuteDatum95))) {
        logical_res105 = true;
    } else {
        DynString* string_temp103 = dynstring_from("Salatbuffet");
        gc_add_to_trace(string_temp103, type_descriptor_Zeichenkette, NULL);
        logical_res105 = __hpi_internal_string_starts_with(Gerichtname99, string_temp103);
    }
    bool logical_res107;
    if (logical_res105) {
        logical_res107 = true;
    } else {
        DynString* string_temp105 = dynstring_from("Relevo");
        gc_add_to_trace(string_temp105, type_descriptor_Zeichenkette, NULL);
        logical_res107 = __hpi_internal_string_contains(Gerichtname99, string_temp105);
    }
    bool logical_res109;
    if (logical_res107) {
        logical_res109 = true;
    } else {
        DynString* string_temp107 = dynstring_from("Wochenende!");
        gc_add_to_trace(string_temp107, type_descriptor_Zeichenkette, NULL);
        logical_res109 = __hpi_internal_string_contains(Gerichtname99, string_temp107);
    }
    bool logical_res111;
    if (logical_res109) {
        logical_res111 = true;
    } else {
        DynString* string_temp109 = dynstring_from("Preise pro 100 g");
        gc_add_to_trace(string_temp109, type_descriptor_Zeichenkette, NULL);
        logical_res111 = __hpi_internal_string_contains(Gerichtname99, string_temp109);
    }
    bool logical_res113;
    if (logical_res111) {
        logical_res113 = true;
    } else {
        DynString* string_temp111 = dynstring_from("Preis pro 100 g");
        gc_add_to_trace(string_temp111, type_descriptor_Zeichenkette, NULL);
        logical_res113 = __hpi_internal_string_contains(Gerichtname99, string_temp111);
    }
    bool logical_res115;
    if (logical_res113) {
        logical_res115 = true;
    } else {
        DynString* string_temp113 = dynstring_from("Preis pro 100g");
        gc_add_to_trace(string_temp113, type_descriptor_Zeichenkette, NULL);
        logical_res115 = __hpi_internal_string_contains(Gerichtname99, string_temp113);
    }
    void* if_res116;
    if (logical_res115) {
        // begin block
        gc_remove_roots(0, (void*[0]){});
        goto head_1;
        gc_remove_roots(0, (void*[0]){});
        // end block
    }
    if_res116;
    HashMap* object_temp116 = gc_alloc(type_descriptor_Objekt_BEGIN__END);
    DynString** object_member_ZeitSlot_n117 = gc_alloc(type_descriptor_Zeiger_auf_Zeichenkette);
    *object_member_ZeitSlot_n117 = ZeitSlot50;
    hashmap_insert(object_temp116, "ZeitSlot", object_member_ZeitSlot_n117);
    DynString* string_temp120 = dynstring_from("Angebot %d");
    gc_add_to_trace(string_temp120, type_descriptor_Zeichenkette, NULL);
    int64_t fmt_ptr_121 = __hpi_internal_list_len(Resultat36) + 1;
    __hpi_internal_libSAP_reset();
    DynString** object_member_Titel_n119 = gc_alloc(type_descriptor_Zeiger_auf_Zeichenkette);
    *object_member_Titel_n119 = __hpi_internal_fmt(1, string_temp120, gc_add_to_trace, type_descriptor_Zahl, &fmt_ptr_121);
    hashmap_insert(object_temp116, "Titel", object_member_Titel_n119);
    DynString** object_member_Beschreibung_n123 = gc_alloc(type_descriptor_Zeiger_auf_Zeichenkette);
    *object_member_Beschreibung_n123 = Gerichtname99;
    hashmap_insert(object_temp116, "Beschreibung", object_member_Beschreibung_n123);
    double* object_member_Preis_n125 = gc_alloc(type_descriptor_Zeiger_auf_Fliesskommazahl);
    *object_member_Preis_n125 = Preis61;
    hashmap_insert(object_temp116, "Preis", object_member_Preis_n125);
    double* object_member_Protein_GR_n127 = gc_alloc(type_descriptor_Zeiger_auf_Fliesskommazahl);
    *object_member_Protein_GR_n127 = Protein64;
    hashmap_insert(object_temp116, "Protein_GR", object_member_Protein_GR_n127);
    double* object_member_Kcal_n129 = gc_alloc(type_descriptor_Zeiger_auf_Fliesskommazahl);
    *object_member_Kcal_n129 = Kcal67;
    hashmap_insert(object_temp116, "Kcal", object_member_Kcal_n129);
    double* object_member_Fett_GR_n131 = gc_alloc(type_descriptor_Zeiger_auf_Fliesskommazahl);
    *object_member_Fett_GR_n131 = Fett70;
    hashmap_insert(object_temp116, "Fett_GR", object_member_Fett_GR_n131);
    double* object_member_Zucker_GR_n133 = gc_alloc(type_descriptor_Zeiger_auf_Fliesskommazahl);
    *object_member_Zucker_GR_n133 = Zucker73;
    hashmap_insert(object_temp116, "Zucker_GR", object_member_Zucker_GR_n133);
    double* object_member_Kohlenhydrate_GR_n135 = gc_alloc(type_descriptor_Zeiger_auf_Fliesskommazahl);
    *object_member_Kohlenhydrate_GR_n135 = Kohlenhydrate76;
    hashmap_insert(object_temp116, "Kohlenhydrate_GR", object_member_Kohlenhydrate_GR_n135);
    DynString** object_member_ZeitSlot_n137 = gc_alloc(type_descriptor_Zeiger_auf_Zeichenkette);
    *object_member_ZeitSlot_n137 = ZeitSlot50;
    hashmap_insert(object_temp116, "ZeitSlot", object_member_ZeitSlot_n137);
    HashMap** push_ptr_139 = gc_alloc(type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_ZeitSlot_DELIM_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END);
    *push_ptr_139 = object_temp116;
    __hpi_internal_list_push(Resultat36, push_ptr_139);
    // end block
    gc_remove_roots(7, (void*[7]){gc_root_temp54, gc_root_temp58, gc_root_temp80, gc_root_temp84, gc_root_temp87, gc_root_temp96, gc_root_temp100});
    goto head_1;
    break_1:;
    gc_remove_roots(7, (void*[7]){gc_root_temp54, gc_root_temp58, gc_root_temp80, gc_root_temp84, gc_root_temp87, gc_root_temp96, gc_root_temp100});
    Zähler34 += 1;
    // end block
    gc_remove_roots(4, (void*[4]){gc_root_temp39, gc_root_temp43, gc_root_temp47, gc_root_temp51});
    goto head_0;
    break_0:;
    gc_remove_roots(4, (void*[4]){gc_root_temp39, gc_root_temp43, gc_root_temp47, gc_root_temp51});
    gc_remove_roots(5, (void*[5]){gc_root_temp10, gc_root_temp13, gc_root_temp29, gc_root_temp33, gc_root_temp37});
    return Resultat36;
}

void Aktualisiere1(int64_t Mensa_Ort140) {
    DynString* string_temp141 = dynstring_from("    ~> Lade Speiseangebot aus dem Internet herunter...");
    gc_add_to_trace(string_temp141, type_descriptor_Zeichenkette, NULL);
    __hpi_internal_libSAP_reset();
    DynString* fmt_ptr_142 = string_temp141;
    __hpi_internal_print(1, type_descriptor_Zeichenkette, &fmt_ptr_142);
    ListNode* gc_root_temp144 = Lade_Speiseangebot0(Mensa_Ort140);
    gc_add_root(gc_root_temp144, type_descriptor_Liste_von_Speise, "Lade_Speiseangebot0(Mensa_Ort140)");
    ListNode* Speisen143 = gc_root_temp144;
    DynString* string_temp145 = dynstring_from("    ~> Speiseangebot bereit. %d Speisen geladen.");
    gc_add_to_trace(string_temp145, type_descriptor_Zeichenkette, NULL);
    int64_t fmt_ptr_146 = __hpi_internal_list_len(Speisen143);
    __hpi_internal_libSAP_reset();
    __hpi_internal_libSAP_reset();
    DynString* fmt_ptr_147 = __hpi_internal_fmt(1, string_temp145, gc_add_to_trace, type_descriptor_Zahl, &fmt_ptr_146);
    __hpi_internal_print(1, type_descriptor_Zeichenkette, &fmt_ptr_147);
    ListNode* list_temp148 = gc_alloc(type_descriptor_Liste_von_Unbekannt);
    ListNode* gc_root_temp150 = list_temp148;
    gc_add_root(gc_root_temp150, type_descriptor_Liste_von_Unbekannt, "list_temp148");
    ListNode* Mittagessen149 = gc_root_temp150;
    ListNode* list_temp151 = gc_alloc(type_descriptor_Liste_von_Unbekannt);
    ListNode* gc_root_temp153 = list_temp151;
    gc_add_root(gc_root_temp153, type_descriptor_Liste_von_Unbekannt, "list_temp151");
    ListNode* Abendessen152 = gc_root_temp153;
    int64_t Zähler154 = 0;
    // while
    head_2:;
    if (!(Zähler154 < __hpi_internal_list_len(Speisen143))) {
        goto break_2;
    }
    // begin block
    HashMap* gc_root_temp156 = *(HashMap**) __hpi_internal_list_index(Speisen143, Zähler154);
    gc_add_root(gc_root_temp156, type_descriptor_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END, "*(HashMap**) __hpi_internal_list_index(Speisen143, Zähler154)");
    HashMap* S155 = gc_root_temp156;
    DynString* string_temp157 = dynstring_from(" ");
    gc_add_to_trace(string_temp157, type_descriptor_Zeichenkette, NULL);
    DynString* string_temp158 = dynstring_from("");
    gc_add_to_trace(string_temp158, type_descriptor_Zeichenkette, NULL);
    MapGetResult m_ZeitSlot_159 = hashmap_get(S155, "ZeitSlot");
    assert(m_ZeitSlot_159.found);
    DynString* gc_root_temp161 = __hpi_internal_string_replace(*(DynString**) m_ZeitSlot_159.value, string_temp157, string_temp158);
    gc_add_root(gc_root_temp161, type_descriptor_Zeichenkette, "__hpi_internal_string_replace(*(DynString**) m_ZeitSlot_159.value, string_temp157, string_temp158)");
    DynString* ZeitSlot160 = gc_root_temp161;
    DynString* string_temp162 = dynstring_from("Mittagessen");
    gc_add_to_trace(string_temp162, type_descriptor_Zeichenkette, NULL);
    void* if_res164;
    if (__hpi_internal_string_contains(ZeitSlot160, string_temp162)) {
        // begin block
        HashMap** push_ptr_164 = gc_alloc(type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END);
        *push_ptr_164 = S155;
        __hpi_internal_list_push(Mittagessen149, push_ptr_164);
        gc_remove_roots(0, (void*[0]){});
        // end block
    } else {
        // begin block
        DynString* string_temp165 = dynstring_from("Abendessen");
        gc_add_to_trace(string_temp165, type_descriptor_Zeichenkette, NULL);
        void* if_res167;
        if (__hpi_internal_string_contains(ZeitSlot160, string_temp165)) {
            // begin block
            HashMap** push_ptr_167 = gc_alloc(type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END);
            *push_ptr_167 = S155;
            __hpi_internal_list_push(Abendessen152, push_ptr_167);
            gc_remove_roots(0, (void*[0]){});
            // end block
        } else {
            // begin block
            DynString* string_temp168 = dynstring_from("Automatenplan");
            gc_add_to_trace(string_temp168, type_descriptor_Zeichenkette, NULL);
            void* if_res170;
            if (__hpi_internal_string_contains(ZeitSlot160, string_temp168)) {
                // begin block
                void* if_res171;
                if (ZEIGE_MEHR_NACHRICHTEN6) {
                    // begin block
                    DynString* string_temp171 = dynstring_from("[NACHRICHT] Ignoriere Automat");
                    gc_add_to_trace(string_temp171, type_descriptor_Zeichenkette, NULL);
                    __hpi_internal_libSAP_reset();
                    DynString* fmt_ptr_172 = string_temp171;
                    __hpi_internal_print(1, type_descriptor_Zeichenkette, &fmt_ptr_172);
                    gc_remove_roots(0, (void*[0]){});
                    // end block
                }
                gc_remove_roots(0, (void*[0]){});
                // end block
                if_res170 = if_res171;
            } else {
                // begin block
                DynString* string_temp173 = dynstring_from("Nicht unterstützter ZeitSlot: `%s`");
                gc_add_to_trace(string_temp173, type_descriptor_Zeichenkette, NULL);
                DynString* fmt_ptr_174 = ZeitSlot160;
                __hpi_internal_libSAP_reset();
                __hpi_internal_libSAP_reset();
                DynString* fmt_ptr_175 = __hpi_internal_fmt(1, string_temp173, gc_add_to_trace, type_descriptor_Zeichenkette, &fmt_ptr_174);
                __hpi_internal_print(1, type_descriptor_Zeichenkette, &fmt_ptr_175);
                cexit(1);
                gc_remove_roots(0, (void*[0]){});
                // end block
            }
            gc_remove_roots(0, (void*[0]){});
            // end block
            if_res167 = if_res170;
        }
        gc_remove_roots(0, (void*[0]){});
        // end block
        if_res164 = if_res167;
    }
    if_res164;
    Zähler154 += 1;
    // end block
    gc_remove_roots(2, (void*[2]){gc_root_temp156, gc_root_temp161});
    goto head_2;
    break_2:;
    gc_remove_roots(2, (void*[2]){gc_root_temp156, gc_root_temp161});
    DynString* string_temp176 = dynstring_from("======= MITTAGESSEN ========");
    gc_add_to_trace(string_temp176, type_descriptor_Zeichenkette, NULL);
    __hpi_internal_libSAP_reset();
    DynString* fmt_ptr_177 = string_temp176;
    __hpi_internal_print(1, type_descriptor_Zeichenkette, &fmt_ptr_177);
    Gericht_Liste_Drucken2(Mittagessen149);
    DynString* string_temp178 = dynstring_from("======== ABENDESSEN ========");
    gc_add_to_trace(string_temp178, type_descriptor_Zeichenkette, NULL);
    __hpi_internal_libSAP_reset();
    DynString* fmt_ptr_179 = string_temp178;
    __hpi_internal_print(1, type_descriptor_Zeichenkette, &fmt_ptr_179);
    Gericht_Liste_Drucken2(Abendessen152);
    gc_remove_roots(3, (void*[3]){gc_root_temp144, gc_root_temp150, gc_root_temp153});
}

void Gericht_Liste_Drucken2(ListNode* Eingabe180) {
    int64_t Zähler181 = 0;
    // while
    head_3:;
    if (!(Zähler181 < __hpi_internal_list_len(Eingabe180))) {
        goto break_3;
    }
    // begin block
    HashMap* gc_root_temp183 = *(HashMap**) __hpi_internal_list_index(Eingabe180, Zähler181);
    gc_add_root(gc_root_temp183, type_descriptor_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END, "*(HashMap**) __hpi_internal_list_index(Eingabe180, Zähler181)");
    HashMap* S182 = gc_root_temp183;
    DynString* string_temp184 = dynstring_from("B");
    gc_add_to_trace(string_temp184, type_descriptor_Zeichenkette, NULL);
    DynString* string_temp185 = dynstring_from("X");
    gc_add_to_trace(string_temp185, type_descriptor_Zeichenkette, NULL);
    DynString* string_temp186 = dynstring_from("Banane");
    gc_add_to_trace(string_temp186, type_descriptor_Zeichenkette, NULL);
    DynString* gc_root_temp188 = __hpi_internal_string_replace(string_temp186, string_temp184, string_temp185);
    gc_add_root(gc_root_temp188, type_descriptor_Zeichenkette, "__hpi_internal_string_replace(string_temp186, string_temp184, string_temp185)");
    DynString* A187 = gc_root_temp188;
    DynString* string_temp189 = dynstring_from("[0;32m    - %s | %.2f€ | %s[0m");
    gc_add_to_trace(string_temp189, type_descriptor_Zeichenkette, NULL);
    MapGetResult m_Titel_190 = hashmap_get(S182, "Titel");
    assert(m_Titel_190.found);
    MapGetResult m_Preis_191 = hashmap_get(S182, "Preis");
    assert(m_Preis_191.found);
    DynString* fmt_ptr_192 = *(DynString**) m_Titel_190.value;
    __hpi_internal_libSAP_reset();
    double fmt_ptr_193 = *(double*) m_Preis_191.value;
    __hpi_internal_libSAP_reset();
    DynString* fmt_ptr_194 = A187;
    __hpi_internal_libSAP_reset();
    __hpi_internal_libSAP_reset();
    DynString* fmt_ptr_195 = __hpi_internal_fmt(3, string_temp189, gc_add_to_trace, type_descriptor_Zeichenkette, &fmt_ptr_192, type_descriptor_Fliesskommazahl, &fmt_ptr_193, type_descriptor_Zeichenkette, &fmt_ptr_194);
    __hpi_internal_print(1, type_descriptor_Zeichenkette, &fmt_ptr_195);
    DynString* string_temp196 = dynstring_from("[0;31m        * %d Kcal        * %d g. Protein        * %d g. Fett          * %d g. Kohl.[0m");
    gc_add_to_trace(string_temp196, type_descriptor_Zeichenkette, NULL);
    MapGetResult m_Kcal_197 = hashmap_get(S182, "Kcal");
    assert(m_Kcal_197.found);
    MapGetResult m_Protein_GR_198 = hashmap_get(S182, "Protein_GR");
    assert(m_Protein_GR_198.found);
    MapGetResult m_Fett_GR_199 = hashmap_get(S182, "Fett_GR");
    assert(m_Fett_GR_199.found);
    MapGetResult m_Kohlenhydrate_GR_200 = hashmap_get(S182, "Kohlenhydrate_GR");
    assert(m_Kohlenhydrate_GR_200.found);
    int64_t fmt_ptr_201 = (int64_t) *(double*) m_Kcal_197.value;
    __hpi_internal_libSAP_reset();
    int64_t fmt_ptr_202 = (int64_t) *(double*) m_Protein_GR_198.value;
    __hpi_internal_libSAP_reset();
    int64_t fmt_ptr_203 = (int64_t) *(double*) m_Fett_GR_199.value;
    __hpi_internal_libSAP_reset();
    int64_t fmt_ptr_204 = (int64_t) *(double*) m_Kohlenhydrate_GR_200.value;
    __hpi_internal_libSAP_reset();
    __hpi_internal_libSAP_reset();
    DynString* fmt_ptr_205 = __hpi_internal_fmt(4, string_temp196, gc_add_to_trace, type_descriptor_Zahl, &fmt_ptr_201, type_descriptor_Zahl, &fmt_ptr_202, type_descriptor_Zahl, &fmt_ptr_203, type_descriptor_Zahl, &fmt_ptr_204);
    __hpi_internal_print(1, type_descriptor_Zeichenkette, &fmt_ptr_205);
    Zähler181 += 1;
    // end block
    gc_remove_roots(2, (void*[2]){gc_root_temp183, gc_root_temp188});
    goto head_3;
    break_3:;
    gc_remove_roots(2, (void*[2]){gc_root_temp183, gc_root_temp188});
    void* if_res207;
    if (Zähler181 == 0) {
        // begin block
        DynString* string_temp207 = dynstring_from("    ");
        gc_add_to_trace(string_temp207, type_descriptor_Zeichenkette, NULL);
        __hpi_internal_libSAP_reset();
        DynString* fmt_ptr_208 = string_temp207;
        __hpi_internal_libSAP_reset();
        DynString* fmt_ptr_209 = KEINE_DATEN5;
        __hpi_internal_print(2, type_descriptor_Zeichenkette, &fmt_ptr_208, type_descriptor_Zeichenkette, &fmt_ptr_209);
        gc_remove_roots(0, (void*[0]){});
        // end block
    }
    gc_remove_roots(0, (void*[0]){});
    return;
}

DynString* bewerbung() {
    DynString* string_temp210 = dynstring_from("Hallo Welt!");
    gc_add_to_trace(string_temp210, type_descriptor_Zeichenkette, NULL);
    gc_remove_roots(0, (void*[0]){});
    return string_temp210;
}

void einschreibung(int64_t Matrikelnummer211) {
    int64_t _212 = Matrikelnummer211;
    gc_remove_roots(0, (void*[0]){});
}

void studium() {
    ListNode* gc_root_temp214 = __hpi_internal_args(gc_alloc, gc_add_to_trace);
    gc_add_root(gc_root_temp214, type_descriptor_Liste_von_Zeichenkette, "__hpi_internal_args(gc_alloc, gc_add_to_trace)");
    ListNode* Args213 = gc_root_temp214;
    void* if_res216;
    if (__hpi_internal_list_len(Args213) == 1) {
        // begin block
        DynString* string_temp216 = dynstring_from("Erwartete Befehlszeilenargument <mensa-ort>, bekam nichts");
        gc_add_to_trace(string_temp216, type_descriptor_Zeichenkette, NULL);
        __hpi_internal_libSAP_reset();
        DynString* fmt_ptr_217 = string_temp216;
        __hpi_internal_print(1, type_descriptor_Zeichenkette, &fmt_ptr_217);
        cexit(69);
        gc_remove_roots(0, (void*[0]){});
        // end block
    }
    if_res216;
    DynString* gc_root_temp219 = *(DynString**) __hpi_internal_list_index(Args213, 1);
    gc_add_root(gc_root_temp219, type_descriptor_Zeichenkette, "*(DynString**) __hpi_internal_list_index(Args213, 1)");
    DynString* Eingabe218 = gc_root_temp219;
    int64_t Mensa_Ort220 = 0;
    DynString* string_temp221 = dynstring_from("hpi");
    gc_add_to_trace(string_temp221, type_descriptor_Zeichenkette, NULL);
    void* if_res223;
    if (dynstring_strcmp(Eingabe218, string_temp221)) {
        // begin block
        Mensa_Ort220 = MENSA_GRIEBNITZSEE1;
        gc_remove_roots(0, (void*[0]){});
        // end block
    } else {
        // begin block
        DynString* string_temp223 = dynstring_from("filmuni");
        gc_add_to_trace(string_temp223, type_descriptor_Zeichenkette, NULL);
        void* if_res225;
        if (dynstring_strcmp(Eingabe218, string_temp223)) {
            // begin block
            Mensa_Ort220 = MENSA_FILMUNI0;
            gc_remove_roots(0, (void*[0]){});
            // end block
        } else {
            // begin block
            DynString* string_temp225 = dynstring_from("bernau");
            gc_add_to_trace(string_temp225, type_descriptor_Zeichenkette, NULL);
            if (dynstring_strcmp(Eingabe218, string_temp225)) {
                // begin block
                DynString* string_temp226 = dynstring_from("Gefährlicher Standort: <%s>\nValide, weniger gefährliche Standorte:\n    - hpi\n    - filmuni");
                gc_add_to_trace(string_temp226, type_descriptor_Zeichenkette, NULL);
                DynString* fmt_ptr_227 = Eingabe218;
                __hpi_internal_libSAP_reset();
                __hpi_internal_libSAP_reset();
                DynString* fmt_ptr_228 = __hpi_internal_fmt(1, string_temp226, gc_add_to_trace, type_descriptor_Zeichenkette, &fmt_ptr_227);
                __hpi_internal_print(1, type_descriptor_Zeichenkette, &fmt_ptr_228);
                DynString* string_temp229 = dynstring_from("UND: wurde in Bernau überhaupt schon Essen erfunden? - Oder gibt es da nur Salz?");
                gc_add_to_trace(string_temp229, type_descriptor_Zeichenkette, NULL);
                __hpi_internal_libSAP_reset();
                DynString* fmt_ptr_230 = string_temp229;
                __hpi_internal_print(1, type_descriptor_Zeichenkette, &fmt_ptr_230);
                cexit(69);
                gc_remove_roots(0, (void*[0]){});
                // end block
            } else {
                // begin block
                DynString* string_temp231 = dynstring_from("afrika");
                gc_add_to_trace(string_temp231, type_descriptor_Zeichenkette, NULL);
                if (dynstring_strcmp(Eingabe218, string_temp231)) {
                    // begin block
                    DynString* string_temp232 = dynstring_from("@TimoNeyer - was gibt's? hast du den speiseplan gechief'd?");
                    gc_add_to_trace(string_temp232, type_descriptor_Zeichenkette, NULL);
                    __hpi_internal_libSAP_reset();
                    DynString* fmt_ptr_233 = string_temp232;
                    __hpi_internal_print(1, type_descriptor_Zeichenkette, &fmt_ptr_233);
                    cexit(1);
                    gc_remove_roots(0, (void*[0]){});
                    // end block
                } else {
                    // begin block
                    DynString* string_temp234 = dynstring_from("Unbekannter Standort: <%s>\nValide Standorte:\n    - hpi\n    - filmuni");
                    gc_add_to_trace(string_temp234, type_descriptor_Zeichenkette, NULL);
                    DynString* fmt_ptr_235 = Eingabe218;
                    __hpi_internal_libSAP_reset();
                    __hpi_internal_libSAP_reset();
                    DynString* fmt_ptr_236 = __hpi_internal_fmt(1, string_temp234, gc_add_to_trace, type_descriptor_Zeichenkette, &fmt_ptr_235);
                    __hpi_internal_print(1, type_descriptor_Zeichenkette, &fmt_ptr_236);
                    cexit(42);
                    gc_remove_roots(0, (void*[0]){});
                    // end block
                }
                gc_remove_roots(0, (void*[0]){});
                // end block
            }
            gc_remove_roots(0, (void*[0]){});
            // end block
        }
        gc_remove_roots(0, (void*[0]){});
        // end block
        if_res223 = if_res225;
    }
    if_res223;
    void* if_res238;
    if (__hpi_internal_list_len(Args213) == 3) {
        // begin block
        Eingabe218 = *(DynString**) __hpi_internal_list_index(Args213, 2);
        DynString* string_temp238 = dynstring_from("schreibe");
        gc_add_to_trace(string_temp238, type_descriptor_Zeichenkette, NULL);
        void* if_res240;
        if (dynstring_strcmp(Eingabe218, string_temp238)) {
            // begin block
            ZEIGE_MEHR_NACHRICHTEN6 = true;
            gc_remove_roots(0, (void*[0]){});
            // end block
        }
        gc_remove_roots(0, (void*[0]){});
        // end block
        if_res238 = if_res240;
    }
    if_res238;
    DynString* string_temp240 = dynstring_from("FAHRE HPI script version V%s für LINUX");
    gc_add_to_trace(string_temp240, type_descriptor_Zeichenkette, NULL);
    DynString* fmt_ptr_241 = __hpi_internal_get_version(gc_add_to_trace);
    __hpi_internal_libSAP_reset();
    __hpi_internal_libSAP_reset();
    DynString* fmt_ptr_242 = __hpi_internal_fmt(1, string_temp240, gc_add_to_trace, type_descriptor_Zeichenkette, &fmt_ptr_241);
    __hpi_internal_print(1, type_descriptor_Zeichenkette, &fmt_ptr_242);
    DynString* string_temp243 = dynstring_from("========================================================");
    gc_add_to_trace(string_temp243, type_descriptor_Zeichenkette, NULL);
    __hpi_internal_libSAP_reset();
    DynString* fmt_ptr_244 = string_temp243;
    __hpi_internal_print(1, type_descriptor_Zeichenkette, &fmt_ptr_244);
    Aktualisiere1(Mensa_Ort220);
    gc_remove_roots(2, (void*[2]){gc_root_temp214, gc_root_temp219});
}

void type_descriptor_setup() {
    // Type descriptor `Zeichenkette`
    type_descriptor_Zeichenkette.kind = TYPE_STRING;
    type_descriptor_Zeichenkette.ptr_count = 0;
    type_descriptor_Zeichenkette.list_inner = NULL;
    // Type descriptor `Zahl`
    type_descriptor_Zahl.kind = TYPE_INT;
    type_descriptor_Zahl.ptr_count = 0;
    type_descriptor_Zahl.list_inner = NULL;
    // Type descriptor `Objekt_BEGIN_Zeichenkette_Schlüssel_DELIM_Zeichenkette_Wert_END`
    type_descriptor_Objekt_BEGIN_Zeichenkette_Schlüssel_DELIM_Zeichenkette_Wert_END.kind = TYPE_OBJECT;
    type_descriptor_Objekt_BEGIN_Zeichenkette_Schlüssel_DELIM_Zeichenkette_Wert_END.ptr_count = 0;
    type_descriptor_Objekt_BEGIN_Zeichenkette_Schlüssel_DELIM_Zeichenkette_Wert_END.list_inner = NULL;
    type_descriptor_Objekt_BEGIN_Zeichenkette_Schlüssel_DELIM_Zeichenkette_Wert_END.obj_fields = hashmap_new();
    hashmap_insert(type_descriptor_Objekt_BEGIN_Zeichenkette_Schlüssel_DELIM_Zeichenkette_Wert_END.obj_fields, "Schlüssel", &type_descriptor_Zeichenkette);
    hashmap_insert(type_descriptor_Objekt_BEGIN_Zeichenkette_Schlüssel_DELIM_Zeichenkette_Wert_END.obj_fields, "Wert", &type_descriptor_Zeichenkette);
    // Type descriptor `Liste_von_Objekt_BEGIN_Zeichenkette_Schlüssel_DELIM_Zeichenkette_Wert_END`
    type_descriptor_Liste_von_Objekt_BEGIN_Zeichenkette_Schlüssel_DELIM_Zeichenkette_Wert_END.kind = TYPE_LIST;
    type_descriptor_Liste_von_Objekt_BEGIN_Zeichenkette_Schlüssel_DELIM_Zeichenkette_Wert_END.ptr_count = 0;
    type_descriptor_Liste_von_Objekt_BEGIN_Zeichenkette_Schlüssel_DELIM_Zeichenkette_Wert_END.list_inner = &type_descriptor_Objekt_BEGIN_Zeichenkette_Schlüssel_DELIM_Zeichenkette_Wert_END;
    // Type descriptor `Objekt_BEGIN__END`
    type_descriptor_Objekt_BEGIN__END.kind = TYPE_OBJECT;
    type_descriptor_Objekt_BEGIN__END.ptr_count = 0;
    type_descriptor_Objekt_BEGIN__END.list_inner = NULL;
    type_descriptor_Objekt_BEGIN__END.obj_fields = hashmap_new();
    // Type descriptor `Zeiger_auf_Zeichenkette`
    type_descriptor_Zeiger_auf_Zeichenkette.kind = TYPE_STRING;
    type_descriptor_Zeiger_auf_Zeichenkette.ptr_count = 1;
    type_descriptor_Zeiger_auf_Zeichenkette.list_inner = NULL;
    // Type descriptor `Zeiger_auf_Objekt_BEGIN_Zeichenkette_Schlüssel_DELIM_Zeichenkette_Wert_END`
    type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_Schlüssel_DELIM_Zeichenkette_Wert_END.kind = TYPE_OBJECT;
    type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_Schlüssel_DELIM_Zeichenkette_Wert_END.ptr_count = 1;
    type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_Schlüssel_DELIM_Zeichenkette_Wert_END.list_inner = NULL;
    type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_Schlüssel_DELIM_Zeichenkette_Wert_END.obj_fields = hashmap_new();
    hashmap_insert(type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_Schlüssel_DELIM_Zeichenkette_Wert_END.obj_fields, "Schlüssel", &type_descriptor_Zeichenkette);
    hashmap_insert(type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_Schlüssel_DELIM_Zeichenkette_Wert_END.obj_fields, "Wert", &type_descriptor_Zeichenkette);
    // Type descriptor `Speicherbox`
    type_descriptor_Speicherbox.kind = TYPE_ANY_OBJECT;
    type_descriptor_Speicherbox.ptr_count = 0;
    type_descriptor_Speicherbox.list_inner = NULL;
    // Type descriptor `Liste_von_Speicherbox`
    type_descriptor_Liste_von_Speicherbox.kind = TYPE_LIST;
    type_descriptor_Liste_von_Speicherbox.ptr_count = 0;
    type_descriptor_Liste_von_Speicherbox.list_inner = &type_descriptor_Speicherbox;
    // Type descriptor `Unbekannt`
    type_descriptor_Unbekannt.kind = TYPE_NONE;
    type_descriptor_Unbekannt.ptr_count = 0;
    type_descriptor_Unbekannt.list_inner = NULL;
    // Type descriptor `Liste_von_Unbekannt`
    type_descriptor_Liste_von_Unbekannt.kind = TYPE_LIST;
    type_descriptor_Liste_von_Unbekannt.ptr_count = 0;
    type_descriptor_Liste_von_Unbekannt.list_inner = &type_descriptor_Unbekannt;
    // Type descriptor `Fliesskommazahl`
    type_descriptor_Fliesskommazahl.kind = TYPE_FLOAT;
    type_descriptor_Fliesskommazahl.ptr_count = 0;
    type_descriptor_Fliesskommazahl.list_inner = NULL;
    // Type descriptor `Zeiger_auf_Fliesskommazahl`
    type_descriptor_Zeiger_auf_Fliesskommazahl.kind = TYPE_FLOAT;
    type_descriptor_Zeiger_auf_Fliesskommazahl.ptr_count = 1;
    type_descriptor_Zeiger_auf_Fliesskommazahl.list_inner = NULL;
    // Type descriptor `Zeiger_auf_Objekt_BEGIN_Zeichenkette_ZeitSlot_DELIM_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END`
    type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_ZeitSlot_DELIM_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.kind = TYPE_OBJECT;
    type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_ZeitSlot_DELIM_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.ptr_count = 1;
    type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_ZeitSlot_DELIM_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.list_inner = NULL;
    type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_ZeitSlot_DELIM_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields = hashmap_new();
    hashmap_insert(type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_ZeitSlot_DELIM_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields, "ZeitSlot", &type_descriptor_Zeichenkette);
    hashmap_insert(type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_ZeitSlot_DELIM_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields, "Titel", &type_descriptor_Zeichenkette);
    hashmap_insert(type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_ZeitSlot_DELIM_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields, "Beschreibung", &type_descriptor_Zeichenkette);
    hashmap_insert(type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_ZeitSlot_DELIM_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields, "Preis", &type_descriptor_Fliesskommazahl);
    hashmap_insert(type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_ZeitSlot_DELIM_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields, "Protein_GR", &type_descriptor_Fliesskommazahl);
    hashmap_insert(type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_ZeitSlot_DELIM_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields, "Kcal", &type_descriptor_Fliesskommazahl);
    hashmap_insert(type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_ZeitSlot_DELIM_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields, "Fett_GR", &type_descriptor_Fliesskommazahl);
    hashmap_insert(type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_ZeitSlot_DELIM_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields, "Zucker_GR", &type_descriptor_Fliesskommazahl);
    hashmap_insert(type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_ZeitSlot_DELIM_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields, "Kohlenhydrate_GR", &type_descriptor_Fliesskommazahl);
    hashmap_insert(type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_ZeitSlot_DELIM_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields, "ZeitSlot", &type_descriptor_Zeichenkette);
    // Type descriptor `Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END`
    type_descriptor_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.kind = TYPE_OBJECT;
    type_descriptor_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.ptr_count = 0;
    type_descriptor_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.list_inner = NULL;
    type_descriptor_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields = hashmap_new();
    hashmap_insert(type_descriptor_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields, "Titel", &type_descriptor_Zeichenkette);
    hashmap_insert(type_descriptor_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields, "Beschreibung", &type_descriptor_Zeichenkette);
    hashmap_insert(type_descriptor_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields, "Preis", &type_descriptor_Fliesskommazahl);
    hashmap_insert(type_descriptor_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields, "Protein_GR", &type_descriptor_Fliesskommazahl);
    hashmap_insert(type_descriptor_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields, "Kcal", &type_descriptor_Fliesskommazahl);
    hashmap_insert(type_descriptor_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields, "Fett_GR", &type_descriptor_Fliesskommazahl);
    hashmap_insert(type_descriptor_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields, "Zucker_GR", &type_descriptor_Fliesskommazahl);
    hashmap_insert(type_descriptor_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields, "Kohlenhydrate_GR", &type_descriptor_Fliesskommazahl);
    hashmap_insert(type_descriptor_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields, "ZeitSlot", &type_descriptor_Zeichenkette);
    // Type descriptor `Liste_von_Speise`
    type_descriptor_Liste_von_Speise.kind = TYPE_LIST;
    type_descriptor_Liste_von_Speise.ptr_count = 0;
    type_descriptor_Liste_von_Speise.list_inner = &type_descriptor_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END;
    // Type descriptor `Zeiger_auf_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END`
    type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.kind = TYPE_OBJECT;
    type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.ptr_count = 1;
    type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.list_inner = NULL;
    type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields = hashmap_new();
    hashmap_insert(type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields, "Titel", &type_descriptor_Zeichenkette);
    hashmap_insert(type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields, "Beschreibung", &type_descriptor_Zeichenkette);
    hashmap_insert(type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields, "Preis", &type_descriptor_Fliesskommazahl);
    hashmap_insert(type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields, "Protein_GR", &type_descriptor_Fliesskommazahl);
    hashmap_insert(type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields, "Kcal", &type_descriptor_Fliesskommazahl);
    hashmap_insert(type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields, "Fett_GR", &type_descriptor_Fliesskommazahl);
    hashmap_insert(type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields, "Zucker_GR", &type_descriptor_Fliesskommazahl);
    hashmap_insert(type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields, "Kohlenhydrate_GR", &type_descriptor_Fliesskommazahl);
    hashmap_insert(type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields, "ZeitSlot", &type_descriptor_Zeichenkette);
    // Type descriptor `Liste_von_Zeichenkette`
    type_descriptor_Liste_von_Zeichenkette.kind = TYPE_LIST;
    type_descriptor_Liste_von_Zeichenkette.ptr_count = 0;
    type_descriptor_Liste_von_Zeichenkette.list_inner = &type_descriptor_Zeichenkette;
}

void type_descriptor_teardown() {
    // Type descriptor `Objekt_BEGIN_Zeichenkette_Schlüssel_DELIM_Zeichenkette_Wert_END`
    hashmap_free(type_descriptor_Objekt_BEGIN_Zeichenkette_Schlüssel_DELIM_Zeichenkette_Wert_END.obj_fields);
    // Type descriptor `Objekt_BEGIN__END`
    hashmap_free(type_descriptor_Objekt_BEGIN__END.obj_fields);
    // Type descriptor `Zeiger_auf_Objekt_BEGIN_Zeichenkette_Schlüssel_DELIM_Zeichenkette_Wert_END`
    hashmap_free(type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_Schlüssel_DELIM_Zeichenkette_Wert_END.obj_fields);
    // Type descriptor `Zeiger_auf_Objekt_BEGIN_Zeichenkette_ZeitSlot_DELIM_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END`
    hashmap_free(type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_ZeitSlot_DELIM_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields);
    // Type descriptor `Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END`
    hashmap_free(type_descriptor_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields);
    // Type descriptor `Zeiger_auf_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END`
    hashmap_free(type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_DELIM_Fliesskommazahl_Protein_GR_DELIM_Fliesskommazahl_Kcal_DELIM_Fliesskommazahl_Fett_GR_DELIM_Fliesskommazahl_Zucker_GR_DELIM_Fliesskommazahl_Kohlenhydrate_GR_DELIM_Zeichenkette_ZeitSlot_END.obj_fields);
}

void global_variable_setup() {
    // Setup for global variable `MENSA_URL_GENERISCH3`
    DynString* string_temp2 = dynstring_from("https://swp.webspeiseplan.de/index.php?token=55ed21609e26bbf68ba2b19390bf7961&model=menu&location=%d&languagetype=2&_=1699354619713");
    gc_add_to_trace(string_temp2, type_descriptor_Zeichenkette, NULL);
    MENSA_URL_GENERISCH3 = string_temp2;
    // Setup for global variable `KEINE_DATEN5`
    DynString* string_temp4 = dynstring_from("! OBACHT: Keine Daten vorhanden");
    gc_add_to_trace(string_temp4, type_descriptor_Zeichenkette, NULL);
    KEINE_DATEN5 = string_temp4;
}

void cexit(int code) {
    gc_remove_roots(0, (void*[0]){});
    gc_die();
    type_descriptor_teardown();
    __hpi_internal_curl_cleanup();
    exit(code);
}

int main(int argc245, char** argv246) {
    type_descriptor_setup();
    __hpi_internal_init_libSAP(argc245, argv246, true, true);
    global_variable_setup();
    bewerbung();
    einschreibung(__hpi_internal_generate_matrikelnummer());
    studium();
    cexit(0);
    gc_remove_roots(0, (void*[0]){});
}