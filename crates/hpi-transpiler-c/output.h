#include "./libSAP/libSAP.h"
#include "./libSAP/dynstring/dynstring.h"
#include "./libSAP/libHttp.h"
#include "./libSAP/libString.h"
#include "./libSAP/libList.h"
#include <stdbool.h>
#include <assert.h>
#include "./libSAP/libTime.h"
#include "./libSAP/libGC.h"
#include "./libSAP/libJson.h"

// Type definitions for runtime 'reflection'
TypeDescriptor type_descriptor_Zeichenkette;
TypeDescriptor type_descriptor_Liste_von_Speise;
TypeDescriptor type_descriptor_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_END;
TypeDescriptor type_descriptor_Fliesskommazahl;
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
TypeDescriptor type_descriptor_Zeiger_auf_Fliesskommazahl;
TypeDescriptor type_descriptor_Zeiger_auf_Objekt_BEGIN_Zeichenkette_Titel_DELIM_Zeichenkette_Beschreibung_DELIM_Fliesskommazahl_Preis_END;

DynString* MENSA_URL1;

void Aktualisiere0();
ListNode* Lade_Speiseangebot1();
DynString* bewerbung();
void einschreibung(int64_t);
void studium();
void type_descriptor_setup();
void type_descriptor_teardown();
void global_variable_setup();
void cexit(int);
int64_t lib_main(int, char**);

void Aktualisiere0();

ListNode* Lade_Speiseangebot1();

DynString* bewerbung();

void einschreibung(int64_t Matrikelnummer103);

void studium();

void type_descriptor_setup();

void type_descriptor_teardown();

void global_variable_setup();

void cexit(int code);

int64_t lib_main(int argc105, char** argv106);