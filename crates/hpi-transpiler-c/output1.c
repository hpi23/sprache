#include "./libSAP.h"
#include "/home/mik/Coding/hpi/hpi-c-tests/dynstring/dynstring.h"
#include <stdbool.h>

DynString* bewerbung();
void einschreibung(long long int);
void studium();
int main();

int count = 0;

DynString* bewerbung() { return dynstring_new("Hello World!"); }

void einschreibung(long long int Matrikelnummer0) {
    // setup type descriptor
    Reflector type0 = {.kind = KIND_INT, .inner = NULL, .ptr_count = 0 };

    ListNode * descr = list_new();
    list_append(descr, type0);
    // end setup

    __hpi_internal_drucke(descr, Matrikelnummer0);
}

void studium() {  }

int main() {
    bewerbung();
    einschreibung(42);
    studium();
    0;
    return 0;
}
