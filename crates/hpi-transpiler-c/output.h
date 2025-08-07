#include <stdbool.h>
#include "./libSAP/libTime.h"
#include "./libSAP/libSAP.h"
#include "./libSAP/libList.h"
#include "./libSAP/dynstring/dynstring.h"
#include "./libSAP/libGC.h"

DynString* bewerbung();
void einschreibung(int64_t);
void studium();
void type_descriptor_setup();
void type_descriptor_teardown();
void global_variable_setup();
void cexit(int);
int main(int, char**);

DynString* bewerbung();

void einschreibung(int64_t Matrikelnummer1);

void studium();

void type_descriptor_setup();

void type_descriptor_teardown();

void global_variable_setup();

void cexit(int code);

int main(int argc11, char** argv12);