#pragma once
#include "../hpi-c-tests/dynstring/dynstring.h"
#include "../hpi-c-tests/hashmap/map.h"
#include "./libAnyObj.h"
#include <stdio.h>

void __hpi_internal_libSAP_reset();
void __hpi_internal_print(ssize_t num_args, ...);
int64_t __hpi_internal_generate_matrikelnummer();
DynString *__hpi_internal_fmt(ssize_t num_args, DynString *fmt, ...);
void __hpi_internal_sleep(double duration);
AnyObject *__hpi_internal_env();
ListNode *__hpi_internal_args();
void __hpi_internal_init_libSAP(size_t p_argc, char ** p_argv);
