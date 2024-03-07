#pragma once

#include "dynstring/dynstring.h"
#include "hashmap/map.h"
#include "reflection.h"

// Returns the HTTP status code
int64_t __hpi_internal_http(DynString *method,       // HTTP method
                            DynString *url,          // Request URL
                            DynString *body,         // Request body
                            ListNode *headers_input, // contains objects with `key=Schlüssel` and `value=Wert`
                            DynString **body_dest,   // Pointer to read the response body into
                            void(trace_alloc)(void *addr, TypeDescriptor type, TypeDescriptor *type_heap));

void __hpi_inernal_curl_cleanup();
