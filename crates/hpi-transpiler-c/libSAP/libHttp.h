#pragma once

#include "../hpi-c-tests/dynstring/dynstring.h"
#include "../hpi-c-tests/hashmap/map.h"

// Returns the HTTP status code
int64_t __hpi_internal_http(
    DynString *method, // HTTP method
    DynString *url,    // Request URL
    DynString *body,   // Request body
    ListNode *headers, // contains objects with `key=Schlüssel` and `value=Wert`
    DynString **body_dest // Pointer to read the response body into
);
