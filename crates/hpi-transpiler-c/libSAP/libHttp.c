#include "/home/mik/Coding/hpi/hpi-c-tests/dynstring/dynstring.h"
#include "/home/mik/Coding/hpi/hpi-c-tests/hashmap/map.h"
#include <stdio.h>

// Returns the HTTP status code
int64_t __hpi_internal_http(
    DynString *method, // HTTP method
    DynString *url,    // Request URL
    DynString *body,   // Request body
    ListNode *headers, // contains objects with `key=Schlüssel` and `value=Wert`
    DynString **body_dest // Pointer to read the response body into
) {
    exit(1);
    printf("Would do HTTP request here\n");
    return -1;
}
