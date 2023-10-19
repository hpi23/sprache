#include "/home/mik/Coding/hpi/hpi-c-tests/dynstring/dynstring.h"
#include "/home/mik/Coding/hpi/hpi-c-tests/hashmap/map.h"
#include <assert.h>
#include <curl/curl.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

DynString *body_buf;

size_t curl_write(char *ptr, size_t size, size_t nmemb, void *stream) {
  for (int i = 0; i < size * nmemb; i++) {
    dynstring_push_char(body_buf, ptr[i]); // TODO: this is bad
  }
  return size * nmemb;
}

// Returns the HTTP status code
int64_t __hpi_internal_http(
    DynString *method, // HTTP method
    DynString *url,    // Request URL
    DynString *body,   // Request body
    ListNode *
        headers_input, // contains objects with `key=Schlüssel` and `value=Wert`
    DynString **body_dest // Pointer to read the response body into
) {
  body_buf = *body_dest;
  dynstring_clear(body_buf);

  int retcode = false;
  CURL *curl = NULL;
  CURLcode res = CURLE_FAILED_INIT;
  char errbuf[CURL_ERROR_SIZE] = {
      0,
  };
  struct curl_slist *headers = NULL;
  char *agent = "libSAP/hpi-script1";

  curl = curl_easy_init();
  if (!curl) {
    fprintf(stderr, "Runtime error: curl_easy_init failed.\n");
    exit(-1);
  }

  /* CURLOPT_CAINFO
     To verify SSL sites you may need to load a bundle of certificates.

     You can download the default bundle here:
     https://raw.githubusercontent.com/bagder/ca-bundle/master/ca-bundle.crt

     However your SSL backend might use a database in addition to or instead of
     the bundle.
     https://curl.haxx.se/docs/ssl-compared.html
     */
  // curl_easy_setopt(curl, CURLOPT_CAINFO, "curl-ca-bundle.crt");
  curl_easy_setopt(curl, CURLOPT_USERAGENT, agent);

  ssize_t header_len = list_len(headers_input);
  for (ssize_t i = 0; i < header_len; i++) {
    ListGetResult header_res = list_at(headers_input, i);
    assert(header_res.found);

    HashMap *header = *(HashMap **)header_res.value;

    MapGetResult key_res = hashmap_get(header, "Schlüssel");
    assert(key_res.found);

    MapGetResult value_res = hashmap_get(header, "Wert");
    assert(value_res.found);

    DynString *header_entry = *(DynString **)key_res.value;
    dynstring_push_string(header_entry, ": ");
    dynstring_push(header_entry, *(DynString **)value_res.value);

    headers = curl_slist_append(headers, dynstring_as_cstr(header_entry));
  }

  // printf("headers: `%s`\n", headers->data);

  curl_easy_setopt(curl, CURLOPT_HEADER, false);
  curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);

  char *method_cstr = dynstring_as_cstr(method);
  curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, method_cstr);

  curl_easy_setopt(curl, CURLOPT_POSTFIELDS, dynstring_as_cstr(body));
  curl_easy_setopt(curl, CURLOPT_POSTFIELDSIZE, -1L);

  char *url_cstr = dynstring_as_cstr(url);

  curl_easy_setopt(curl, CURLOPT_URL, url_cstr);

  curl_easy_setopt(curl, CURLOPT_VERBOSE, 0L);
  curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, errbuf);

  char *response;
  curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, curl_write);
  curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

  // dynstring_set(body_buf, response);

  res = curl_easy_perform(curl);

  long long http_code = 0;
  curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

  if (res != CURLE_OK) {
    size_t len = strlen(errbuf);
    fprintf(stderr, "Runtime error: (%d) %s\n", res, errbuf);
    exit(-1);
  }

cleanup:
  curl_slist_free_all(headers);
  curl_easy_cleanup(curl);
  errno = 0;
  return http_code;
}
