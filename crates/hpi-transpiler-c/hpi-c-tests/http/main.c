/* Use libcurl to POST JSON data.

Usage: PostJSON <name> <value>

curl-library mailing list thread:
'how do i post json to a https ?'
https://curl.haxx.se/mail/lib-2015-01/0049.html

* Copyright (C) 2015 Jay Satiro <raysatiro@yahoo.com>
https://curl.haxx.se/docs/copyright.html

https://gist.github.com/jay/2a6c54cc6da442489561
*/

/* !checksrc! disable SNPRINTF all */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* https://curl.haxx.se/download.html */
#include <curl/curl.h>


#undef FALSE
#define FALSE 0
#undef TRUE
#define TRUE 1

#ifdef _WIN32
#undef snprintf
#define snprintf _snprintf
#endif


/* Post JSON data to a server.
name and value must be UTF-8 strings.
Returns TRUE on success, FALSE on failure.
*/
int PostJSON()
{
  int retcode = FALSE;
  char *json = NULL;
  CURL *curl = NULL;
  CURLcode res = CURLE_FAILED_INIT;
  char errbuf[CURL_ERROR_SIZE] = { 0, };
  struct curl_slist *headers = NULL;
  char agent[1024] = { 0, };

  curl = curl_easy_init();
  if(!curl) {
    fprintf(stderr, "Error: curl_easy_init failed.\n");
    goto cleanup;
  }

  json = "{ \"numeric_value\": 42 }";

  /* CURLOPT_CAINFO
     To verify SSL sites you may need to load a bundle of certificates.

     You can download the default bundle here:
     https://raw.githubusercontent.com/bagder/ca-bundle/master/ca-bundle.crt

     However your SSL backend might use a database in addition to or instead of
     the bundle.
     https://curl.haxx.se/docs/ssl-compared.html
     */
  curl_easy_setopt(curl, CURLOPT_CAINFO, "curl-ca-bundle.crt");

  snprintf(agent, sizeof(agent), "libcurl/%s",
           curl_version_info(CURLVERSION_NOW)->version);
  agent[sizeof(agent) - 1] = 0;
  curl_easy_setopt(curl, CURLOPT_USERAGENT, agent);

  headers = curl_slist_append(headers, "Expect:");
  headers = curl_slist_append(headers, "Content-Type: application/json");
  curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);

  curl_easy_setopt(curl, CURLOPT_POSTFIELDS, json);
  curl_easy_setopt(curl, CURLOPT_POSTFIELDSIZE, -1L);

  /* This is a test server, it fakes a reply as if the json object were
     created */
  curl_easy_setopt(curl, CURLOPT_URL,
                   "http://jsonplaceholder.typicode.com/posts");

  curl_easy_setopt(curl, CURLOPT_VERBOSE, 1L);
  curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, errbuf);

  res = curl_easy_perform(curl);
  if(res != CURLE_OK) {
    size_t len = strlen(errbuf);
    fprintf(stderr, "\nlibcurl: (%d) ", res);
    if(len)
      fprintf(stderr, "%s%s", errbuf, ((errbuf[len - 1] != '\n') ? "\n" : ""));
    fprintf(stderr, "%s\n\n", curl_easy_strerror(res));
    goto cleanup;
  }

  retcode = TRUE;
cleanup:
  curl_slist_free_all(headers);
  curl_easy_cleanup(curl);
  return retcode;
}

int main()
{

  if(curl_global_init(CURL_GLOBAL_ALL)) {
    fprintf(stderr, "Fatal: The initialization of libcurl has failed.\n");
    return EXIT_FAILURE;
  }

  if(atexit(curl_global_cleanup)) {
    fprintf(stderr, "Fatal: atexit failed to register curl_global_cleanup.\n");
    curl_global_cleanup();
    return EXIT_FAILURE;
  }

  if(!PostJSON()) {
    fprintf(stderr, "Fatal: PostJSON failed.\n");
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
