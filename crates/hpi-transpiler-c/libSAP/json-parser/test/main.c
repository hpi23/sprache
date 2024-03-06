#include "../../dynstring/dynstring.h"
#include "../parser.h"
#include <stdio.h>

#define TEST_FILE "test.json"

char *read_test_file() {
  FILE *ptr;

  // Using `dynstring` as a dynamic reading buffer
  DynString *string = dynstring_new();

  // Opening file in reading mode
  ptr = fopen(TEST_FILE, "r");

  if (NULL == ptr) {
    printf("File can't be opened \n");
    exit(1);
  }

  // Printing what is written in file
  // character by character using loop.
  char ch;
  do {
    ch = fgetc(ptr);
    dynstring_push_char(string, ch);

    // Checking if character is not EOF.
    // If it is EOF stop reading.
  } while (ch != EOF);

  // Closing the file
  fclose(ptr);

  char * c_str = dynstring_as_cstr(string);
  dynstring_free(string);
  return c_str;
}

int main() {
  char * input = read_test_file();

  // char *input = "{ \"a_key\": 42, \"another\": 3.1415, \"a_string\": \"foo-bar\" }";
  NewJsonParserResult result = parser_new(input);
  if (result.error != NULL) {
    printf("%s\n", result.error);
    exit(1);
  }

  JsonParser parser = result.parser;
  JsonParseResult parse_result = parse_json(&parser);
  if (parse_result.error != NULL) {
    printf("%s\n", parse_result.error);
    exit(2);
  }

  parser_free(&parser);

  json_print_value(parse_result.value);
  char * str = json_value_to_string(parse_result.value);
  printf("%s\n", str);
  free(str);

  json_parse_result_free(parse_result);

  return 0;
}
