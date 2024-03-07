#include "../dynstring/dynstring.h"
#include "lexer.h"
#include "token.h"
#include <assert.h>
#include <stdio.h>
#include <sys/types.h>

#define TEST_FILE "test.scm"

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

  char *c_str = dynstring_as_cstr(string);
  dynstring_free(string);
  return c_str;
}

int main() {
  char *input = read_test_file();
  //
  Lexer *lexer = lexer_new(input);
  lexer_scan(lexer);

  ListNode *output = lexer->output;

  uint len = list_len(output);
  for (int i = 0; i < len; i++) {
    ListGetResult res = list_at(output, i);
    assert(res.found);
    Token curr = *(Token *)res.value;
    token_print(curr);
  }

  return 0;
}
