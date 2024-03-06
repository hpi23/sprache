#include "./lexer.h"
#include "../dynstring/dynstring.h"
#include "token.h"
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

static inline bool lexer_char_is_ascii_digit(char input) { return input >= 0x30 && input <= 0x39; }
static inline bool lexer_char_is_ascii_alphabetic(char input) { return input >= 0x61 && input <= 0x7a; }

void lexer_advance(Lexer *lexer) {
  lexer->curr_char = lexer->input[lexer->curr_loc.index]; // TODO: is this OK?
  lexer->curr_loc.index++;
}

Lexer lexer_new(char *input) {
  int input_len = strlen(input);
  Location loc = {.index = 0};
  Lexer lexer = {.input = input, .curr_loc = loc, .curr_char = '?'};
  lexer_advance(&lexer);
  return lexer;
}

void lexer_free(Lexer *lexer) { free(lexer->input); }

// TODO: improve this: (use make ident)

TokenResult lexer_make_bool(Lexer *lexer) {
  Token bool_tok = {.kind = TOKENKIND_BOOL, .value = NULL};
  TokenResult result = {.error = NULL, .token = bool_tok};

  ssize_t bool_start_idx = lexer->curr_loc.index;
  while (lexer_char_is_ascii_alphabetic(lexer->curr_char)) {
    lexer_advance(lexer);
  }

  ssize_t inner_str_len = lexer->curr_loc.index - bool_start_idx + 1;
  result.token.value = malloc(sizeof(char) * inner_str_len + 1);
  memcpy(result.token.value, &lexer->input[bool_start_idx - 1], inner_str_len);
  result.token.value[inner_str_len - 1] = '\0';

  return result;
}

TokenResult lexer_make_null(Lexer *lexer) {
  Token null_tok = {.kind = TOKENKIND_NULL, .value = NULL};
  TokenResult result = {.error = NULL, .token = null_tok};

  ssize_t null_start_idx = lexer->curr_loc.index;
  while (lexer_char_is_ascii_alphabetic(lexer->curr_char)) {
    lexer_advance(lexer);
  }

  ssize_t inner_str_len = lexer->curr_loc.index - null_start_idx + 1;
  result.token.value = malloc(sizeof(char) * inner_str_len + 1);
  memcpy(result.token.value, &lexer->input[null_start_idx - 1], inner_str_len);
  result.token.value[inner_str_len - 1] = '\0';

  return result;
}

TokenResult lexer_make_string(Lexer *lexer) {
  Token string_tok = {.kind = TOKENKIND_STRING, .value = NULL};
  TokenResult result = {.error = NULL, .token = string_tok};
  // skip opening quote
  lexer_advance(lexer);

  DynString *out = dynstring_new();

  while (lexer->curr_char != '"' && lexer->curr_char != '\0') {
    if (lexer->curr_char == '\\') {
      lexer_advance(lexer);

      switch (lexer->curr_char) {
      case '"': // String quote escape
        dynstring_push_char(out, '"');
        lexer_advance(lexer);
        continue;
      case 'u': { // Unicode escape
        uint16_t n = 0;

        for (int i = 0; i < 4; i++) {
          lexer_advance(lexer);
          if (lexer->curr_char > 0) {
            n = (n << 4) + lexer->curr_char;
          } else {
            asprintf(&result.error, "Error: invalid escape sequence, found `%c` before end of sequence", lexer->curr_char);
            return result;
          }
        }

        continue;
      }
      case 'n':
        dynstring_push_char(out, '\n');
        lexer_advance(lexer);
        continue;
      case '/':
        dynstring_push_char(out, '/');
        lexer_advance(lexer);
        continue;
      default:
        asprintf(&result.error, "Error: expected escape sequence, found `%c`", lexer->curr_char);
        return result;
      }
    }

    dynstring_push_char(out, lexer->curr_char);
    lexer_advance(lexer);
  }

  // skip closing quote
  if (lexer->curr_char != '"') {
    asprintf(&result.error, "Error: missing closing quote: expected `\"`, found `%c`", lexer->curr_char);
    return result;
  }
  lexer_advance(lexer);

  result.token.value = dynstring_as_cstr(out);
  dynstring_free(out);

  return result;
}

TokenResult lexer_make_number(Lexer *lexer) {
  Token num_tok = {.kind = TOKENKIND_INT, .value = NULL};
  TokenResult result = {.error = NULL, .token = num_tok};

  ssize_t num_start_idx = lexer->curr_loc.index;

  bool is_float = false;

  ssize_t prev_idx = 0;

  while (lexer_char_is_ascii_digit(lexer->curr_char)) {
    prev_idx = lexer->curr_loc.index;
    lexer_advance(lexer);
  }

  if (lexer->curr_char == '.') {
    is_float = true;
    lexer_advance(lexer);

    while (lexer_char_is_ascii_digit(lexer->curr_char) && lexer->curr_char != '\0') {
      prev_idx = lexer->curr_loc.index;
      lexer_advance(lexer);
    }
  }

  ssize_t num_str_len = prev_idx - num_start_idx + 1;
  result.token.value = malloc(sizeof(char) * num_str_len + 1);
  memcpy(result.token.value, &lexer->input[num_start_idx - 1], num_str_len);
  result.token.value[num_str_len] = '\0';

  if (is_float) {
    result.token.kind = TOKENKIND_FLOAT;
  }

  return result;
}

TokenResult lexer_next_token(Lexer *lexer) {
  TokenResult result = {.error = NULL};

  // EoF is reached, stop here | IS this OK
  // TODO: this is required for the mensa Griebnitzsee API, but why?
  if (lexer->curr_loc.index >= strlen(lexer->input) + 1) {
    return result;
  }

  while (lexer->curr_char == ' ' || lexer->curr_char == '\n' || lexer->curr_char == '\t' || lexer->curr_char == '\r') {
    lexer_advance(lexer);
  }

  switch (lexer->curr_char) {
  case '{':
    result.token.kind = TOKENKIND_LBRACE;
    result.token.value = "{";
    break;
  case '}':
    result.token.kind = TOKENKIND_RBRACE;
    result.token.value = "}";
    break;
  case '[':
    result.token.kind = TOKENKIND_LBRACKET;
    result.token.value = "[";
    break;
  case ']':
    result.token.kind = TOKENKIND_RBRACKET;
    result.token.value = "]";
    break;
  case ':':
    result.token.kind = TOKENKIND_COLON;
    result.token.value = ":";
    break;
  case ',':
    result.token.kind = TOKENKIND_COMMA;
    result.token.value = ",";
    break;
  case '"':
    return lexer_make_string(lexer);
  case 't':
  case 'f':
    return lexer_make_bool(lexer);
  case 'n':
    return lexer_make_null(lexer);
  case '\0':
  case -1:
    result.token.kind = TOKENKIND_EOF;
    result.token.value = "EOF";
    break;
  default:
    if (lexer_char_is_ascii_digit(lexer->curr_char)) {
      return lexer_make_number(lexer);
    }

    asprintf(&result.error, "Error: illegal character at position %ld (%d) '%d'", lexer->curr_loc.index, lexer->curr_char, lexer->curr_char);
    return result;
  }

  lexer_advance(lexer);
  return result;
}
