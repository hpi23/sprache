#include "parser.h"
#include "lexer.h"
#include "token.h"
#include <assert.h>
#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../hashmap/map.h"
#include "json.h"

void parser_free_token(Token token) {
  switch (token.kind) {
  case TOKENKIND_STRING:
  case TOKENKIND_INT:
  case TOKENKIND_FLOAT:
  case TOKENKIND_BOOL:
  case TOKENKIND_NULL:
    free(token.value);
  default:
    // no need to free tokens which are not heap-allocated
    return;
  }
}

void parser_free(JsonParser *parser) {
  parser_free_token(parser->curr_tok);
  lexer_free(&parser->lexer);
}

char *parser_next(JsonParser *parser) {
  parser_free_token(parser->curr_tok);
  TokenResult result = lexer_next_token(&parser->lexer);

  parser->curr_tok = result.token;

  if (result.error != NULL) {
    return result.error;
  }

  return NULL;
}

NewJsonParserResult parser_new(char *input) {
  NewJsonParserResult result = {.error = NULL};

  Lexer lexer = lexer_new(input);
  JsonParser parser = {.lexer = lexer};
  char *err = parser_next(&parser);
  if (err != NULL) {
    result.error = err;
    return result;
  }

  result.parser = parser;
  return result;
}

JsonParseResult parse_json(JsonParser *parser, void*(allocator)(TypeDescriptor)) {
  JsonParseResult result = {.error = NULL};

  switch (parser->curr_tok.kind) {
  case TOKENKIND_LBRACE: {
    ParseResultObject obj_result = parse_object(parser, allocator);
    if (obj_result.error != NULL) {
      result.error = obj_result.error;
      return result;
    }
    result.value.type = JSON_TYPE_OBJECT;
    result.value.object = obj_result.value;
    break;
  }
  case TOKENKIND_LBRACKET: {
    ParseResultArray array_result = parser_parse_array(parser, allocator);
    if (array_result.error != NULL) {
      result.error = array_result.error;
      return result;
    }
    result.value.type = JSON_TYPE_ARRAY;
    result.value.array = array_result.value;
    break;
  }
  case TOKENKIND_INT: {
    result.value.type = JSON_TYPE_INT;
    char *remaining_string;
    result.value.num_int = strtoll(parser->curr_tok.value, &remaining_string, 10);
    if (strlen(remaining_string) != 0 || errno != 0) {
      asprintf(&result.error, "Error: integer `%s` parse error", parser->curr_tok.value);
      return result;
    }

    char *error = parser_next(parser);
    if (error != NULL) {
      result.error = error;
      return result;
    }

    break;
  }
  case TOKENKIND_FLOAT: {
    result.value.type = JSON_TYPE_FLOAT;
    char *remaining_string;
    result.value.num_float = strtold(parser->curr_tok.value, &remaining_string);
    if (strlen(remaining_string) != 0 || errno != 0) {
      asprintf(&result.error, "Error: float `%s` parse error", parser->curr_tok.value);
      return result;
    }

    char *error = parser_next(parser);
    if (error != NULL) {
      result.error = error;
      return result;
    }

    break;
  }
  case TOKENKIND_STRING: {
    result.value.type = JSON_TYPE_STRING;
    result.value.string = malloc(strlen(parser->curr_tok.value) + 1);
    strcpy(result.value.string, parser->curr_tok.value);

    char *error = parser_next(parser);
    if (error != NULL) {
      result.error = error;
      return result;
    }

    break;
  }
    // TODO: do not do this validation anymore
  case TOKENKIND_NULL: {
    result.value.type = JSON_TYPE_NULL;

    if (strcmp(parser->curr_tok.value, "null") != 0) {
      asprintf(&result.error, "Error: expected `null`, got `%s`", parser->curr_tok.value);
      return result;
    }

    char *error = parser_next(parser);
    if (error != NULL) {
      result.error = error;
      return result;
    }

    break;
  }
    // TODO: do not do this validation anymore
  case TOKENKIND_BOOL: {
    result.value.type = JSON_TYPE_BOOL;

    if (strcmp(parser->curr_tok.value, "true") == 0) {
      result.value.boolean = true;
    } else if (strcmp(parser->curr_tok.value, "false") == 0) {
      result.value.boolean = false;
    } else {
      asprintf(&result.error, "Error: expected either `true` or `false`, got `%s`", parser->curr_tok.value);
      return result;
    }

    char *error = parser_next(parser);
    if (error != NULL) {
      result.error = error;
      return result;
    }

    break;
  }
  default:
    asprintf(&result.error, "Error: expected JSON value, got `%s`", display_tokenkind(parser->curr_tok.kind));
    return result;
  }

  return result;
}

ParseResultObjectField parse_object_field(JsonParser *parser, void*(allocator)(TypeDescriptor)) {
  ParseResultObjectField result = {.error = NULL};

  // expect a key (string)
  if (parser->curr_tok.kind != TOKENKIND_STRING) {
    asprintf(&result.error, "Error: expected `STRING`, got `%s`", display_tokenkind(parser->curr_tok.kind));
    return result;
  }

  result.key = malloc(strlen(parser->curr_tok.value) + 1);
  strcpy(result.key, parser->curr_tok.value);

  char *error = parser_next(parser);
  if (error != NULL) {
    result.error = error;
    return result;
  }

  // expect a `:`
  if (parser->curr_tok.kind != TOKENKIND_COLON) {
    asprintf(&result.error, "Error: expected `COLON`, got `%s`", display_tokenkind(parser->curr_tok.kind));
    return result;
  }

  error = parser_next(parser);
  if (error != NULL) {
    result.error = error;
    return result;
  }

  JsonParseResult value_res = parse_json(parser, allocator);
  if (value_res.error != NULL) {
    result.error = value_res.error;
    return result;
  }
  result.value = malloc(sizeof(JsonValue));
  *result.value = value_res.value;
  return result;
}

ParseResultObject parse_object(JsonParser *parser, void*(allocator)(TypeDescriptor)) {
  ParseResultObject result = {.error = NULL};

  // Skip the `{`
  char *err = parser_next(parser);
  if (err != NULL) {
    result.error = err;
    return result;
  }

  HashMap *field_map = hashmap_new();

  if (parser->curr_tok.kind != TOKENKIND_RBRACE && parser->curr_tok.kind != TOKENKIND_EOF) {
    // make initial field
    ParseResultObjectField field_res = parse_object_field(parser, allocator);
    if (field_res.error != NULL) {
      result.error = field_res.error;
      return result;
    }
    hashmap_insert(field_map, field_res.key, field_res.value);
    free(field_res.key);

    while (parser->curr_tok.kind == TOKENKIND_COMMA) {
      // skip comma
      char *err = parser_next(parser);
      if (err != NULL) {
        result.error = err;
        return result;
      }

      // prevent a trailing comma
      if (parser->curr_tok.kind == TOKENKIND_RBRACE) {
        result.error = "Error: trailing comma is not allowed";
        return result;
      }

      if (parser->curr_tok.kind == TOKENKIND_EOF) {
        break;
      }

      // make other fields
      ParseResultObjectField field_res = parse_object_field(parser, allocator);
      if (field_res.error != NULL) {
        result.error = field_res.error;
        return result;
      }
      hashmap_insert(field_map, field_res.key, field_res.value);
      free(field_res.key);
    }
  }

  if (parser->curr_tok.kind != TOKENKIND_RBRACE) {
    asprintf(&result.error, "Error: expected token `%s` at position %ld, found `%s` (%s)", display_tokenkind(TOKENKIND_RBRACE),
             parser->lexer.curr_loc.index, display_tokenkind(parser->curr_tok.kind), parser->curr_tok.value);
    return result;
  }

  // skip the `}`
  err = parser_next(parser);
  if (err != NULL) {
    result.error = err;
    return result;
  }

  JsonValueObject object = {.fields = field_map};
  result.value = object;
  return result;
}

ParseResultArray parser_parse_array(JsonParser *parser, void*(allocator)(TypeDescriptor)) {
  JsonValueArray array = {.fields = list_new()};
  ParseResultArray result = {.value = array, .error = NULL};

  // skip the `[`
  char *err = parser_next(parser);
  if (err != NULL) {
    result.error = err;
    return result;
  }

  if (parser->curr_tok.kind != TOKENKIND_RBRACKET && parser->curr_tok.kind != TOKENKIND_EOF) {
    // make initial value
    JsonParseResult value_result = parse_json(parser, allocator);
    if (value_result.error != NULL) {
      result.error = value_result.error;
      return result;
    }

    JsonValue *value = (JsonValue *)malloc(sizeof(JsonValue));
    *value = value_result.value;
    list_append(array.fields, value);

    while (parser->curr_tok.kind == TOKENKIND_COMMA) {
      // skip the `,`
      char *err = parser_next(parser);
      if (err != NULL) {
        result.error = err;
        return result;
      }

      // prevent a trailing comma
      if (parser->curr_tok.kind == TOKENKIND_RBRACKET) {
        result.error = "Error: trailing comma is not allowed";
        return result;
      }

      if (parser->curr_tok.kind == TOKENKIND_EOF) {
        break;
      }

      // make the value
      JsonParseResult value_result = parse_json(parser, allocator);
      if (value_result.error != NULL) {
        result.error = value_result.error;
        return result;
      }

      JsonValue *value = (JsonValue *)malloc(sizeof(JsonValue));
      *value = value_result.value;
      list_append(array.fields, value);
    }
  }

  if (parser->curr_tok.kind != TOKENKIND_RBRACKET) {
    asprintf(&result.error, "Error: expected token `%s` at position %ld, found `%s`", display_tokenkind(TOKENKIND_RBRACKET),
             parser->lexer.curr_loc.index, display_tokenkind(parser->curr_tok.kind));
    return result;
  }

  // skip the `]`
  err = parser_next(parser);
  if (err != NULL) {
    result.error = err;
    return result;
  }

  return result;
}

//
// HELPER FUNCTIONS
//

void json_parse_result_free(JsonParseResult result) {
  if (result.error != NULL) {
    free(result.error);
  }
  json_value_free(result.value);
}
