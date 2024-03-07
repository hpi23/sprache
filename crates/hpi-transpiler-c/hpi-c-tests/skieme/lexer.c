#include "./lexer.h"
#include "span.h"
#include "token.h"
#include <stdlib.h>

void lexer_push(Lexer *self, TokenKind kind, DynString *value) {
  Token *heap_tok = malloc(sizeof(Token));
  *heap_tok = token_new(kind, value);
  list_append(self->output, heap_tok);
}

#define LEXER_PUSH(kind, value) lexer_push(self, kind, value)
#define LEXER_SINGLECHAR_SW(kind)                                              \
  LEXER_PUSH(kind, NULL);                                                      \
  lexer_next(self, false);                                                     \
  break
#define LEXER_CURR self->input[self->loc.index]
#define LEXER_IS_NUMERIC LEXER_CURR >= '0' && LEXER_CURR <= '9'
#define LEXER_IS_IDENT_START                                                   \
  (LEXER_CURR >= 'A' && LEXER_CURR <= 'Z') ||                                  \
      (LEXER_CURR >= 'a' && LEXER_CURR <= 'z')

#define LEXER_IS_IDENT_INNER                                                   \
  (LEXER_IS_IDENT_START) || (LEXER_IS_NUMERIC) || LEXER_CURR == '-'

Lexer *lexer_new(char *input) {
  Lexer *lex = malloc(sizeof(Lexer));
  lex->output = list_new();
  lex->loc = loc_default();
  lex->input = input;

  return lex;
}

void lexer_free(Lexer *self) {
  list_free(self->output);
  free(self);
}

void lexer_next(Lexer *self, bool newline) { loc_next(&self->loc, newline); }

void lexer_number(Lexer *self) {
  DynString *buffer = dynstring_new();

  while (LEXER_IS_NUMERIC) {
    dynstring_push_char(buffer, LEXER_CURR);
    lexer_next(self, false);
  }

  // TODO: handle floats

  DynStringParseInt result = dynstring_parse_int64(buffer);
  if (result.error != NULL) {
    puts(result.error);
    exit(1);
  }

  Token tok = {.kind = TOK_INT, .str_value = NULL, .int_value = result.num};
  Token *heap_tok = malloc(sizeof(Token));
  *heap_tok = tok;
  list_append(self->output, heap_tok);
}

#define LEXER_IDENT_CASE(str, tok)                                             \
  if (dynstring_strcmp_c(buffer, str)) {                                       \
    LEXER_PUSH(tok, NULL);                                                     \
  };                                                                           \
  goto after;

void lexer_ident(Lexer *self) {
  DynString *buffer = dynstring_new();

  while (LEXER_IS_IDENT_INNER) {
    dynstring_push_char(buffer, LEXER_CURR);
    lexer_next(self, false);
  }

  LEXER_IDENT_CASE("access", TOK_ACCESS)
  LEXER_IDENT_CASE("define-syntax", TOK_DEFINE)
  LEXER_IDENT_CASE("macro", TOK_MACRO)
  LEXER_IDENT_CASE("and", TOK_AND)
  LEXER_IDENT_CASE("delay", TOK_DELAY)
  LEXER_IDENT_CASE("make-environment", TOK_MAKE_ENVIRONMENT)
  LEXER_IDENT_CASE("begin", TOK_BEGIN)
  LEXER_IDENT_CASE("do", TOK_DO)
  LEXER_IDENT_CASE("named-lambda", TOK_NAMED_LAMBDA)
  LEXER_IDENT_CASE("bkpt", TOK_BKPT)
  LEXER_IDENT_CASE("fluid-let", TOK_FLUID_LET)
  LEXER_IDENT_CASE("or", TOK_OR)
  LEXER_IDENT_CASE("case", TOK_CASE)
  LEXER_IDENT_CASE("if", TOK_IF)
  LEXER_IDENT_CASE("quasiquote", TOK_QUASIQUOTE)
  LEXER_IDENT_CASE("cond", TOK_COND)
  LEXER_IDENT_CASE("in-package", TOK_IN_PACKAGE)
  LEXER_IDENT_CASE("quote", TOK_QUOTE)
  LEXER_IDENT_CASE("lambda", TOK_LAMBDA)
  LEXER_IDENT_CASE("declare", TOK_DECLARE)
  LEXER_IDENT_CASE("let", TOK_LET)
  LEXER_IDENT_CASE("sequence", TOK_SEQUENCE)
  LEXER_IDENT_CASE("let*", TOK_LET_STAR)
  LEXER_IDENT_CASE("set!", TOK_SET_STAR)
  LEXER_IDENT_CASE("define", TOK_DEFINE)
  LEXER_IDENT_CASE("letrec", TOK_LETREC)
  LEXER_IDENT_CASE("unassigned?", TOK_UNASSIGNED_QUESTIONMARK)
  LEXER_IDENT_CASE("let-syntax", TOK_LET_SYNTAX)
  LEXER_IDENT_CASE("default-object?", TOK_DEFAULT_OBJECT_QUESTIONMARK)
  LEXER_IDENT_CASE("scode-quote", TOK_SCODE_QUOTE)
  LEXER_IDENT_CASE("cons-stream", TOK_CONS_STREAM)
  LEXER_IDENT_CASE("the-environment", TOK_THE_ENVIRONMENT)
  LEXER_IDENT_CASE("define-integrable", TOK_DEFINE_INTEGRABLE)
  LEXER_IDENT_CASE("define-macro", TOK_DEFINE_MACRO)
  LEXER_IDENT_CASE("local-declare", TOK_LOCAL_DECLARE)
  LEXER_IDENT_CASE("using-syntax", TOK_USING_SYNTAX)
  LEXER_IDENT_CASE("define-structure", TOK_USING_SYNTAX)
  LEXER_IDENT_CASE("define", TOK_DEFINE)
  LEXER_IDENT_CASE("lambda", TOK_LAMBDA)

  printf("push ident: %p\n", buffer);
  LEXER_PUSH(TOK_IDENTIFIER, buffer);
after:
  dynstring_free(buffer);
  return;
}

void lexer_with_optional_eq(Lexer *self, TokenKind base, TokenKind with_opt) {
  TokenKind result = base;

  lexer_next(self, false);
  if (LEXER_CURR != '=') {
    goto end;
  }

  result = with_opt;
  lexer_next(self, false);

end:
  LEXER_PUSH(result, NULL);
}

void lexer_scan(Lexer *self) {
  while (1) {
    char curr = LEXER_CURR;
    printf("curr = `%c` (%d)\n", curr, curr);

    // Skip any whitespace or newlines
    if (curr <= 0) {
      LEXER_PUSH(TOK_EOF, NULL);
      goto eof;
    }

    switch (curr) {
    case ' ':
    case '\n':
    case '\t': {
      lexer_next(self, curr == '\n');
      break;
    }
    case '(':
      LEXER_SINGLECHAR_SW(TOK_LPAREN);
    case ')':
      LEXER_SINGLECHAR_SW(TOK_RPAREN);
    case '<':
      lexer_with_optional_eq(self, TOK_LT, TOK_LTE);
      break;
    case '>':
      lexer_with_optional_eq(self, TOK_GT, TOK_GTE);
      break;
    case '+':
      LEXER_SINGLECHAR_SW(TOK_PLUS);
    case '-':
      LEXER_SINGLECHAR_SW(TOK_MINUS);
    case '*':
      LEXER_SINGLECHAR_SW(TOK_MUL);
    case '/':
      LEXER_SINGLECHAR_SW(TOK_DIV);
    default: {
      if (LEXER_IS_NUMERIC) {
        lexer_number(self);
        break;
      }

      if (LEXER_IS_IDENT_START) {
        lexer_ident(self);
        break;
      }

      DynString *e = dynstring_new();
      dynstring_push_fmt(e, "Invalid character `%c` (%d)", curr, (int)curr);
      dynstring_print(e);
      exit(1);
    }
    }
  }

eof:
  return;
}
