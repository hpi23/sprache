#include "../dynstring/dynstring.h"
#include <stdint.h>

typedef enum {
  TOK_EOF,
  TOK_SEMICOLON,
  TOK_DQUOTE,
  TOK_SQUOTE,
  TOK_BACK_TICK,
  TOK_PIPE,
  TOK_L_ANGLE_BRACKET,
  TOK_R_ANGLE_BRACKET,
  TOKE_L_PAREN,
  TOKE_R_PAREN,
  TOK_LPAREN,
  TOK_RPAREN,
  TOK_PLUS,
  TOK_MINUS,
  TOK_LT,
  TOK_LTE,
  TOK_GT,
  TOK_GTE,
  TOK_MUL,
  TOK_DIV,
  TOK_DEFINE,
  TOK_LAMBDA,
  TOK_IDENTIFIER,
  TOK_INT,
  TOK_FLOAT,
  TOK_ACCESS,
  TOK_MACRO,
  TOK_AND,
  TOK_DELAY,
  TOK_MAKE_ENVIRONMENT,
  TOK_BEGIN,
  TOK_DO,
  TOK_NAMED_LAMBDA,
  TOK_BKPT,
  TOK_FLUID_LET,
  TOK_OR,
  TOK_CASE,
  TOK_IF,
  TOK_QUASIQUOTE,
  TOK_COND,
  TOK_IN_PACKAGE,
  TOK_QUOTE,
  TOK_DECLARE,
  TOK_LET,
  TOK_SEQUENCE,
  TOK_LET_STAR,
  TOK_SET_STAR,
  TOK_LETREC,
  TOK_UNASSIGNED_QUESTIONMARK,
  TOK_LET_SYNTAX,
  TOK_DEFAULT_OBJECT_QUESTIONMARK,
  TOK_SCODE_QUOTE,
  TOK_CONS_STREAM,
  TOK_THE_ENVIRONMENT,
  TOK_DEFINE_INTEGRABLE,
  TOK_DEFINE_MACRO,
  TOK_LOCAL_DECLARE,
  TOK_USING_SYNTAX,
} TokenKind;

typedef struct {
  TokenKind kind;
  DynString *str_value;
  int64_t int_value;
} Token;

Token token_new(TokenKind kind, DynString *value);
long long token_value_int(Token self);
DynString *token_value(Token self);
void token_print(Token self);
