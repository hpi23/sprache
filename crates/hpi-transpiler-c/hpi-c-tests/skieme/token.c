#include "./token.h"
#include <assert.h>
#include <stdio.h>

Token token_new(TokenKind kind, DynString *value) {
  Token t = {.kind = kind, .str_value = value};
  return t;
}

void token_free(Token self) { dynstring_free(self.str_value); }

void token_print(Token self) {
  switch (self.kind) {
  case TOK_EOF:
    printf("TOK_EOF\n");
    break;
  case TOK_SEMICOLON:
    printf("TOK_SEMICOLON\n");
    break;
  case TOK_DQUOTE:
    printf("TOK_DQUOTE\n");
    break;
  case TOK_SQUOTE:
    printf("TOK_SQUOTE\n");
    break;
  case TOK_BACK_TICK:
    printf("TOK_BACK_TICK\n");
    break;
  case TOK_PIPE:
    printf("TOK_PIPE\n");
    break;
  case TOK_L_ANGLE_BRACKET:
    printf("TOK_L_ANGLE_BRACKET\n");
    break;
  case TOK_R_ANGLE_BRACKET:
    printf("TOK_R_ANGLE_BRACKET\n");
    break;
  case TOKE_L_PAREN:
    printf("TOKE_L_PAREN\n");
    break;
  case TOKE_R_PAREN:
    printf("TOKE_R_PAREN\n");
    break;
  case TOK_LPAREN:
    printf("TOK_LPAREN\n");
    break;
  case TOK_RPAREN:
    printf("TOK_RPAREN\n");
    break;
  case TOK_PLUS:
    printf("TOK_PLUS\n");
    break;
  case TOK_MINUS:
    printf("TOK_MINUS\n");
    break;
  case TOK_MUL:
    printf("TOK_MUL\n");
    break;
  case TOK_DIV:
    printf("TOK_DIV\n");
    break;
  case TOK_LT:
    printf("TOK_LT\n");
    break;
  case TOK_LTE:
    printf("TOK_LTE\n");
    break;
  case TOK_GT:
    printf("TOK_GT\n");
    break;
  case TOK_GTE:
    printf("TOK_GTE\n");
    break;
  case TOK_DEFINE:
    printf("TOK_DEFINE\n");
    break;
  case TOK_LAMBDA:
    printf("TOK_LAMBDA\n");
    break;
  case TOK_ACCESS:
    printf("TOK_ACCESS\n");
    break;
  case TOK_MACRO:
    printf("TOK_MACRO\n");
    break;
  case TOK_AND:
    printf("TOK_AND\n");
    break;
  case TOK_DELAY:
    printf("TOK_DELAY\n");
    break;
  case TOK_MAKE_ENVIRONMENT:
    printf("TOK_MAKE_ENVIRONMENT\n");
    break;
  case TOK_BEGIN:
    printf("TOK_BEGIN\n");
    break;
  case TOK_DO:
    printf("TOK_DO\n");
    break;
  case TOK_NAMED_LAMBDA:
    printf("TOK_NAMED_LAMBDA\n");
    break;
  case TOK_BKPT:
    printf("TOK_BKPT\n");
    break;
  case TOK_FLUID_LET:
    printf("TOK_FLUID_LET\n");
    break;
  case TOK_OR:
    printf("TOK_OR\n");
    break;
  case TOK_CASE:
    printf("TOK_CASE\n");
    break;
  case TOK_IF:
    printf("TOK_IF\n");
    break;
  case TOK_QUASIQUOTE:
    printf("TOK_QUASIQUOTE\n");
    break;
  case TOK_COND:
    printf("TOK_COND\n");
    break;
  case TOK_IN_PACKAGE:
    printf("TOK_IN_PACKAGE\n");
    break;
  case TOK_QUOTE:
    printf("TOK_QUOTE\n");
    break;
  case TOK_DECLARE:
    printf("TOK_DECLARE\n");
    break;
  case TOK_LET:
    printf("TOK_LET\n");
    break;
  case TOK_SEQUENCE:
    printf("TOK_SEQUENCE\n");
    break;
  case TOK_LET_STAR:
    printf("TOK_LET_STAR\n");
    break;
  case TOK_SET_STAR:
    printf("TOK_SET_STAR\n");
    break;
  case TOK_LETREC:
    printf("TOK_LETREC\n");
    break;
  case TOK_UNASSIGNED_QUESTIONMARK:
    printf("TOK_UNASSIGNED_QUESTIONMARK\n");
    break;
  case TOK_LET_SYNTAX:
    printf("TOK_LET_SYNTAX\n");
    break;
  case TOK_DEFAULT_OBJECT_QUESTIONMARK:
    printf("TOK_DEFAULT_OBJECT_QUESTIONMARK\n");
    break;
  case TOK_SCODE_QUOTE:
    printf("TOK_SCODE_QUOTE\n");
    break;
  case TOK_CONS_STREAM:
    printf("TOK_CONS_STREAM\n");
    break;
  case TOK_THE_ENVIRONMENT:
    printf("TOK_THE_ENVIRONMENT\n");
    break;
  case TOK_DEFINE_INTEGRABLE:
    printf("TOK_DEFINE_INTEGRABLE\n");
    break;
  case TOK_DEFINE_MACRO:
    printf("TOK_DEFINE_MACRO\n");
    break;
  case TOK_LOCAL_DECLARE:
    printf("TOK_LOCAL_DECLARE\n");
    break;
  case TOK_USING_SYNTAX:
    printf("TOK_USING_SYNTAX\n");
    break;
  case TOK_IDENTIFIER: {
    assert(self.str_value != NULL);
    dynstring_print(self.str_value);
    break;
  }
  case TOK_INT: {
    printf("%ld\n", self.int_value);
    break;
  }
  default:
    printf("%d\n", self.kind);
    assert(0 && "Invalid token kind");
  }
}
