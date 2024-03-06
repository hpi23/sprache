#include "../dynstring.h"
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

int main() {
  // DynString *string = dynstring_from("Hello World");
  // dynstring_print(string);
  // dynstring_push_char(string, '!');
  // dynstring_print(string);
  // dynstring_push_string(string, " Mom");
  // dynstring_print(string);
  //
  // dynstring_push_string(string, " Dad!");
  // char *temp = dynstring_as_cstr(string);
  // printf("%s\n", temp);
  // free(temp);
  //
  // dynstring_push_fmt(string, "%d", 42);
  // dynstring_print(string);
  //
  // dynstring_set(string, ".");
  // dynstring_print(string);
  // dynstring_repeat(string, 4);
  // dynstring_print(string);
  // dynstring_repeat(string, 0);
  // dynstring_print(string);
  //
  // dynstring_set(string, "Hello World!");
  // dynstring_print(string);
  // dynstring_clear(string);
  // // char *c_str = dynstring_as_cstr(string);
  //
  // dynstring_free(string);
  //
  // DynString *num_int = dynstring_from("42a");
  // DynStringParseInt res1 = dynstring_parse_int64(num_int);
  // if (res1.error != NULL) {
  //   printf("%s\n", res1.error);
  //   free(res1.error);
  // }
  // printf("int parse result: %ld\n", res1.num);
  // dynstring_free(num_int);
  //
  // DynString *num_float = dynstring_from("3.1415");
  // DynStringParseDouble res2 = dynstring_parse_double(num_float);
  // if (res2.error != NULL) {
  //   printf("%s\n", res2.error);
  //   free(res2.error);
  // }
  // printf("double parse result: %f\n", res2.num);
  // dynstring_free(num_float);
  //
  // DynString *base = dynstring_from("CONCAT: Hello");
  // DynString *add = dynstring_from(" World!");
  //
  // dynstring_push(base, add);
  // dynstring_print(base);
  // dynstring_free(base);
  // dynstring_free(add);
  //
  // DynString *left = dynstring_from("Foo");
  // DynString *right = dynstring_from("Foo");
  //
  // bool cmp_res = dynstring_strcmp(left, right);
  // printf("Comparison result: %d\n", cmp_res);
  //
  // dynstring_free(left);
  // dynstring_free(right);
  //
  // DynString *to_split = dynstring_from("1=2=3=4=5");
  // DynString *where = dynstring_from("=");
  // ListNode *split = dynstring_split(to_split, where, 2);
  //
  // for (int i = 0; i < list_len(split); i++) {
  //   DynString *curr = list_at(split, i).value;
  //   char *out = dynstring_as_cstr(curr);
  //   dynstring_free(curr);
  //   printf("`%s`\n", out);
  //   free(out);
  // }
  //
  // list_free(split);
  // dynstring_free(to_split);
  // dynstring_free(where);
  //
  // printf("=== Split 2 ===\n");
  //
  // to_split = dynstring_from("MAKEFLAGS=");
  // where = dynstring_from("=");
  //
  // split = dynstring_split(to_split, where, 1);
  //
  // for (int i = 0; i < list_len(split); i++) {
  //   DynString *curr = list_at(split, i).value;
  //   char *out = dynstring_as_cstr(curr);
  //   printf("`%s`\n", out);
  //   free(out);
  //   dynstring_free(curr);
  // }
  //
  // dynstring_free(to_split);
  // dynstring_free(where);
  //
  // list_free(split);
  //
  // DynString *replace = dynstring_from("Hello Mom, Hello Dad!");
  //
  // DynString *what = dynstring_from("ll");
  // DynString *with = dynstring_from("XYZ");
  //
  // dynstring_replace(replace, what, with);
  // dynstring_print(replace);
  // dynstring_free(replace);
  // dynstring_free(what);
  // dynstring_free(with);
  //
  // return 0;

  // DynString *to_split = dynstring_from("Relevo Schale");
  // DynString *what = dynstring_from("vo");
  // ListNode *res = dynstring_split(to_split, what, 0);
  //
  // // dynstring_free(what, to_split);
  //
  // for (int i = 0; i < list_len(res); i++) {
  //   ListGetResult curr = list_at(res, i);
  //   char *str = dynstring_as_cstr(curr.value);
  //   printf("SPLIT: `%s`\n", str);
  //   free(str);
  //   dynstring_free(curr.value);
  // }

  // DynString * print_test = dynstring_from("große Schale, Relevo Schale");
  // bool cont = dynstring_contains(print_test, dynstring_from("vo"));
  // printf("%d\n", cont);

  DynString *foo = dynstring_from("hi");
  dynstring_repeat(foo, 3);
  dynstring_print(foo);
  dynstring_free(foo);

  return 0;
}
