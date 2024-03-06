#include "../list.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

// struct ListNode *primfaktoren(int input) {
//   int teiler = 2;
//   struct ListNode *list = list_new();
//   while (teiler <= input) {
//     if (input % teiler == 0) {
//       list_append(list, teiler);
//       input /= teiler;
//     } else {
//       teiler++;
//     }
//   }
//
//   return list;
// }

// void aufgabe_11() {
//   int inputs[] = {
//       60, 1024, 777, 1000, 77, 945252000,
//   };
//
//   for (int i = 0; i < sizeof(inputs) / sizeof(int); i++) {
//     struct ListNode *res = primfaktoren(inputs[i]);
//     printf("%d => \n", inputs[i]);
//     list_print(res);
//     list_delete(res);
//   }
// }

int main(void) {
  ListNode *list = list_new();
  for (int i = 0; i < 3; i++) {
    int *num = malloc(sizeof(num));
    *num = i;
    list_append(list, num);
  }

  list_print(list);
  list_delete_index(list, 0);
  list_print(list);

  list_delete_index(list, 1);
  list_print(list);

  // list_delete_index(list, 0);
  // list_print(list);
  // list_delete_index(list, 0);
  // list_print(list);

  // list_free(list);
}
