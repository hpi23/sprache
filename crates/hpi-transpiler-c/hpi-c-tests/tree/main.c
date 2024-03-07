#include "./tree.h"
#include <assert.h>

int main(void) {
  TreeNode *tree = tree_new("Vater");

  TreeNode *sohn2 = tree_new("Sohn 2");
  tree_insert_value(sohn2, "Sohn Kind 1");

  tree_insert_value(tree, "Sohn 1");
  tree_insert_node(tree, sohn2);
  tree_insert_value(tree, "Sohn 3");
  tree_print(tree, 0);

  assert(tree_delete_by_value(tree, "Sohn 2"));

  tree_print(tree, 0);
  tree_delete(tree);

  return 0;
}
