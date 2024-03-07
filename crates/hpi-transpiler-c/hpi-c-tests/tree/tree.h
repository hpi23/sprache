#include <stdlib.h>
#include <stdbool.h>

typedef struct TreeNode {
  char *value;
  struct TreeNode **children;
  ssize_t num_children;
} TreeNode;

TreeNode *tree_new(const char *value);
void tree_insert_node(TreeNode *parent, TreeNode *child);
void tree_insert_value(TreeNode *parent, const char *child_value);
void tree_print(TreeNode *root, int indent);
void tree_delete(TreeNode *root);
bool tree_delete_by_value(TreeNode *root, const char *to_delete);
