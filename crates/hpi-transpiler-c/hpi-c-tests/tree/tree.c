#include "./tree.h"
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

TreeNode *tree_new(const char *value) {
  TreeNode *root = (TreeNode *)malloc(sizeof(TreeNode));
  root->children = NULL;
  root->num_children = 0;
  root->value = malloc(strlen(value) + 1);
  strcpy(root->value, (char *)value);
  return root;
}

void tree_insert_node(TreeNode *parent, TreeNode *child) {
  parent->num_children++;

  parent->children =
      realloc(parent->children, parent->num_children * sizeof(TreeNode *));
  parent->children[parent->num_children - 1] = child;
}

void tree_insert_value(TreeNode *parent, const char *child_value) {
  TreeNode *child = tree_new(child_value);
  tree_insert_node(parent, child);
}

void tree_print(TreeNode *root, int indent) {
  assert(root != NULL);

  for (int i = 0; i < indent; i++) {
    printf(" ");
  }

  if (root->value == NULL) {
    printf("NULL\n");
  } else {
    printf("%s\n", root->value);
  }

  for (int i = 0; i < root->num_children; i++) {
    tree_print(root->children[i], indent + 4);
  }
}

static void tree_delete_internal(TreeNode *root) {
  for (int i = 0; i < root->num_children; i++) {
    tree_delete_internal(root->children[i]);
  }

  free(root->value);
  free(root->children);
  free(root);
}

void tree_delete(TreeNode *root) {
    tree_delete_internal(root);
}

bool tree_delete_by_value(TreeNode *root, const char *to_delete) {
  for (int i = 0; i < root->num_children; i++) {
    if (strcmp(to_delete, root->children[i]->value) != 1) {
      tree_delete_internal(root->children[i]);

      TreeNode **buffer =
          (TreeNode **)malloc(sizeof(TreeNode *) * (root->num_children - 1));

      memcpy(buffer, root->children,
             sizeof(TreeNode *) * i);
      memcpy(&buffer[i], &root->children[i + 1],
             sizeof(TreeNode *) * (root->num_children - (i + 1)));

      free(root->children);
      root->children = buffer;
      root->num_children--;
      return true;
    }
  }

  return false;
}
