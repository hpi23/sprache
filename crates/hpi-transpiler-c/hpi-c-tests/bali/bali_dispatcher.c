#include "../dynstring/dynstring.h"
#include "../hashmap/map.h"
#include "./bali_codegen.h"
#include <assert.h>
#include <dirent.h>
#include <dlfcn.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#define BALI_VERBOSE 0
#define MAKE_VERBOSE 0

// Hashmap<fn-ptr>
HashMap *new_dispatcher(Function *functions, ssize_t num_functions, void *ptr_to_instance, char *class_name, char *class_header_path) {
  // Setup instance name and prefix
  struct timeval tv;
  gettimeofday(&tv, NULL);

  unsigned long long instance = (unsigned long long)(tv.tv_sec) * 1000 + (unsigned long long)(tv.tv_usec) / 1000;

  DynString *LIB_PREFIX = dynstring_from("bali_");
  dynstring_push_fmt(LIB_PREFIX, "%s_instance_", class_name);
  dynstring_push_fmt(LIB_PREFIX, "%lld", instance);

  // Create C file
  char *error;
  FILE *fp;
  DynString *path = dynstring_clone(LIB_PREFIX);

  DynString *c_file = dynstring_from("./runtime/");
  dynstring_push(c_file, LIB_PREFIX);
  dynstring_push_string(c_file, ".c");
  char *path_c = dynstring_as_cstr(c_file);

  if (BALI_VERBOSE) {
    printf("\tGenerating dynamic C code for instance %lld...\n", instance);
  }

  fp = fopen(path_c, "a");

  DynString *file_contents = bali_codegen(functions, num_functions, class_header_path, ptr_to_instance);

  fprintf(fp, "%s", dynstring_as_cstr(file_contents));
  fclose(fp);

  if (BALI_VERBOSE) {
    printf("\tCompiling dispatcher code for instance %lld...\n", instance);
  }

  // Compile dynamic library using `make`
  DynString *command = dynstring_from("make FILE=");
  dynstring_push(command, LIB_PREFIX);
  dynstring_push_string(command, ".c");

  DynString *class_c_file = dynstring_from(class_header_path);
  dynstring_replace(class_c_file, dynstring_from(".h"), dynstring_from(""));
  dynstring_push_string(class_c_file, "_methods.c");
  dynstring_push_fmt(command, " COMPILE_WITH=%s", dynstring_as_cstr(class_c_file));

  char *os_cmd = dynstring_as_cstr(command);

  FILE *cmd = popen(os_cmd, "r");
  char result[10000] = {0x0};
  while (fgets(result, sizeof(result), cmd) != NULL)
    if (MAKE_VERBOSE) {
      printf("\t%s\n", result);
    }
  pclose(cmd);

  if (BALI_VERBOSE) {
    printf("\tCompilation for instance %lld finished.\n", instance);
  }

  // Load dynamic library
  DynString *so_path = dynstring_clone(LIB_PREFIX);
  dynstring_push_string(so_path, ".c.so");

  char *path_so = dynstring_as_cstr(so_path);

  if (BALI_VERBOSE) {
    printf("\tLoading shared object file for instance %lld: (%s)...\n", instance, path_so);
  }

  void *loaded = dlopen(path_so, RTLD_NOW);
  if (!loaded) {
    printf("%s\n", dlerror());
    assert(0);
  }

  // dlclose(loaded);

  HashMap *methods = hashmap_new();

  for (int i = 0; i < num_functions; i++) {
    DynString *method_name = dynstring_from("bali_");
    dynstring_push_string(method_name, functions[i].name);

    char *name_cstr = dynstring_as_cstr(method_name);
    if (BALI_VERBOSE) {
      printf("\tLoading func: %s\n", name_cstr);
    }
    void *(*method_res)(void) = dlsym(loaded, name_cstr);
    if ((error = dlerror()) != NULL) {
      fputs(error, stderr);
      assert(0);
    }

    hashmap_insert(methods, functions[i].name, method_res);
    dynstring_free(method_name);
  }

  if (BALI_VERBOSE) {
    printf("\tShared object file and dispatcher functions for instance %lld loaded.\n", instance);
  }

  return methods;
}
