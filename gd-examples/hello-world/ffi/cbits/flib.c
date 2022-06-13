#include "HsFFI.h"

static void flib_init() __attribute__((constructor));
static void flib_init() {
  static char *argv[] = { "libHShello-world-0.1.0.0-inplace-ghc9.2.2.so", 0 }, **argv_ = argv;
  static int argc = 1;
  hs_init(&argc, &argv_);
}

static void flib_fini() __attribute__((destructor));
static void flib_fini() {
  hs_exit();
}