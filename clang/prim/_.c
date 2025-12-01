// Primitive name constants (computed from name encoding)
#define PRIM_ADD 4356

// Primitive implementations
#include "add.c"

// Dispatch a primitive call
fn Term prim_call(u32 nam, u32 ari, u32 loc) {
  switch (nam) {
    case PRIM_ADD: {
      if (ari != 2) {
        fprintf(stderr, "@@add: expected 2 arguments\n");
        exit(1);
      }
      return prim_add(HEAP[loc + 0], HEAP[loc + 1]);
    }
    default: {
      fprintf(stderr, "unknown primitive\n");
      exit(1);
    }
  }
}
