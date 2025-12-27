#include <sys/mman.h>

fn void wnf_stack_free_all(void) {
  u32 threads = thread_get_count();
  for (u32 i = 0; i < threads; i++) {
    WnfBank *bank = &WNF_BANKS[i];
    if (!bank->stack) {
      continue;
    }
    if (bank->stack_mmap) {
      munmap(bank->stack, bank->stack_bytes);
    } else {
      free(bank->stack);
    }
    bank->stack       = NULL;
    bank->stack_bytes = 0;
    bank->stack_mmap  = 0;
    bank->s_pos       = 0;
  }
}
