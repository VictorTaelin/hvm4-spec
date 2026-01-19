fn Term wnf_pri(Term pri) {
  u32 prim_id = term_ext(pri);
  Term (*fun)(Term *args) = prim_fun(prim_id);
  u32 arity = prim_arity(prim_id);

  if (fun == NULL || arity == 0) {
    char *prim_name = table_get(prim_id);
    if (prim_name != NULL) {
      fprintf(stderr, "RUNTIME_ERROR: unknown primitive '%%%s'\n", prim_name);
    } else {
      fprintf(stderr, "RUNTIME_ERROR: unknown primitive id %u\n", prim_id);
    }
    exit(1);
  }

  u32  loc = term_val(pri);
  Term args_stack[16];
  Term *args = args_stack;
  if (arity > 16) {
    args = malloc(arity * sizeof(Term));
    if (args == NULL) {
      fprintf(stderr, "RUNTIME_ERROR: primitive args allocation failed\n");
      exit(1);
    }
  }

  for (u32 i = 0; i < arity; i++) {
    args[i] = heap_read(loc + i);
  }

  Term res = fun(args);
  if (args != args_stack) {
    free(args);
  }
  return res;
}
