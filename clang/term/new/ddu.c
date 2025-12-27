// DynDup(lab, val, bod): dynamic DUP binder, strict on lab
// Layout: heap_read(loc+0) = lab, heap_read(loc+1) = val, heap_read(loc+2) = bod
fn Term term_new_ddu(Term lab, Term val, Term bod) {
  u32 loc = heap_alloc(3);
  heap_write(loc + 0, lab);
  heap_write(loc + 1, val);
  heap_write(loc + 2, bod);
  return term_new(0, DDU, 0, loc);
}
