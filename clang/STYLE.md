# Style Guide

Abide to the guidelines below strictly!

## NEVER write single-line ifs, loops, statements, functions.

Don't:
```c
if { ... }
while { ... }
u32 foo(x) { ... }
foo(); bar();
```

Do:
```c
if {
  ...
}
while {
  ...
}
u32 foo(x) {
  ...
}
foo();
bar();
```

## ALWAYS use switch for Term pattern matching.

Don't:
```c
if (tag == FOO) {
  ...
} else if (tag == BAR) {
  ...
} ...
```

Do:
```c
switch (tag) {
  case FOO: {
    ...
  }
  case BAR: {
    ...
  }
}
```

## Aggressively abstract common patterns (DRY).

When a pattern is repeated in multiple places:

Don't:
```c
fn Term <many_fns>(...) {
  ...
  if (side == 0) {
    subst_var(loc, res1);
    return res0;
  } else {
    subst_var(loc, res0);
    return res1;
  }
}
```

Do:
```c
fn Term subst_cop(u8 side, u32 loc, Term r0, Term r1) {
  subst_var(loc, side == 0 ? r1 : r0);
  return side == 0 ? r0 : r1;
}
fn Term <many_fns>(...) {
  ...
  return subst_cop(side, loc, res0, res1);
}
```

In general, spend some time reasoning about opportunities to apply the DRY
principle, extracting common patterns out to reduce code size. We greatly
appreciate simplicity brought by good abstractions!

## Align columns whenever reasonable; adjust names as needed.

Don't:
```c
Term abc = foo;
u32 x = 123;
Term the_amazing_cat = bar;
```

Do:
```c
Term abc = foo;
u32  x   = 123;
Term cat = bar;
```

Don't:
```c
foo[x] = 123;
foo[x+1] = 456;
```

Do:
```c
foo[x+0] = 123;
foo[x+1] = 456;
```

## Separate sessions with markdown-inspired headers.

Don't:
```c
---------------------------------
File Session
---------------------------------
```

Do:
```c
// File Session
// ============

// File Sub-Session
// ----------------

// ### File Sub-Sub-Session
```
