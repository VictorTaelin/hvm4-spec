Calculus of Interactions
========================
CoI is a term rewrite system for the following grammar:

```
Term ::=
| Var ::= Name
| Dp0 ::= Name "₀"
| Dp1 ::= Name "₁"
| Ref ::= "@" Name
| Nam ::= "^" Name
| Dry ::= "^" "(" Term " " Term ")"
| Era ::= "&{}"
| Sup ::= "&" Name "{" Term "," Term "}"
| Dup ::= "!" Name "&" Name "=" Term ";" Term
| Ctr ::= "#" Name "{" [Term] "}"
| Mat ::= "λ" "{" "#" Name ":" Term ";" Term "}"
| Lam ::= "λ" Name "." Term
| App ::= "(" Term " " Term ")"
| And ::= Term "&&" Term
| Eql ::= Term "==" Term
| Gua ::= Term "~>" Term
```

Where:
- `Name ::= any sequence of base-64 chars in _ A-Z a-z 0-9 $`
- `[T]  ::= any sequence of T separated by ","`

In CoI:
- Variables are affine; they must occur at most once.
- Variables range globally; they can occur anywhere.

Reference Interaction
---------------------

```
@foo
-------------------- ref
foo ~> alloc(book.foo)
```

Duplication Interactions
------------------------

```
! X &L = &{}
---------- dup-era
X₀ ← &{}
X₁ ← &{}

! X &L = &R{a,b}
-------------- dup-sup
if L == R:
  X₀ ← a
  X₁ ← b
else:
  ! A &L = a
  ! B &L = b
  X₀ ← &R{A₀,B₀}
  X₁ ← &R{A₁,B₁}

! F &L = λx.f
-------------- dup-lam
F₀ ← λ$x0.G₀
F₁ ← λ$x1.G₁
x  ← &L{$x0,$x1}
! G &L = f

! X &L = #K{a,b}
---------------- dup-ctr
! A &L = a
! B &L = b
X₀ ← #K{A₀,B₀}
X₁ ← #K{A₁,B₁}

! X &L = λ{#K:h; m}
------------------- dup-mat
! H &L = h
! M &L = m
X₀ ← λ{#K:H₀; M₀}
X₁ ← λ{#K:H₁; M₁}

! X &L = ^n
----------- dup-nam
X₀ ← ^n
X₁ ← ^n

! X &L = ^(f x)
--------------- dup-dry
! F &L = f
! A &L = x
X₀ ← ^(F₀ A₀)
X₁ ← ^(F₁ A₁)

! X &L = f ~> g
--------------- dup-gua
! F &L = f
! G &L = g
X₀ ← F₀ ~> G₀
X₁ ← F₁ ~> G₁
```

Application Interactions
------------------------

```
(&{} a)
------- app-era
&{}

(&L{f,g} a)
----------------- app-sup
! A &L = a
&L{(f A₀),(g A₁)}

(λx.f a)
-------- app-lam
x ← a
f

(λ{#K:h; m} &{})
---------------- app-mat-era
&{}

(λ{#K:h; m} &L{a,b})
-------------------- app-mat-sup
! H &L = h
! M &L = m
&L{(λ{#K:H₀; M₀} a)
  ,(λ{#K:H₁; M₁} b)}

(λ{#K:h; m} #K{a,b})
-------------------- app-mat-ctr-match
(h a b)

(λ{#K:h; m} #L{a,b})
-------------------- app-mat-ctr-miss
(m #L{a,b})

(^n a)
------- app-nam
^(^n a)

(^(f x) a)
----------- app-dry
^(^(f x) a)
```

Conjunction Interactions
------------------------

```
&{} && b
-------- and-era
&{}

&L{a0,a1} && b
-------------- and-sup
! B &L = b
&L{B₀ && a0
  ,B₁ && a1}

#F{} && b
--------- and-fal
#F{}

#T{} && b
--------- and-tru
b
```

Equality Interactions
---------------------

```
&{} == b
-------- eql-era-any
&{}

&L{a0,a1} == b
-------------- eql-sup-any
! &L B = b
&L{a0 == B₀
  ,a1 == B₁}

a == &{}
-------- eql-any-era
&{}

a == &L{b0,b1}
-------------- eql-any-sup
! &L A = a
&L{A₀ == b0
  ,A₁ == b1}

λax.af == λbx.bf
---------------- eql-lam-lam
ax ← X
bx ← X
af == bf

#K{a0,a1} == #K{b0,b1}
---------------------- eql-ctr-ctr-match
(a0==b0) && (a1==b1)

#K{...} == #L{...}
------------------ eql-ctr-ctr-diff
#F{}

λ{#K:h0; m0} == λ{#K:h1; m1}
---------------------------- eql-mat-mat-match
(h0==h1) && (m0==m1)

λ{#K:h0; m0} == λ{#L:h1; m1}
---------------------------- eql-mat-mat-diff
#F{}

^x == ^x
-------- eql-nam-nam-same
#T{}

^x == ^y
-------- eql-nam-nam-diff
#F{}

^(af ax) == ^(bf bx)
---------------------- eql-dry-dry
(af == bf) && (ax == bx)

(af~>ag) == b
----------- eql-gua
#F{}
```

Guarded Application Interactions
--------------------------------

```
((f ~> &{}) a)
-------------- app-gua-era
&{}

((f ~> &L{x,y}) a)
------------------ app-gua-sup
! &L F = f
! F &L = f
! A &L = a
&L{((F₀ ~> x) A₀)
  ,((F₁ ~> y) A₁)}

((f ~> λx.g) a)
--------------- app-gua-lam
x ← a
(f x) ~> g

((f ~> (g ~> h)) a)
----------------------- app-gua-gua
((f a) ~> (g a))

((f ~> λ{#K:h; m}) &{})
----------------------- app-gua-mat-era
&{}

((f ~> λ{#K:h; m}) &L{a,b})
--------------------------- app-gua-mat-sup
! F &L = f
! H &L = h
! M &L = m
&L{((F₀ ~> λ{#K:H₀; M₀}) a)
  ,((F₁ ~> λ{#K:H₁; M₁}) b)}

((f ~> λ{#K:h; m}) #K{a,b})
--------------------------- app-gua-mat-ctr-match
let fn = λa.λb.(f #K{a,b})
((fn ~> h) a b)

((f ~> λ{#K:h; m}) #L{a,b})
--------------------------- app-gua-mat-ctr-miss
let fn = λa.λb.(f #L{a,b})
((fn ~> m) a b)
```
