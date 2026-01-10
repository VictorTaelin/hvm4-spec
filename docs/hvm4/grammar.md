# HVM4 Grammar Reference

## A concise specification for writing consistent HVM4 code.

## File Structure

File     ::= (Include | Def)*
Include  ::= "#include" '"' Path '"'
Def      ::= "@" Name "=" Term
Comment  ::= "//" ... newline

## Names

Name     ::= [a-zA-Z][a-zA-Z0-9_$]*    -- identifiers
Label    ::= Name | "(" Term ")"       -- static or dynamic label

## Terms

Term ::=
  -- Primitives
  | Name                               -- variable
  | Name "₀"                           -- first dup branch
  | Name "₁"                           -- second dup branch
  | Integer                            -- number literal
  | "@" Name                           -- reference to definition
  | "*"                                -- wildcard (don't care)

  -- Lambda & Application
  | "λ" Params "." Term                -- lambda: λx.body or λx,y.body
  | "λ" "&" Name "." Term              -- cloned lambda: λ&x.body (auto-dup x)
  | "λ" Name "&" Label "." Term        -- lambda w/ dup: λx&L.body
  | "λ" "&" Name "&" Label "." Term    -- cloned lambda w/ dup
  | Term "(" Args ")"                  -- application: f(x,y)
  | "(" Term Term+ ")"                 -- application: (f x y)

  -- Pattern Matching (inside λ{...})
  | "λ" "{" Cases "}"                  -- match lambda
  | "λ" "{" Term "}"                   -- use (unbox): λ{x}

  -- Duplication & Superposition
  | "!" Name "&" Label "=" Term ";" Term        -- dup: !x&L=val;body
  | "!" Name "&" "(" Term ")" "=" Term ";" Term -- dynamic dup
  | "&" Label "{" Term "," Term "}"             -- sup: &L{a,b}
  | "&" "(" Term ")" "{" Term "," Term "}"      -- dynamic sup
  | "&" "{" "}"                                 -- erasure

  -- Let Bindings (sugar)
  | "!" Name "=" Term ";" Term         -- let: !x=val;body → (λx.body)(val)
  | "!" "&" Name "=" Term ";" Term     -- cloned let: !&x=val;body
  | "!!" Name "=" Term ";" Term        -- strict let (forces evaluation)
  | "!!" "&" Name "=" Term ";" Term    -- strict cloned let
  | "!" Name "=" "λ" Name ";" Term     -- unscoped lambda

  -- Constructors
  | "#" Name "{" Args "}"              -- constructor: #Pair{a,b}
  | "#" Name "{" "}"                   -- nullary: #Z{} or #True{}

  -- Stuck Terms
  | "^" Name                           -- stuck name
  | "^" "(" Term Term ")"              -- stuck application (dry)

  -- Operators (infix, require grouping)
  | Term Op Term                       -- binary op: (a + b)
  | Term "===" Term                    -- structural equality
  | Term ".&." Term                    -- short-circuit AND
  | Term ".|." Term                    -- short-circuit OR

  -- Special Forms
  | "↑" Term                           -- priority (collapse ordering)
  | Term "~>" Term                     -- reduction hint
  | Term "<>" Term                     -- cons: a <> b → #CON{a,b}
  | "&" Label "λ" Names "{" Term ";" Term "}"  -- fork

  -- Sugar
  | "[" Args "]"                       -- list: [a,b,c]
  | '"' Chars '"'                      -- string: "hello"
  | "'" Char "'"                       -- char: 'x' → #CHR{code}
  | Integer "n"                        -- nat: 2n → #S{#S{#Z{}}}
  | Integer "n" "+" Term               -- nat+: 2n+x → #S{#S{x}}

Params   ::= Name | Name "," Params
Args     ::= Term ("," Term)*
Names    ::= Name ("," Name)*

## Pattern Cases (inside λ{...})

Cases    ::= Case+ [Default]
Case     ::= Pattern ":" Term ";"?
Default  ::= Term | "_" ":" Term

Pattern  ::=
  | "#" Name        -- constructor: #Z, #S, #Cons
  | Integer         -- number: 0
  | Integer "+"     -- successor: 1+ (binds predecessor)
  | "[]"            -- nil (empty list)
  | "<>"            -- cons (list head::tail)
  | Integer "n"     -- nat zero: 0n
  | Integer "n" "+" -- nat succ: 1n+ (binds pred)

## Operators (by precedence, low to high)

  1. ||        logical OR
  2. &&        logical AND
  3. == !=     equality (numeric)
  4. < <= > >= comparison
  5. << >>     bit shift
  6. + -       addition, subtraction
  7. * / %     multiplication, division, modulo
  8. ^         bitwise XOR

## Key Rules

1. AFFINITY: Variables are affine (used at most once) unless:
   - Prefixed with & (cloned): λ&x or !&x allows multiple uses
   - Duplicated explicitly: !x&L=val gives x₀ and x₁

2. LABELS: DUP and SUP with same label annihilate; different labels commute.
   - Static: !x&L=val or &L{a,b}
   - Dynamic: !x&(n)=val or &(n){a,b}
   - Auto: !x&=val (fresh label generated)

3. SUBSCRIPTS: Access dup branches with x₀ (first) and x₁ (second).

4. FORK: &Lλx,y{A;B} desugars to λx&L.λy&L.&L{A;B}
   - A uses x₀,y₀; B uses x₁,y₁

## Examples

```hvm4
// Identity function
@id = λx.x

// Church numerals with duplication
@c2 = λs. !S&C=s; λx. S₀(S₁(x))

// Pattern matching on naturals
@add = λ{ #Z: λb.b; #S: λp.λb. #S{@add(p,b)} }

// Superposition (non-deterministic choice)
@coin = &A{0,1}

// Fork for parallel computation
@both = &Lλx,y{(x+y);(x*y)}

// Let binding
@main = ! x = 5; ! y = 3; x + y

// Cloned variable (multiple uses)
@square = λ&x. x * x

// List and string literals
@nums = [1,2,3]
@hello = "hello"

// Factorial with dup
@fac = λ{ #Z: #S{#Z{}}; #S: λp. !P&A=p; @mul(#S{P₀}, @fac(P₁)) }

// Constructors
@pair = #Pair{1,2}
@true = #T{}
@zero = #Z{}

// Natural number sugar
@two = 2n           // #S{#S{#Z{}}}
@succ = 1n+@zero    // #S{#Z{}}
```
