// HVM4 Runtime Implementation in CUDA
// ====================================
//
// This file implements the HVM4, an Interaction Calculus runtime, ported from
// the C implementation. It uses CUDA unified memory for seamless CPU/GPU access.
//
// Multi-threaded Mode:
// Each thread gets its own heap/stack partition and runs the same computation
// independently. This tests SM saturation with parallel independent reductions.
//
// Term Pointer Layout (64-bit)
// ----------------------------
// | SUB  (1 bit)   ::= marks heap slot as containing a substitution
// | TAG  (7 bits)  ::= constructor variant (APP, LAM, SUP, etc.)
// | EXT  (24 bits) ::= dup label, ctr name, or ref name
// | VAL  (32 bits) ::= heap address or unboxed value

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <ctype.h>
#include <time.h>

// Types
// =====

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef u64 Term;

typedef struct {
  Term k0;
  Term k1;
} Copy;

// Function Definition Macros
// ==========================

#define fn static inline
#define dfn __device__ __forceinline__
#define hdfn __host__ __device__ __forceinline__

// Tags
// ====

#define NAM  0
#define DRY  1
#define REF  2
#define ALO  3
#define ERA  4
#define CO0  5
#define CO1  6
#define VAR  7
#define LAM  8
#define APP  9
#define SUP 10
#define DUP 11
#define MAT 12
#define CTR 13
#define CTR_MAX_ARI 16

// Bit Layout
// ==========

#define SUB_BITS 1
#define TAG_BITS 7
#define EXT_BITS 24
#define VAL_BITS 32

#define SUB_SHIFT 63
#define TAG_SHIFT 56
#define EXT_SHIFT 32
#define VAL_SHIFT 0

#define SUB_MASK 0x1
#define TAG_MASK 0x7F
#define EXT_MASK 0xFFFFFF
#define VAL_MASK 0xFFFFFFFF

// Capacities
// ==========

#define BOOK_CAP      (1ULL << 24)  // ~16M entries for book (shared, matches EXT_MASK)
// Per-thread memory will be computed dynamically based on available GPU memory
// and requested thread count. These are fallback defaults.
#define DEFAULT_HEAP_PER_THR  (1ULL << 21)  // 2M terms (~16MB)
#define DEFAULT_STACK_PER_THR (1ULL << 21)  // 2M entries (~16MB)
#define MAX_THREADS   1024  // Max threads per block

// Globals (Unified Memory - Host side)
// ====================================

static u32  *BOOK;       // Shared book (read-only after parsing)
static Term *HEAP;       // Global heap (partitioned per thread)
static Term *STACK;      // Global stack (partitioned per thread)

// Host-side globals for parsing
static u64 H_ALLOC = 1;

// Per-thread state (passed to kernel, stored in local vars)
// Each thread has:
//   - heap:  HEAP  + tid * HEAP_PER_THR
//   - stack: STACK + tid * STACK_PER_THR
//   - alloc: starts at book_size (after copying book terms)
//   - itrs:  local counter, summed at end

// Error Handling
// ==============

fn void error(const char *msg) {
  fprintf(stderr, "ERROR: %s\n", msg);
  exit(1);
}

#define CUDA_CHECK(call) do { \
  cudaError_t err = call; \
  if (err != cudaSuccess) { \
    fprintf(stderr, "CUDA error at %s:%d: %s\n", __FILE__, __LINE__, cudaGetErrorString(err)); \
    exit(1); \
  } \
} while(0)

// Term Helpers (Host + Device)
// ============================

hdfn Term new_term(u8 sub, u8 tag, u32 ext, u32 val) {
  return ((u64)sub << SUB_SHIFT)
       | ((u64)(tag & TAG_MASK) << TAG_SHIFT)
       | ((u64)(ext & EXT_MASK) << EXT_SHIFT)
       | ((u64)(val & VAL_MASK));
}

hdfn u8 sub_of(Term t) {
  return (t >> SUB_SHIFT) & SUB_MASK;
}

hdfn u8 tag(Term t) {
  return (t >> TAG_SHIFT) & TAG_MASK;
}

hdfn u32 ext(Term t) {
  return (t >> EXT_SHIFT) & EXT_MASK;
}

hdfn u32 val(Term t) {
  return (t >> VAL_SHIFT) & VAL_MASK;
}

hdfn u32 arity_of(Term t) {
  switch (tag(t)) {
    case LAM: {
      return 1;
    }
    case APP:
    case SUP:
    case DUP:
    case MAT:
    case DRY: {
      return 2;
    }
    default: {
      if (tag(t) >= CTR && tag(t) <= CTR + CTR_MAX_ARI) {
        return tag(t) - CTR;
      }
      return 0;
    }
  }
}

hdfn Term mark_sub(Term t) {
  return t | ((u64)1 << SUB_SHIFT);
}

hdfn Term clear_sub(Term t) {
  return t & ~(((u64)SUB_MASK) << SUB_SHIFT);
}

// Host Allocation (for parsing)
// =============================

fn u64 host_heap_alloc(u64 size) {
  u64 at = H_ALLOC;
  H_ALLOC += size;
  return at;
}

// Names
// =====

static const char *alphabet = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$";

fn int char_to_b64(char c) {
  if (c == '_') {
    return 0;
  }
  if (c >= 'a' && c <= 'z') {
    return 1 + (c - 'a');
  }
  if (c >= 'A' && c <= 'Z') {
    return 27 + (c - 'A');
  }
  if (c >= '0' && c <= '9') {
    return 53 + (c - '0');
  }
  if (c == '$') {
    return 63;
  }
  return -1;
}

fn int is_name_start(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

fn int is_name_char(char c) {
  return char_to_b64(c) >= 0;
}

// Host Term Constructors (for parsing)
// ====================================

fn Term H_NewAt(u32 loc, u8 tag, u32 ext, u32 ari, Term *args) {
  for (u32 i = 0; i < ari; i++) {
    HEAP[loc + i] = args[i];
  }
  return new_term(0, tag, ext, loc);
}

fn Term H_New(u8 tag, u32 ext, u32 ari, Term *args) {
  return H_NewAt(host_heap_alloc(ari), tag, ext, ari, args);
}

fn Term H_Var(u32 loc) {
  return new_term(0, VAR, 0, loc);
}

fn Term H_Ref(u32 nam) {
  return new_term(0, REF, nam, 0);
}

fn Term H_Nam(u32 nam) {
  return new_term(0, NAM, 0, nam);
}

fn Term H_Era(void) {
  return new_term(0, ERA, 0, 0);
}

fn Term H_Co0(u32 lab, u32 loc) {
  return new_term(0, CO0, lab, loc);
}

fn Term H_Co1(u32 lab, u32 loc) {
  return new_term(0, CO1, lab, loc);
}

fn Term H_LamAt(u32 loc, Term bod) {
  Term args[1] = {bod};
  return H_NewAt(loc, LAM, 0, 1, args);
}

fn Term H_Lam(Term bod) {
  return H_LamAt(host_heap_alloc(1), bod);
}

fn Term H_AppAt(u32 loc, Term fun, Term arg) {
  Term args[2] = {fun, arg};
  return H_NewAt(loc, APP, 0, 2, args);
}

fn Term H_App(Term fun, Term arg) {
  return H_AppAt(host_heap_alloc(2), fun, arg);
}

fn Term H_SupAt(u32 loc, u32 lab, Term tm0, Term tm1) {
  Term args[2] = {tm0, tm1};
  return H_NewAt(loc, SUP, lab, 2, args);
}

fn Term H_Sup(u32 lab, Term tm0, Term tm1) {
  return H_SupAt(host_heap_alloc(2), lab, tm0, tm1);
}

fn Term H_DryAt(u32 loc, Term tm0, Term tm1) {
  Term args[2] = {tm0, tm1};
  return H_NewAt(loc, DRY, 0, 2, args);
}

fn Term H_Dry(Term tm0, Term tm1) {
  return H_DryAt(host_heap_alloc(2), tm0, tm1);
}

fn Term H_DupAt(u32 loc, u32 lab, Term v, Term bod) {
  Term args[2] = {v, bod};
  return H_NewAt(loc, DUP, lab, 2, args);
}

fn Term H_Dup(u32 lab, Term v, Term bod) {
  return H_DupAt(host_heap_alloc(2), lab, v, bod);
}

fn Term H_MatAt(u32 loc, u32 nam, Term v, Term nxt) {
  Term args[2] = {v, nxt};
  return H_NewAt(loc, MAT, nam, 2, args);
}

fn Term H_Mat(u32 nam, Term v, Term nxt) {
  return H_MatAt(host_heap_alloc(2), nam, v, nxt);
}

fn Term H_CtrAt(u32 loc, u32 nam, u32 ari, Term *args) {
  return H_NewAt(loc, CTR + ari, nam, ari, args);
}

fn Term H_Ctr(u32 nam, u32 ari, Term *args) {
  return H_CtrAt(host_heap_alloc(ari), nam, ari, args);
}

// Stringifier (Host only)
// =======================

#define STR_LOG 0
#define STR_BUF 1

static u8    STR_MODE     = STR_LOG;
static char *TERM_BUF     = NULL;
static u32   TERM_BUF_POS = 0;
static u32   TERM_BUF_CAP = 0;

fn void str_putc(char c) {
  if (STR_MODE == STR_LOG) {
    putchar(c);
  } else {
    if (TERM_BUF_POS + 1 >= TERM_BUF_CAP) {
      TERM_BUF_CAP *= 2;
      TERM_BUF = (char*)realloc(TERM_BUF, TERM_BUF_CAP);
    }
    TERM_BUF[TERM_BUF_POS++] = c;
    TERM_BUF[TERM_BUF_POS]   = 0;
  }
}

fn void str_puts(const char *s) {
  while (*s) {
    str_putc(*s++);
  }
}

fn void str_name(u32 n) {
  if (n < 64) {
    str_putc(alphabet[n]);
  } else {
    str_name(n / 64);
    str_putc(alphabet[n % 64]);
  }
}

fn void str_term_go(Term term, Term *heap, u32 depth);

fn void str_term_go(Term term, Term *heap, u32 depth) {
  switch (tag(term)) {
    case VAR:
    case NAM: {
      str_name(val(term));
      break;
    }
    case REF: {
      str_putc('@');
      str_name(ext(term));
      break;
    }
    case ERA: {
      str_puts("&{}");
      break;
    }
    case CO0:
    case CO1: {
      str_name(val(term));
      str_puts(tag(term) == CO0 ? "₀" : "₁");
      break;
    }
    case LAM: {
      u32 loc = val(term);
      u32 nam = depth + 1;
      str_puts("λ");
      str_name(nam);
      str_putc('.');
      str_term_go(heap[loc], heap, depth + 1);
      break;
    }
    case APP:
    case DRY: {
      Term spine[256];
      u32  len  = 0;
      Term curr = term;
      while ((tag(curr) == APP || tag(curr) == DRY) && len < 256) {
        u32 loc = val(curr);
        spine[len++] = heap[loc + 1];
        curr = heap[loc];
      }
      if (tag(curr) == LAM) {
        str_putc('(');
        str_term_go(curr, heap, depth);
        str_putc(')');
      } else {
        str_term_go(curr, heap, depth);
      }
      str_putc('(');
      for (u32 i = 0; i < len; i++) {
        if (i > 0) {
          str_putc(',');
        }
        str_term_go(spine[len - 1 - i], heap, depth);
      }
      str_putc(')');
      break;
    }
    case SUP: {
      u32 loc = val(term);
      str_putc('&');
      str_name(ext(term));
      str_putc('{');
      str_term_go(heap[loc + 0], heap, depth);
      str_putc(',');
      str_term_go(heap[loc + 1], heap, depth);
      str_putc('}');
      break;
    }
    case DUP: {
      u32 loc = val(term);
      u32 nam = depth + 1;
      str_putc('!');
      str_name(nam);
      str_putc('&');
      str_name(ext(term));
      str_putc('=');
      str_term_go(heap[loc + 0], heap, depth);
      str_putc(';');
      str_term_go(heap[loc + 1], heap, depth + 1);
      break;
    }
    case MAT: {
      u32 loc = val(term);
      str_puts("λ{#");
      str_name(ext(term));
      str_putc(':');
      str_term_go(heap[loc + 0], heap, depth);
      str_putc(';');
      str_term_go(heap[loc + 1], heap, depth);
      str_putc('}');
      break;
    }
    case ALO: {
      str_puts("<ALO>");
      break;
    }
    default: {
      if (tag(term) >= CTR && tag(term) <= CTR + CTR_MAX_ARI) {
        u32 ari = tag(term) - CTR;
        u32 loc = val(term);
        str_putc('#');
        str_name(ext(term));
        str_putc('{');
        for (u32 i = 0; i < ari; i++) {
          if (i > 0) {
            str_putc(',');
          }
          str_term_go(heap[loc + i], heap, depth);
        }
        str_putc('}');
      }
      break;
    }
  }
}

fn void print_term(Term term, Term *heap) {
  STR_MODE = STR_LOG;
  str_term_go(term, heap, 0);
}

// Parser (Host only)
// ==================

typedef struct {
  char *file;
  char *src;
  u32   pos;
  u32   len;
  u32   line;
  u32   col;
} PState;

typedef struct {
  u32 name;
  u32 depth;
  u32 lab;
} PBind;

static char  *PARSE_SEEN_FILES[1024];
static u32    PARSE_SEEN_FILES_LEN = 0;
static PBind  PARSE_BINDS[16384];
static u32    PARSE_BINDS_LEN = 0;

fn void parse_error(PState *s, const char *expected, char detected) {
  fprintf(stderr, "\033[1;31mPARSE_ERROR\033[0m (%s:%d:%d)\n", s->file, s->line, s->col);
  fprintf(stderr, "- expected: %s\n", expected);
  if (detected == 0) {
    fprintf(stderr, "- detected: EOF\n");
  } else {
    fprintf(stderr, "- detected: '%c'\n", detected);
  }
  exit(1);
}

fn int at_end(PState *s) {
  return s->pos >= s->len;
}

fn char peek_at(PState *s, u32 offset) {
  u32 idx = s->pos + offset;
  return (idx >= s->len) ? 0 : s->src[idx];
}

fn char peek(PState *s) {
  return peek_at(s, 0);
}

fn void advance(PState *s) {
  if (at_end(s)) {
    return;
  }
  if (s->src[s->pos] == '\n') {
    s->line++;
    s->col = 1;
  } else {
    s->col++;
  }
  s->pos++;
}

fn int starts_with(PState *s, const char *str) {
  u32 i = 0;
  while (str[i]) {
    if (peek_at(s, i) != str[i]) {
      return 0;
    }
    i++;
  }
  return 1;
}

fn int match(PState *s, const char *str) {
  if (!starts_with(s, str)) {
    return 0;
  }
  while (*str) {
    advance(s);
    str++;
  }
  return 1;
}

fn int is_space(char c) {
  return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

fn void skip_comment(PState *s) {
  while (!at_end(s) && peek(s) != '\n') {
    advance(s);
  }
}

fn void skip(PState *s) {
  while (!at_end(s)) {
    if (is_space(peek(s))) {
      advance(s);
      continue;
    }
    if (starts_with(s, "//")) {
      skip_comment(s);
      continue;
    }
    break;
  }
}

fn void consume(PState *s, const char *str) {
  skip(s);
  if (!match(s, str)) {
    parse_error(s, str, peek(s));
  }
  skip(s);
}

fn void bind_push(u32 name, u32 depth, u32 lab) {
  PARSE_BINDS[PARSE_BINDS_LEN++] = (PBind){name, depth, lab};
}

fn void bind_pop(void) {
  PARSE_BINDS_LEN--;
}

fn void bind_lookup(u32 name, u32 depth, int *idx, u32 *lab) {
  for (int i = PARSE_BINDS_LEN - 1; i >= 0; i--) {
    if (PARSE_BINDS[i].name == name) {
      *idx = depth - 1 - PARSE_BINDS[i].depth;
      *lab = PARSE_BINDS[i].lab;
      return;
    }
  }
  *idx = -1;
  *lab = 0;
}

fn u32 parse_name(PState *s) {
  skip(s);
  char c = peek(s);
  if (!is_name_start(c)) {
    parse_error(s, "name", c);
  }
  u32 k = 0;
  while (is_name_char(peek(s))) {
    c = peek(s);
    k = ((k << 6) + char_to_b64(c)) & EXT_MASK;
    advance(s);
  }
  skip(s);
  return k;
}

fn Term parse_term(PState *s, u32 depth);
fn void parse_def(PState *s);

fn Term parse_mat_body(PState *s, u32 depth) {
  skip(s);
  if (peek(s) == '}') {
    consume(s, "}");
    return H_Era();
  }
  if (peek(s) == '#') {
    consume(s, "#");
    u32  nam = parse_name(s);
    consume(s, ":");
    Term v = parse_term(s, depth);
    skip(s);
    match(s, ";");
    skip(s);
    Term nxt = parse_mat_body(s, depth);
    return H_Mat(nam, v, nxt);
  }
  Term v = parse_term(s, depth);
  consume(s, "}");
  return v;
}

fn Term parse_lam(PState *s, u32 depth) {
  skip(s);
  if (peek(s) == '{') {
    consume(s, "{");
    return parse_mat_body(s, depth);
  }
  u32 nam = parse_name(s);
  consume(s, ".");
  bind_push(nam, depth, 0);
  u64  loc  = host_heap_alloc(1);
  Term body = parse_term(s, depth + 1);
  HEAP[loc] = body;
  bind_pop();
  return new_term(0, LAM, depth, loc);
}

fn Term parse_dup(PState *s, u32 depth) {
  u32 nam = parse_name(s);
  consume(s, "&");
  u32  lab = parse_name(s);
  consume(s, "=");
  Term v = parse_term(s, depth);
  skip(s);
  match(s, ";");
  skip(s);
  bind_push(nam, depth, lab);
  u64 loc       = host_heap_alloc(2);
  HEAP[loc + 0] = v;
  Term body     = parse_term(s, depth + 1);
  HEAP[loc + 1] = body;
  bind_pop();
  return new_term(0, DUP, lab, loc);
}

fn Term parse_sup(PState *s, u32 depth) {
  skip(s);
  if (peek(s) == '{') {
    consume(s, "{");
    consume(s, "}");
    return H_Era();
  }
  u32 lab = parse_name(s);
  consume(s, "{");
  Term tm0 = parse_term(s, depth);
  skip(s);
  match(s, ",");
  skip(s);
  Term tm1 = parse_term(s, depth);
  consume(s, "}");
  return H_Sup(lab, tm0, tm1);
}

fn Term parse_ctr(PState *s, u32 depth) {
  u32  nam = parse_name(s);
  consume(s, "{");
  Term args[16];
  u32  cnt = 0;
  skip(s);
  if (peek(s) != '}') {
    while (1) {
      args[cnt++] = parse_term(s, depth);
      skip(s);
      if (peek(s) == ',') {
        consume(s, ",");
        continue;
      }
      break;
    }
  }
  consume(s, "}");
  return H_Ctr(nam, cnt, args);
}

fn Term parse_ref(PState *s) {
  return H_Ref(parse_name(s));
}

fn Term parse_par(PState *s, u32 depth) {
  Term term = parse_term(s, depth);
  consume(s, ")");
  return term;
}

fn Term parse_var(PState *s, u32 depth) {
  skip(s);
  u32 nam = parse_name(s);
  int idx;
  u32 lab;
  bind_lookup(nam, depth, &idx, &lab);
  skip(s);
  int side = match(s, "₀") ? 0 : match(s, "₁") ? 1 : -1;
  skip(s);
  u32 v   = (idx >= 0) ? (u32)idx : nam;
  u8  tg  = (side == 0) ? CO0 : (side == 1) ? CO1 : VAR;
  return new_term(0, tg, lab, v);
}

fn Term parse_app(Term f, PState *s, u32 depth) {
  skip(s);
  if (peek(s) != '(') {
    return f;
  }
  consume(s, "(");
  if (peek(s) == ')') {
    consume(s, ")");
    return parse_app(f, s, depth);
  }
  while (1) {
    Term arg = parse_term(s, depth);
    f = H_App(f, arg);
    skip(s);
    if (peek(s) == ',') {
      consume(s, ",");
      continue;
    }
    if (peek(s) == ')') {
      consume(s, ")");
      break;
    }
    parse_error(s, "',' or ')'", peek(s));
  }
  return parse_app(f, s, depth);
}

fn Term parse_term(PState *s, u32 depth) {
  skip(s);
  Term t;
  if (match(s, "λ")) {
    t = parse_lam(s, depth);
  } else if (match(s, "!")) {
    t = parse_dup(s, depth);
  } else if (match(s, "&")) {
    t = parse_sup(s, depth);
  } else if (match(s, "#")) {
    t = parse_ctr(s, depth);
  } else if (match(s, "@")) {
    t = parse_ref(s);
  } else if (match(s, "(")) {
    t = parse_par(s, depth);
  } else {
    t = parse_var(s, depth);
  }
  return parse_app(t, s, depth);
}

fn void parse_def(PState *s) {
  skip(s);
  if (at_end(s)) {
    return;
  }
  if (match(s, "@")) {
    u32 nam = parse_name(s) & EXT_MASK;
    consume(s, "=");
    PARSE_BINDS_LEN = 0;
    Term v          = parse_term(s, 0);
    u64  loc        = host_heap_alloc(1);
    HEAP[loc]       = v;
    BOOK[nam]       = (u32)loc;
    parse_def(s);
    return;
  }
  parse_error(s, "definition", peek(s));
}

// ============================================================================
// DEVICE CODE - Per-thread state passed via parameters
// ============================================================================

// Device Term Constructors (use local heap pointer)
// =================================================

dfn Term d_NewAt(Term *heap, u32 loc, u8 tag, u32 ext, u32 ari, Term *args) {
  for (u32 i = 0; i < ari; i++) {
    heap[loc + i] = args[i];
  }
  return new_term(0, tag, ext, loc);
}

dfn Term d_New(Term *heap, u32 *alloc, u8 tag, u32 ext, u32 ari, Term *args) {
  u32 loc = *alloc;
  *alloc += ari;
  return d_NewAt(heap, loc, tag, ext, ari, args);
}

dfn Term d_Var(u32 loc) {
  return new_term(0, VAR, 0, loc);
}

dfn Term d_Ref(u32 nam) {
  return new_term(0, REF, nam, 0);
}

dfn Term d_Nam(u32 nam) {
  return new_term(0, NAM, 0, nam);
}

dfn Term d_Era(void) {
  return new_term(0, ERA, 0, 0);
}

dfn Term d_Co0(u32 lab, u32 loc) {
  return new_term(0, CO0, lab, loc);
}

dfn Term d_Co1(u32 lab, u32 loc) {
  return new_term(0, CO1, lab, loc);
}

dfn Term d_LamAt(Term *heap, u32 loc, Term bod) {
  Term args[1] = {bod};
  return d_NewAt(heap, loc, LAM, 0, 1, args);
}

dfn Term d_Lam(Term *heap, u32 *alloc, Term bod) {
  u32 loc = (*alloc)++;
  return d_LamAt(heap, loc, bod);
}

dfn Term d_AppAt(Term *heap, u32 loc, Term fun, Term arg) {
  Term args[2] = {fun, arg};
  return d_NewAt(heap, loc, APP, 0, 2, args);
}

dfn Term d_App(Term *heap, u32 *alloc, Term fun, Term arg) {
  u32 loc = *alloc;
  *alloc += 2;
  return d_AppAt(heap, loc, fun, arg);
}

dfn Term d_SupAt(Term *heap, u32 loc, u32 lab, Term tm0, Term tm1) {
  Term args[2] = {tm0, tm1};
  return d_NewAt(heap, loc, SUP, lab, 2, args);
}

dfn Term d_Sup(Term *heap, u32 *alloc, u32 lab, Term tm0, Term tm1) {
  u32 loc = *alloc;
  *alloc += 2;
  return d_SupAt(heap, loc, lab, tm0, tm1);
}

dfn Term d_DryAt(Term *heap, u32 loc, Term tm0, Term tm1) {
  Term args[2] = {tm0, tm1};
  return d_NewAt(heap, loc, DRY, 0, 2, args);
}

dfn Term d_Dry(Term *heap, u32 *alloc, Term tm0, Term tm1) {
  u32 loc = *alloc;
  *alloc += 2;
  return d_DryAt(heap, loc, tm0, tm1);
}

dfn Term d_DupAt(Term *heap, u32 loc, u32 lab, Term v, Term bod) {
  Term args[2] = {v, bod};
  return d_NewAt(heap, loc, DUP, lab, 2, args);
}

dfn Term d_Dup(Term *heap, u32 *alloc, u32 lab, Term v, Term bod) {
  u32 loc = *alloc;
  *alloc += 2;
  return d_DupAt(heap, loc, lab, v, bod);
}

dfn Term d_MatAt(Term *heap, u32 loc, u32 nam, Term v, Term nxt) {
  Term args[2] = {v, nxt};
  return d_NewAt(heap, loc, MAT, nam, 2, args);
}

dfn Term d_Ctr(Term *heap, u32 *alloc, u32 nam, u32 ari, Term *args) {
  u32 loc = *alloc;
  *alloc += ari;
  return d_NewAt(heap, loc, CTR + ari, nam, ari, args);
}

// Cloning (Device)
// ================

dfn Copy d_clone_at(u32 loc, u32 lab) {
  return (Copy){ d_Co0(lab, loc), d_Co1(lab, loc) };
}

dfn Copy d_clone(Term *heap, u32 *alloc, u32 lab, Term v) {
  u32 loc = (*alloc)++;
  heap[loc] = v;
  return d_clone_at(loc, lab);
}

// Substitution Helpers (Device)
// =============================

dfn void d_subst_var(Term *heap, u32 loc, Term v) {
  heap[loc] = mark_sub(v);
}

dfn Term d_subst_cop(Term *heap, u8 side, u32 loc, Term r0, Term r1) {
  heap[loc] = mark_sub(side == 0 ? r1 : r0);
  return side == 0 ? r0 : r1;
}

// Beta Interactions (Device)
// ==========================

dfn Term d_app_era(u64 *itrs) {
  (*itrs)++;
  return d_Era();
}

dfn Term d_app_stuck(Term *heap, u32 *alloc, u64 *itrs, Term fun, Term arg) {
  (*itrs)++;
  return d_Dry(heap, alloc, fun, arg);
}

dfn Term d_app_lam(Term *heap, u64 *itrs, Term lam, Term arg) {
  (*itrs)++;
  u32  loc  = val(lam);
  Term body = heap[loc];
  d_subst_var(heap, loc, arg);
  return body;
}

dfn Term d_app_sup(Term *heap, u32 *alloc, u64 *itrs, Term app, Term sup) {
  (*itrs)++;
  u32  app_loc = val(app);
  u32  sup_loc = val(sup);
  u32  lab     = ext(sup);
  Term arg     = heap[app_loc + 1];
  Term tm1     = heap[sup_loc + 1];
  u32  loc     = *alloc;
  *alloc += 3;
  heap[loc + 2] = arg;
  Copy D = d_clone_at(loc + 2, lab);
  heap[sup_loc + 1] = D.k0;
  Term ap0 = new_term(0, APP, 0, sup_loc);
  Term ap1 = d_AppAt(heap, loc, tm1, D.k1);
  return d_SupAt(heap, app_loc, lab, ap0, ap1);
}

// Match Interactions (Device)
// ===========================

dfn Term d_app_mat_sup(Term *heap, u32 *alloc, u64 *itrs, Term mat, Term sup) {
  (*itrs)++;
  u32  lab = ext(sup);
  Copy M   = d_clone(heap, alloc, lab, mat);
  u32  loc = val(sup);
  Term a   = heap[loc + 0];
  Term b   = heap[loc + 1];
  return d_Sup(heap, alloc, lab, d_App(heap, alloc, M.k0, a), d_App(heap, alloc, M.k1, b));
}

dfn Term d_app_mat_ctr(Term *heap, u32 *alloc, u64 *itrs, Term mat, Term ctr) {
  (*itrs)++;
  u32 ari = tag(ctr) - CTR;
  if (ext(mat) == ext(ctr)) {
    Term res = heap[val(mat)];
    for (u32 i = 0; i < ari; i++) {
      res = d_App(heap, alloc, res, heap[val(ctr) + i]);
    }
    return res;
  } else {
    return d_App(heap, alloc, heap[val(mat) + 1], ctr);
  }
}

// Dup Interactions (Device)
// =========================

dfn Term d_dup_lam(Term *heap, u32 *alloc, u64 *itrs, u32 lab, u32 loc, u8 side, Term lam) {
  (*itrs)++;
  u32  lam_loc = val(lam);
  Term bod     = heap[lam_loc];
  u32  a       = *alloc;
  *alloc += 5;
  heap[a + 4]  = bod;
  Copy B  = d_clone_at(a + 4, lab);
  Term su = d_SupAt(heap, a + 2, lab, d_Var(a), d_Var(a + 1));
  Term l0 = d_LamAt(heap, a + 0, B.k0);
  Term l1 = d_LamAt(heap, a + 1, B.k1);
  d_subst_var(heap, lam_loc, su);
  return d_subst_cop(heap, side, loc, l0, l1);
}

dfn Term d_dup_sup(Term *heap, u32 *alloc, u64 *itrs, u32 lab, u32 loc, u8 side, Term sup) {
  (*itrs)++;
  u32 sup_loc = val(sup);
  u32 sup_lab = ext(sup);
  if (lab == sup_lab) {
    Term tm0 = heap[sup_loc + 0];
    Term tm1 = heap[sup_loc + 1];
    return d_subst_cop(heap, side, loc, tm0, tm1);
  } else {
    Copy A  = d_clone_at(sup_loc + 0, lab);
    Copy B  = d_clone_at(sup_loc + 1, lab);
    u32  a  = *alloc;
    *alloc += 4;
    Term s0 = d_SupAt(heap, a + 0, sup_lab, A.k0, B.k0);
    Term s1 = d_SupAt(heap, a + 2, sup_lab, A.k1, B.k1);
    return d_subst_cop(heap, side, loc, s0, s1);
  }
}

dfn Term d_dup_node(Term *heap, u32 *alloc, u64 *itrs, u32 lab, u32 loc, u8 side, Term term) {
  (*itrs)++;
  u32 ari = arity_of(term);
  if (ari == 0) {
    d_subst_var(heap, loc, term);
    return term;
  }
  u32  t_loc = val(term);
  u32  t_ext = ext(term);
  u8   t_tag = tag(term);
  Term args0[16], args1[16];
  for (u32 i = 0; i < ari; i++) {
    Copy A   = d_clone(heap, alloc, lab, heap[t_loc + i]);
    args0[i] = A.k0;
    args1[i] = A.k1;
  }
  Term r0 = d_New(heap, alloc, t_tag, t_ext, ari, args0);
  Term r1 = d_New(heap, alloc, t_tag, t_ext, ari, args1);
  return d_subst_cop(heap, side, loc, r0, r1);
}

// Alloc Helpers (Device)
// ======================

dfn u32 d_bind_at(Term *heap, u32 ls, u32 idx) {
  for (u32 i = 0; i < idx && ls != 0; i++) {
    ls = (u32)(heap[ls] & 0xFFFFFFFF);
  }
  return (ls != 0) ? (u32)(heap[ls] >> 32) : 0;
}

dfn u32 d_make_bind(Term *heap, u32 *alloc, u32 tail, u32 loc) {
  u32 entry = (*alloc)++;
  heap[entry] = ((u64)loc << 32) | tail;
  return entry;
}

dfn Term d_make_alo(Term *heap, u32 *alloc, u32 ls_loc, u32 tm_loc) {
  u32 loc = (*alloc)++;
  heap[loc] = ((u64)ls_loc << 32) | tm_loc;
  return new_term(0, ALO, 0, loc);
}

// Alloc Interactions (Device)
// ===========================

dfn Term d_alo_var(Term *heap, u32 ls_loc, u32 idx) {
  u32 bind = d_bind_at(heap, ls_loc, idx);
  return bind ? d_Var(bind) : new_term(0, VAR, 0, idx);
}

dfn Term d_alo_cop(Term *heap, u32 ls_loc, u32 idx, u32 lab, u8 side) {
  u32 bind = d_bind_at(heap, ls_loc, idx);
  u8  tg   = side == 0 ? CO0 : CO1;
  return bind ? new_term(0, tg, lab, bind) : new_term(0, tg, lab, idx);
}

dfn Term d_alo_lam(Term *heap, u32 *alloc, u32 ls_loc, u32 book_body_loc) {
  u32 lam_body = (*alloc)++;
  u32 new_bind = d_make_bind(heap, alloc, ls_loc, lam_body);
  heap[lam_body] = d_make_alo(heap, alloc, new_bind, book_body_loc);
  return new_term(0, LAM, 0, lam_body);
}

dfn Term d_alo_dup(Term *heap, u32 *alloc, u32 ls_loc, u32 book_loc, u32 lab) {
  u32 dup_val  = (*alloc)++;
  u32 new_bind = d_make_bind(heap, alloc, ls_loc, dup_val);
  heap[dup_val] = d_make_alo(heap, alloc, ls_loc, book_loc + 0);
  return d_Dup(heap, alloc, lab, d_make_alo(heap, alloc, ls_loc, book_loc + 0), d_make_alo(heap, alloc, new_bind, book_loc + 1));
}

dfn Term d_alo_node(Term *heap, u32 *alloc, u32 ls_loc, u32 loc, u8 tg, u32 ex, u32 ari) {
  Term args[16];
  for (u32 i = 0; i < ari; i++) {
    args[i] = d_make_alo(heap, alloc, ls_loc, loc + i);
  }
  return d_New(heap, alloc, tg, ex, ari, args);
}

// WNF (Device)
// ============

__device__ Term d_wnf(Term *heap, Term *stack, u32 *alloc, u64 *itrs, u32 *book, Term term) {
  u32 s_pos = 0;
  Term next = term;
  Term whnf;

  enter: {
    switch (tag(next)) {
      case VAR: {
        u32 loc = val(next);
        if (sub_of(heap[loc])) {
          next = clear_sub(heap[loc]);
          goto enter;
        }
        whnf = next;
        goto apply;
      }

      case CO0:
      case CO1: {
        u32 loc = val(next);
        if (sub_of(heap[loc])) {
          next = clear_sub(heap[loc]);
          goto enter;
        }
        Term dup_val = heap[loc];
        stack[s_pos++] = next;
        next = dup_val;
        goto enter;
      }

      case APP: {
        u32  loc = val(next);
        Term fun = heap[loc];
        stack[s_pos++] = next;
        next = fun;
        goto enter;
      }

      case DUP: {
        u32  loc  = val(next);
        Term body = heap[loc + 1];
        next = body;
        goto enter;
      }

      case REF: {
        u32 nam = ext(next);
        if (book[nam] != 0) {
          next = d_make_alo(heap, alloc, 0, book[nam]);
          goto enter;
        }
        whnf = next;
        goto apply;
      }

      case ALO: {
        u32  alo_loc = val(next);
        u64  pair    = heap[alo_loc];
        u32  tm_loc  = (u32)(pair & 0xFFFFFFFF);
        u32  ls_loc  = (u32)(pair >> 32);
        Term bk      = heap[tm_loc];

        switch (tag(bk)) {
          case VAR: {
            next = d_alo_var(heap, ls_loc, val(bk));
            goto enter;
          }
          case CO0:
          case CO1: {
            next = d_alo_cop(heap, ls_loc, val(bk), ext(bk), tag(bk) == CO0 ? 0 : 1);
            goto enter;
          }
          case LAM: {
            next = d_alo_lam(heap, alloc, ls_loc, val(bk));
            goto enter;
          }
          case APP:
          case SUP:
          case MAT:
          case DRY: {
            next = d_alo_node(heap, alloc, ls_loc, val(bk), tag(bk), ext(bk), arity_of(bk));
            goto enter;
          }
          case DUP: {
            next = d_alo_dup(heap, alloc, ls_loc, val(bk), ext(bk));
            goto enter;
          }
          case REF:
          case NAM:
          case ERA: {
            next = bk;
            goto enter;
          }
          default: {
            if (tag(bk) >= CTR && tag(bk) <= CTR + CTR_MAX_ARI) {
              next = d_alo_node(heap, alloc, ls_loc, val(bk), tag(bk), ext(bk), arity_of(bk));
              goto enter;
            }
            whnf = next;
            goto apply;
          }
        }
      }

      case NAM:
      case DRY:
      case ERA:
      case SUP:
      case LAM:
      case MAT: {
        whnf = next;
        goto apply;
      }

      default: {
        if (tag(next) >= CTR && tag(next) <= CTR + CTR_MAX_ARI) {
          whnf = next;
          goto apply;
        }
        whnf = next;
        goto apply;
      }
    }
  }

  apply: {
    while (s_pos > 0) {
      Term frame = stack[--s_pos];

      switch (tag(frame)) {
        case APP: {
          u32  loc = val(frame);
          Term arg = heap[loc + 1];

          switch (tag(whnf)) {
            case ERA: {
              whnf = d_app_era(itrs);
              continue;
            }
            case NAM:
            case DRY: {
              whnf = d_app_stuck(heap, alloc, itrs, whnf, arg);
              continue;
            }
            case LAM: {
              whnf = d_app_lam(heap, itrs, whnf, arg);
              next = whnf;
              goto enter;
            }
            case SUP: {
              whnf = d_app_sup(heap, alloc, itrs, frame, whnf);
              next = whnf;
              goto enter;
            }
            case MAT: {
              stack[s_pos++] = whnf;
              next = arg;
              goto enter;
            }
            default: {
              whnf = d_App(heap, alloc, whnf, arg);
              continue;
            }
          }
        }

        case MAT: {
          Term mat = frame;
          switch (tag(whnf)) {
            case ERA: {
              whnf = d_app_era(itrs);
              continue;
            }
            case SUP: {
              whnf = d_app_mat_sup(heap, alloc, itrs, mat, whnf);
              next = whnf;
              goto enter;
            }
            default: {
              if (tag(whnf) >= CTR && tag(whnf) <= CTR + CTR_MAX_ARI) {
                whnf = d_app_mat_ctr(heap, alloc, itrs, mat, whnf);
                next = whnf;
                goto enter;
              }
              whnf = d_App(heap, alloc, mat, whnf);
              continue;
            }
          }
        }

        case CO0:
        case CO1: {
          u8  side = (tag(frame) == CO0) ? 0 : 1;
          u32 loc  = val(frame);
          u32 lab  = ext(frame);

          switch (tag(whnf)) {
            case LAM: {
              whnf = d_dup_lam(heap, alloc, itrs, lab, loc, side, whnf);
              next = whnf;
              goto enter;
            }
            case SUP: {
              whnf = d_dup_sup(heap, alloc, itrs, lab, loc, side, whnf);
              next = whnf;
              goto enter;
            }
            case ERA:
            case NAM: {
              whnf = d_dup_node(heap, alloc, itrs, lab, loc, side, whnf);
              continue;
            }
            case MAT:
            case DRY: {
              whnf = d_dup_node(heap, alloc, itrs, lab, loc, side, whnf);
              next = whnf;
              goto enter;
            }
            default: {
              if (tag(whnf) >= CTR && tag(whnf) <= CTR + CTR_MAX_ARI) {
                whnf = d_dup_node(heap, alloc, itrs, lab, loc, side, whnf);
                next = whnf;
                goto enter;
              }
              u32 new_loc = (*alloc)++;
              heap[new_loc] = whnf;
              d_subst_var(heap, loc, new_term(0, side == 0 ? CO1 : CO0, lab, new_loc));
              whnf = new_term(0, side == 0 ? CO0 : CO1, lab, new_loc);
              continue;
            }
          }
        }

        default: {
          continue;
        }
      }
    }
  }

  return whnf;
}

// SNF (Device) - Iterative version using manual stack
// ====================================================
//
// The recursive d_snf would use GPU call stack which is very limited.
// This iterative version uses our pre-allocated stack memory instead.
// Stack layout per thread:
//   [0, stack_size/2)         - used by WNF
//   [stack_size/2, stack_size) - used by SNF entries (loc:u32, depth:u32)

typedef struct {
  u32 loc;    // Heap location to process
  u32 depth;  // Current depth for variable naming
} SNFEntry;

__device__ Term d_snf(Term *heap, Term *stack, u32 *alloc, u64 *itrs, u32 *book, Term term, u32 stack_size) {
  // SNF uses second half of stack (reinterpreted as SNFEntry array)
  SNFEntry *snf_stack = (SNFEntry*)(stack + stack_size / 2);
  u32 snf_pos = 0;

  // Process initial term
  Term result = d_wnf(heap, stack, alloc, itrs, book, term);
  u32 ari = arity_of(result);

  if (ari > 0) {
    u32 loc = val(result);
    if (tag(result) == LAM) {
      d_subst_var(heap, loc, d_Nam(1));
      snf_stack[snf_pos++] = (SNFEntry){loc, 1};
    } else {
      for (u32 i = 0; i < ari; i++) {
        snf_stack[snf_pos++] = (SNFEntry){loc + i, 0};
      }
    }
  }

  // Process stack iteratively
  while (snf_pos > 0) {
    SNFEntry entry = snf_stack[--snf_pos];

    // WNF the term at this location
    Term t = d_wnf(heap, stack, alloc, itrs, book, heap[entry.loc]);
    heap[entry.loc] = t;

    // Push children onto SNF stack
    ari = arity_of(t);
    if (ari > 0) {
      u32 loc = val(t);
      if (tag(t) == LAM) {
        d_subst_var(heap, loc, d_Nam(entry.depth + 1));
        snf_stack[snf_pos++] = (SNFEntry){loc, entry.depth + 1};
      } else {
        for (u32 i = 0; i < ari; i++) {
          snf_stack[snf_pos++] = (SNFEntry){loc + i, entry.depth};
        }
      }
    }
  }

  return result;
}

// Multi-threaded Kernel
// =====================
//
// Toggle for memory layout comparison:
// COALESCED=1: Interleaved layout for coalesced access
// COALESCED=0: Blocked layout (each thread has contiguous memory)

#ifndef COALESCED
#define COALESCED 1
#endif

// Heap accessor - abstracts memory layout
// Layout:
//   [0, book_size) - shared book terms (all threads read same data)
//   [book_size, ...) - per-warp slices, interleaved within each warp for coalescing
//
// Within warp slice: thread lane 0 gets indices 0,32,64,...; lane 1 gets 1,33,65,...
// When all 32 threads in a warp access heap[idx], they hit 32 consecutive addresses = coalesced
//
// Physical address for thread tid accessing idx (where idx >= book_size):
//   warp_id = tid / 32
//   lane_id = tid % 32
//   local_idx = idx - book_size
//   addr = book_size + warp_id * warp_slice_size + local_idx * 32 + lane_id
struct Heap {
  Term *base;           // Single global buffer
  u32 book_size;        // Size of shared book region
  u64 warp_slice_size;  // = heap_per_thr * 32 (size of one warp's slice in terms)
  u32 warp_id;          // tid / 32
  u32 lane_id;          // tid % 32

  __device__ __forceinline__ Term& operator[](u32 idx) {
    if (idx < book_size) {
      // Book region: shared, direct access
      return base[idx];
    } else {
      // Per-warp slice with interleaved layout for coalescing
      u32 local_idx = idx - book_size;
      return base[book_size + warp_id * warp_slice_size + (u64)local_idx * 32 + lane_id];
    }
  }
};

// Stats for tracking max usage
struct Stats {
  u32 max_wnf;  // Max WNF stack depth
  u32 max_snf;  // Max SNF stack depth
};

// Device WNF with heap accessor
__device__ Term d_wnf_heap(Heap &heap, Term *stack, u32 *alloc, u64 *itrs, u32 *book, Term term, Stats *stats = nullptr) {
  u32 s_pos = 0;
  u32 max_wnf = 0;
  Term next = term;
  Term whnf;

  enter: {
    switch (tag(next)) {
      case VAR: {
        u32 loc = val(next);
        Term h = heap[loc];
        if (sub_of(h)) {
          next = clear_sub(h);
          goto enter;
        }
        whnf = next;
        goto apply;
      }

      case CO0:
      case CO1: {
        u32 loc = val(next);
        Term h = heap[loc];
        if (sub_of(h)) {
          next = clear_sub(h);
          goto enter;
        }
        stack[s_pos++] = next;
        if (s_pos > max_wnf) max_wnf = s_pos;
        next = h;
        goto enter;
      }

      case APP: {
        u32 loc = val(next);
        Term fun = heap[loc];
        stack[s_pos++] = next;
        if (s_pos > max_wnf) max_wnf = s_pos;
        next = fun;
        goto enter;
      }

      case DUP: {
        u32 loc = val(next);
        Term body = heap[loc + 1];
        next = body;
        goto enter;
      }

      case REF: {
        u32 nam = ext(next);
        if (book[nam] != 0) {
          // Inline ALO creation
          u32 alo_loc = (*alloc)++;
          heap[alo_loc] = ((u64)0 << 32) | book[nam];
          next = new_term(0, ALO, 0, alo_loc);
          goto enter;
        }
        whnf = next;
        goto apply;
      }

      case ALO: {
        u32 alo_loc = val(next);
        u64 pair = heap[alo_loc];
        u32 tm_loc = (u32)(pair & 0xFFFFFFFF);
        u32 ls_loc = (u32)(pair >> 32);
        Term bk = heap[tm_loc];

        switch (tag(bk)) {
          case VAR: {
            // d_alo_var inlined
            u32 idx = val(bk);
            u32 bind = 0;
            u32 ls = ls_loc;
            for (u32 i = 0; i < idx && ls != 0; i++) {
              ls = (u32)(heap[ls] & 0xFFFFFFFF);
            }
            if (ls != 0) bind = (u32)(heap[ls] >> 32);
            next = bind ? d_Var(bind) : new_term(0, VAR, 0, idx);
            goto enter;
          }
          case CO0:
          case CO1: {
            u32 idx = val(bk);
            u32 lab = ext(bk);
            u8 side = (tag(bk) == CO0) ? 0 : 1;
            u32 bind = 0;
            u32 ls = ls_loc;
            for (u32 i = 0; i < idx && ls != 0; i++) {
              ls = (u32)(heap[ls] & 0xFFFFFFFF);
            }
            if (ls != 0) bind = (u32)(heap[ls] >> 32);
            u8 tg = side == 0 ? CO0 : CO1;
            next = bind ? new_term(0, tg, lab, bind) : new_term(0, tg, lab, idx);
            goto enter;
          }
          case LAM: {
            u32 book_body_loc = val(bk);
            u32 lam_body = (*alloc)++;
            u32 entry = (*alloc)++;
            heap[entry] = ((u64)lam_body << 32) | ls_loc;
            u32 alo2 = (*alloc)++;
            heap[alo2] = ((u64)entry << 32) | book_body_loc;
            heap[lam_body] = new_term(0, ALO, 0, alo2);
            next = new_term(0, LAM, 0, lam_body);
            goto enter;
          }
          case APP:
          case SUP:
          case MAT:
          case DRY: {
            u32 loc = val(bk);
            u8 tg = tag(bk);
            u32 ex = ext(bk);
            u32 ari = (tg == LAM) ? 1 : 2;
            Term args[2];
            for (u32 i = 0; i < ari; i++) {
              u32 a = (*alloc)++;
              heap[a] = ((u64)ls_loc << 32) | (loc + i);
              args[i] = new_term(0, ALO, 0, a);
            }
            u32 new_loc = *alloc;
            *alloc += ari;
            for (u32 i = 0; i < ari; i++) {
              heap[new_loc + i] = args[i];
            }
            next = new_term(0, tg, ex, new_loc);
            goto enter;
          }
          case DUP: {
            u32 loc = val(bk);
            u32 lab = ext(bk);
            u32 dup_val = (*alloc)++;
            u32 entry = (*alloc)++;
            heap[entry] = ((u64)dup_val << 32) | ls_loc;
            u32 alo_v = (*alloc)++;
            heap[alo_v] = ((u64)ls_loc << 32) | (loc + 0);
            u32 alo_b = (*alloc)++;
            heap[alo_b] = ((u64)entry << 32) | (loc + 1);
            heap[dup_val] = new_term(0, ALO, 0, alo_v);
            u32 dup_loc = *alloc;
            *alloc += 2;
            heap[dup_loc + 0] = new_term(0, ALO, 0, alo_v);
            heap[dup_loc + 1] = new_term(0, ALO, 0, alo_b);
            next = new_term(0, DUP, lab, dup_loc);
            goto enter;
          }
          case REF:
          case NAM:
          case ERA: {
            next = bk;
            goto enter;
          }
          default: {
            if (tag(bk) >= CTR && tag(bk) <= CTR + CTR_MAX_ARI) {
              u32 loc = val(bk);
              u8 tg = tag(bk);
              u32 ex = ext(bk);
              u32 ari = tg - CTR;
              Term args[16];
              for (u32 i = 0; i < ari; i++) {
                u32 a = (*alloc)++;
                heap[a] = ((u64)ls_loc << 32) | (loc + i);
                args[i] = new_term(0, ALO, 0, a);
              }
              u32 new_loc = *alloc;
              *alloc += ari;
              for (u32 i = 0; i < ari; i++) {
                heap[new_loc + i] = args[i];
              }
              next = new_term(0, tg, ex, new_loc);
              goto enter;
            }
            whnf = next;
            goto apply;
          }
        }
      }

      case NAM:
      case DRY:
      case ERA:
      case SUP:
      case LAM:
      case MAT: {
        whnf = next;
        goto apply;
      }

      default: {
        if (tag(next) >= CTR && tag(next) <= CTR + CTR_MAX_ARI) {
          whnf = next;
          goto apply;
        }
        whnf = next;
        goto apply;
      }
    }
  }

  apply: {
    while (s_pos > 0) {
      Term frame = stack[--s_pos];

      switch (tag(frame)) {
        case APP: {
          u32 loc = val(frame);
          Term arg = heap[loc + 1];

          switch (tag(whnf)) {
            case ERA: {
              (*itrs)++;
              whnf = d_Era();
              continue;
            }
            case NAM:
            case DRY: {
              (*itrs)++;
              u32 dry_loc = *alloc;
              *alloc += 2;
              heap[dry_loc + 0] = whnf;
              heap[dry_loc + 1] = arg;
              whnf = new_term(0, DRY, 0, dry_loc);
              continue;
            }
            case LAM: {
              (*itrs)++;
              u32 lam_loc = val(whnf);
              Term body = heap[lam_loc];
              heap[lam_loc] = mark_sub(arg);
              whnf = body;
              next = whnf;
              goto enter;
            }
            case SUP: {
              (*itrs)++;
              u32 app_loc = loc;
              u32 sup_loc = val(whnf);
              u32 lab = ext(whnf);
              Term tm1 = heap[sup_loc + 1];
              u32 a = *alloc;
              *alloc += 3;
              heap[a + 2] = arg;
              Term co0 = new_term(0, CO0, lab, a + 2);
              Term co1 = new_term(0, CO1, lab, a + 2);
              heap[sup_loc + 1] = co0;
              Term ap0 = new_term(0, APP, 0, sup_loc);
              heap[a + 0] = tm1;
              heap[a + 1] = co1;
              Term ap1 = new_term(0, APP, 0, a);
              heap[app_loc + 0] = ap0;
              heap[app_loc + 1] = ap1;
              whnf = new_term(0, SUP, lab, app_loc);
              next = whnf;
              goto enter;
            }
            case MAT: {
              stack[s_pos++] = whnf;
              if (s_pos > max_wnf) max_wnf = s_pos;
              next = arg;
              goto enter;
            }
            default: {
              u32 new_loc = *alloc;
              *alloc += 2;
              heap[new_loc + 0] = whnf;
              heap[new_loc + 1] = arg;
              whnf = new_term(0, APP, 0, new_loc);
              continue;
            }
          }
        }

        case MAT: {
          Term mat = frame;
          switch (tag(whnf)) {
            case ERA: {
              (*itrs)++;
              whnf = d_Era();
              continue;
            }
            case SUP: {
              (*itrs)++;
              u32 lab = ext(whnf);
              u32 cloc = (*alloc)++;
              heap[cloc] = mat;
              Term co0 = new_term(0, CO0, lab, cloc);
              Term co1 = new_term(0, CO1, lab, cloc);
              u32 sup_loc = val(whnf);
              Term a = heap[sup_loc + 0];
              Term b = heap[sup_loc + 1];
              u32 ap0_loc = *alloc; *alloc += 2;
              u32 ap1_loc = *alloc; *alloc += 2;
              heap[ap0_loc + 0] = co0;
              heap[ap0_loc + 1] = a;
              heap[ap1_loc + 0] = co1;
              heap[ap1_loc + 1] = b;
              u32 new_sup = *alloc; *alloc += 2;
              heap[new_sup + 0] = new_term(0, APP, 0, ap0_loc);
              heap[new_sup + 1] = new_term(0, APP, 0, ap1_loc);
              whnf = new_term(0, SUP, lab, new_sup);
              next = whnf;
              goto enter;
            }
            default: {
              if (tag(whnf) >= CTR && tag(whnf) <= CTR + CTR_MAX_ARI) {
                (*itrs)++;
                u32 ari = tag(whnf) - CTR;
                if (ext(mat) == ext(whnf)) {
                  Term res = heap[val(mat)];
                  for (u32 i = 0; i < ari; i++) {
                    u32 ap_loc = *alloc; *alloc += 2;
                    heap[ap_loc + 0] = res;
                    heap[ap_loc + 1] = heap[val(whnf) + i];
                    res = new_term(0, APP, 0, ap_loc);
                  }
                  whnf = res;
                  next = whnf;
                  goto enter;
                } else {
                  u32 ap_loc = *alloc; *alloc += 2;
                  heap[ap_loc + 0] = heap[val(mat) + 1];
                  heap[ap_loc + 1] = whnf;
                  whnf = new_term(0, APP, 0, ap_loc);
                  next = whnf;
                  goto enter;
                }
              }
              u32 ap_loc = *alloc; *alloc += 2;
              heap[ap_loc + 0] = mat;
              heap[ap_loc + 1] = whnf;
              whnf = new_term(0, APP, 0, ap_loc);
              continue;
            }
          }
        }

        case CO0:
        case CO1: {
          u8 side = (tag(frame) == CO0) ? 0 : 1;
          u32 loc = val(frame);
          u32 lab = ext(frame);

          switch (tag(whnf)) {
            case LAM: {
              (*itrs)++;
              u32 lam_loc = val(whnf);
              Term bod = heap[lam_loc];
              u32 a = *alloc; *alloc += 5;
              heap[a + 4] = bod;
              Term co0_b = new_term(0, CO0, lab, a + 4);
              Term co1_b = new_term(0, CO1, lab, a + 4);
              u32 sup_loc = a + 2;
              heap[sup_loc + 0] = d_Var(a);
              heap[sup_loc + 1] = d_Var(a + 1);
              Term su = new_term(0, SUP, lab, sup_loc);
              heap[a + 0] = co0_b;
              heap[a + 1] = co1_b;
              Term l0 = new_term(0, LAM, 0, a + 0);
              Term l1 = new_term(0, LAM, 0, a + 1);
              heap[lam_loc] = mark_sub(su);
              heap[loc] = mark_sub(side == 0 ? l1 : l0);
              whnf = side == 0 ? l0 : l1;
              next = whnf;
              goto enter;
            }
            case SUP: {
              (*itrs)++;
              u32 sup_loc = val(whnf);
              u32 sup_lab = ext(whnf);
              if (lab == sup_lab) {
                Term tm0 = heap[sup_loc + 0];
                Term tm1 = heap[sup_loc + 1];
                heap[loc] = mark_sub(side == 0 ? tm1 : tm0);
                whnf = side == 0 ? tm0 : tm1;
                next = whnf;
                goto enter;
              } else {
                Term co0_a = new_term(0, CO0, lab, sup_loc + 0);
                Term co1_a = new_term(0, CO1, lab, sup_loc + 0);
                Term co0_b = new_term(0, CO0, lab, sup_loc + 1);
                Term co1_b = new_term(0, CO1, lab, sup_loc + 1);
                u32 a = *alloc; *alloc += 4;
                heap[a + 0] = co0_a;
                heap[a + 1] = co0_b;
                heap[a + 2] = co1_a;
                heap[a + 3] = co1_b;
                Term s0 = new_term(0, SUP, sup_lab, a + 0);
                Term s1 = new_term(0, SUP, sup_lab, a + 2);
                heap[loc] = mark_sub(side == 0 ? s1 : s0);
                whnf = side == 0 ? s0 : s1;
                next = whnf;
                goto enter;
              }
            }
            case ERA:
            case NAM: {
              (*itrs)++;
              heap[loc] = mark_sub(whnf);
              continue;
            }
            case MAT:
            case DRY: {
              (*itrs)++;
              u32 ari = arity_of(whnf);
              u32 t_loc = val(whnf);
              u32 t_ext = ext(whnf);
              u8 t_tag = tag(whnf);
              Term args0[2], args1[2];
              for (u32 i = 0; i < ari; i++) {
                u32 c = (*alloc)++;
                heap[c] = heap[t_loc + i];
                args0[i] = new_term(0, CO0, lab, c);
                args1[i] = new_term(0, CO1, lab, c);
              }
              u32 loc0 = *alloc; *alloc += ari;
              u32 loc1 = *alloc; *alloc += ari;
              for (u32 i = 0; i < ari; i++) {
                heap[loc0 + i] = args0[i];
                heap[loc1 + i] = args1[i];
              }
              Term r0 = new_term(0, t_tag, t_ext, loc0);
              Term r1 = new_term(0, t_tag, t_ext, loc1);
              heap[loc] = mark_sub(side == 0 ? r1 : r0);
              whnf = side == 0 ? r0 : r1;
              next = whnf;
              goto enter;
            }
            default: {
              if (tag(whnf) >= CTR && tag(whnf) <= CTR + CTR_MAX_ARI) {
                (*itrs)++;
                u32 ari = tag(whnf) - CTR;
                u32 t_loc = val(whnf);
                u32 t_ext = ext(whnf);
                u8 t_tag = tag(whnf);
                Term args0[16], args1[16];
                for (u32 i = 0; i < ari; i++) {
                  u32 c = (*alloc)++;
                  heap[c] = heap[t_loc + i];
                  args0[i] = new_term(0, CO0, lab, c);
                  args1[i] = new_term(0, CO1, lab, c);
                }
                u32 loc0 = *alloc; *alloc += ari;
                u32 loc1 = *alloc; *alloc += ari;
                for (u32 i = 0; i < ari; i++) {
                  heap[loc0 + i] = args0[i];
                  heap[loc1 + i] = args1[i];
                }
                Term r0 = new_term(0, t_tag, t_ext, loc0);
                Term r1 = new_term(0, t_tag, t_ext, loc1);
                heap[loc] = mark_sub(side == 0 ? r1 : r0);
                whnf = side == 0 ? r0 : r1;
                next = whnf;
                goto enter;
              }
              u32 new_loc = (*alloc)++;
              heap[new_loc] = whnf;
              heap[loc] = mark_sub(new_term(0, side == 0 ? CO1 : CO0, lab, new_loc));
              whnf = new_term(0, side == 0 ? CO0 : CO1, lab, new_loc);
              continue;
            }
          }
        }

        default: {
          continue;
        }
      }
    }
  }

  if (stats) {
    if (max_wnf > stats->max_wnf) stats->max_wnf = max_wnf;
  }
  return whnf;
}

// SNF with heap accessor and stats tracking
__device__ Term d_snf_heap(Heap &heap, Term *stack, u32 *alloc, u64 *itrs, u32 *book, Term term, u32 stack_size, Stats *stats) {
  typedef struct { u32 loc; u32 depth; } SNFEntry;
  SNFEntry *snf_stack = (SNFEntry*)(stack + stack_size / 2);
  u32 snf_pos = 0;
  u32 max_snf = 0;

  Term result = d_wnf_heap(heap, stack, alloc, itrs, book, term, stats);
  u32 ari = arity_of(result);

  if (ari > 0) {
    u32 loc = val(result);
    if (tag(result) == LAM) {
      heap[loc] = mark_sub(d_Nam(1));
      snf_stack[snf_pos++] = {loc, 1};
    } else {
      for (u32 i = 0; i < ari; i++) {
        snf_stack[snf_pos++] = {loc + i, 0};
      }
    }
    if (snf_pos > max_snf) max_snf = snf_pos;
  }

  while (snf_pos > 0) {
    SNFEntry entry = snf_stack[--snf_pos];
    Term t = d_wnf_heap(heap, stack, alloc, itrs, book, heap[entry.loc], stats);
    heap[entry.loc] = t;
    ari = arity_of(t);
    if (ari > 0) {
      u32 loc = val(t);
      if (tag(t) == LAM) {
        heap[loc] = mark_sub(d_Nam(entry.depth + 1));
        snf_stack[snf_pos++] = {loc, entry.depth + 1};
      } else {
        for (u32 i = 0; i < ari; i++) {
          snf_stack[snf_pos++] = {loc + i, entry.depth};
        }
      }
      if (snf_pos > max_snf) max_snf = snf_pos;
    }
  }

  stats->max_snf = max_snf;
  return result;
}

__global__ void hvm_kernel_mt(
  Term * __restrict__ global_heap,    // Single heap: [book | warp0 slice | warp1 slice | ...]
  Term * __restrict__ global_stack,   // Single stack: [thread0 | thread1 | ...]
  u32  * __restrict__ book,           // Shared book dict (name -> loc)
  u32   book_size,      // Size of book data
  Term  input,          // Input term (REF to main)
  Term * __restrict__ outputs,        // Per-thread outputs
  u64  * __restrict__ itrs_out,       // Per-thread iteration counts
  u32  * __restrict__ heap_used_out,  // Per-thread heap usage (final alloc)
  Stats * __restrict__ stats_out,     // Per-thread stats (max_wnf, max_snf)
  u32   num_threads,
  u64   heap_per_thr,   // Heap entries per thread (excluding book)
  u64   stack_per_thr   // Stack entries per thread
) {
  u32 tid = blockIdx.x * blockDim.x + threadIdx.x;
  if (tid >= num_threads) return;

  // Create heap accessor with warp-based interleaved layout
  // Each warp gets a slice of size heap_per_thr * 32
  // Within the slice, interleaved by lane for coalescing
  Heap heap;
  heap.base = global_heap;
  heap.book_size = book_size;
  heap.warp_slice_size = heap_per_thr * 32;  // Each warp's slice
  heap.warp_id = tid / 32;
  heap.lane_id = tid % 32;

  // Stack layout: each thread gets stack_per_thr entries (blocked, not interleaved)
  Term *stack = global_stack + (u64)tid * stack_per_thr;

  // Initialize allocation pointer (starts after book region)
  u32 alloc = book_size;
  u64 itrs  = 0;
  Stats stats = {0, 0};

  // Run SNF
  Term result = d_snf_heap(heap, stack, &alloc, &itrs, book, input, (u32)stack_per_thr, &stats);

  // Store results
  outputs[tid]  = result;
  itrs_out[tid] = itrs;
  heap_used_out[tid] = alloc - book_size;  // Heap terms used (excluding book)
  stats_out[tid] = stats;
}

// Main
// ====

int main(int argc, char **argv) {
  // Query GPU info
  if (argc > 1 && strcmp(argv[1], "--gpu-info") == 0) {
    cudaDeviceProp prop;
    CUDA_CHECK(cudaGetDeviceProperties(&prop, 0));
    printf("GPU: %s\n", prop.name);
    printf("Total memory: %zu MB\n", prop.totalGlobalMem / (1024*1024));
    printf("Max threads per block: %d\n", prop.maxThreadsPerBlock);
    printf("Max threads per SM: %d\n", prop.maxThreadsPerMultiProcessor);
    printf("SM count: %d\n", prop.multiProcessorCount);
    size_t free_mem, total_mem;
    CUDA_CHECK(cudaMemGetInfo(&free_mem, &total_mem));
    printf("Free memory: %zu MB / %zu MB\n", free_mem/(1024*1024), total_mem/(1024*1024));
    return 0;
  }

  // Parse thread count from args
  int num_threads = 1024;
  int arg_idx = 1;

  if (argc > 1 && argv[1][0] == '-' && argv[1][1] == 't') {
    num_threads = atoi(argv[1] + 2);
    if (num_threads < 1) num_threads = 1;
    if (num_threads > MAX_THREADS) num_threads = MAX_THREADS;
    arg_idx = 2;
  }

  // Allocate unified memory for book (shared)
  CUDA_CHECK(cudaMallocManaged(&BOOK, BOOK_CAP * sizeof(u32)));
  memset(BOOK, 0, BOOK_CAP * sizeof(u32));

  // Allocate heap for parsing using regular malloc (CPU only during parse)
  HEAP = (Term*)malloc(DEFAULT_HEAP_PER_THR * sizeof(Term));
  if (!HEAP) {
    fprintf(stderr, "Failed to allocate parsing heap\n");
    return 1;
  }
  memset(HEAP, 0, DEFAULT_HEAP_PER_THR * sizeof(Term));

  // cnots_lite benchmark (hardcoded for testing) - 16 dup steps
  const char *test_src =
    "@ctru = λt.λf.t\n"
    "@cfal = λt.λf.f\n"
    "@cnot = λx.x(@cfal,@ctru)\n"
    "@P24  = λf.\n"
    "  ! F &A = f;\n"
    "  ! F &A = λk. F₀(F₁(k));\n"
    "  ! F &A = λk. F₀(F₁(k));\n"
    "  ! F &A = λk. F₀(F₁(k));\n"
    "  ! F &A = λk. F₀(F₁(k));\n"
    "  ! F &A = λk. F₀(F₁(k));\n"
    "  ! F &A = λk. F₀(F₁(k));\n"
    "  ! F &A = λk. F₀(F₁(k));\n"
    "  ! F &A = λk. F₀(F₁(k));\n"
    "  ! F &A = λk. F₀(F₁(k));\n"
    "  ! F &A = λk. F₀(F₁(k));\n"
    "  ! F &A = λk. F₀(F₁(k));\n"
    "  ! F &A = λk. F₀(F₁(k));\n"
    "  ! F &A = λk. F₀(F₁(k));\n"
    "  ! F &A = λk. F₀(F₁(k));\n"
    "  ! F &A = λk. F₀(F₁(k));\n"
    "  λk. F₀(F₁(k))\n"
    "@main = @P24(@cnot,@ctru)\n";

  char *src = NULL;
  if (argc > arg_idx) {
    FILE *fp = fopen(argv[arg_idx], "rb");
    if (!fp) {
      fprintf(stderr, "Error: could not open '%s'\n", argv[arg_idx]);
      return 1;
    }
    fseek(fp, 0, SEEK_END);
    long len = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    src = (char*)malloc(len + 1);
    fread(src, 1, len, fp);
    src[len] = 0;
    fclose(fp);
  } else {
    src = strdup(test_src);  // Use cnots_lite benchmark
  }

  PState s = {
    .file = (char*)(argc > arg_idx ? argv[arg_idx] : "<inline>"),
    .src  = src,
    .pos  = 0,
    .len  = (u32)strlen(src),
    .line = 1,
    .col  = 1
  };
  parse_def(&s);
  free(src);

  u32 book_size = H_ALLOC;

  // Get @main name hash
  u32 main_name = 0;
  const char *p = "main";
  while (*p) {
    main_name = ((main_name << 6) + char_to_b64(*p)) & EXT_MASK;
    p++;
  }

  if (BOOK[main_name] == 0) {
    fprintf(stderr, "Error: @main not defined\n");
    return 1;
  }

  Term main_ref = new_term(0, REF, main_name, 0);

  printf("Threads: %d\n", num_threads);
  printf("Input: ");
  print_term(HEAP[BOOK[main_name]], HEAP);
  printf("\n");

  // Query GPU memory and partition: 50% heap, 50% stack
  size_t free_mem, total_mem;
  CUDA_CHECK(cudaMemGetInfo(&free_mem, &total_mem));

  // Reserve memory for BOOK, book_data, outputs, itrs_out, CUDA call stack, and system
  // Use 2GB base + 64KB per thread for CUDA internal structures
  size_t reserved = BOOK_CAP * sizeof(u32) + book_size * sizeof(Term) +
                    num_threads * sizeof(Term) + num_threads * sizeof(u64) +
                    num_threads * 64 * 1024 +  // CUDA internal per-thread overhead
                    2ULL * 1024 * 1024 * 1024;  // 2 GB safety margin
  size_t usable_mem = (free_mem > reserved) ? (free_mem - reserved) : 0;

  // Per-thread allocation: 90% heap, 10% stack (in terms)
  // Divide available memory by thread count, then cap at maximums
  u64 bytes_per_thr = usable_mem / num_threads;
  u64 heap_bytes_per_thr = (bytes_per_thr * 9) / 10;
  u64 stack_bytes_per_thr = bytes_per_thr / 10;

  u64 heap_per_thr  = heap_bytes_per_thr / sizeof(Term);
  u64 stack_per_thr = stack_bytes_per_thr / sizeof(Term);

  // Cap at reasonable maximums
  u64 max_heap_per_thr = 1ULL << 23;   // 8M terms = 64MB
  u64 max_stack_per_thr = 1ULL << 19;  // 512K entries = 4MB
  if (heap_per_thr > max_heap_per_thr) heap_per_thr = max_heap_per_thr;
  if (stack_per_thr > max_stack_per_thr) stack_per_thr = max_stack_per_thr;

  // Total heap = book region + per-warp slices
  // Each warp slice is heap_per_thr * 32 terms (interleaved layout)
  u32 num_warps = (num_threads + 31) / 32;
  u64 heap_bytes  = (book_size + (u64)num_warps * heap_per_thr * 32) * sizeof(Term);
  u64 stack_bytes = (u64)num_threads * stack_per_thr * sizeof(Term);

  printf("Memory: %.1f GB total, %.1f GB usable\n",
         total_mem / (1024.0*1024*1024), usable_mem / (1024.0*1024*1024));
  printf("Book: %u terms (%.1f KB)\n", book_size, book_size * 8 / 1024.0);
  printf("Per-thread: %llu MB heap (%llu terms), %llu MB stack (%llu entries)\n",
         heap_per_thr * 8 / (1024*1024), heap_per_thr,
         stack_per_thr * 8 / (1024*1024), stack_per_thr);

  // Allocate single heap buffer: [book | warp0 slice | warp1 slice | ...]
  // Each warp slice is heap_per_thr * 32 terms, interleaved by lane within
  Term *global_heap, *global_stack;
  Term *outputs;
  u64  *itrs_out;
  u32  *heap_used_out;
  Stats *stats_out;

  CUDA_CHECK(cudaMalloc(&global_heap,  heap_bytes));
  CUDA_CHECK(cudaMalloc(&global_stack, stack_bytes));
  CUDA_CHECK(cudaMallocManaged(&outputs,      num_threads * sizeof(Term)));
  CUDA_CHECK(cudaMallocManaged(&itrs_out,     num_threads * sizeof(u64)));
  CUDA_CHECK(cudaMallocManaged(&heap_used_out, num_threads * sizeof(u32)));
  CUDA_CHECK(cudaMallocManaged(&stats_out,    num_threads * sizeof(Stats)));

  // Copy book data to start of heap
  CUDA_CHECK(cudaMemcpy(global_heap, HEAP, book_size * sizeof(Term), cudaMemcpyHostToDevice));

  // Set GPU call stack size (small since SNF is now iterative)
  CUDA_CHECK(cudaDeviceSetLimit(cudaLimitStackSize, 8 * 1024));

  // Prefer L1 cache over shared memory (we don't use shared memory)
  CUDA_CHECK(cudaFuncSetCacheConfig(hvm_kernel_mt, cudaFuncCachePreferL1));

  CUDA_CHECK(cudaDeviceSynchronize());

  // Time the kernel
  cudaEvent_t start, stop;
  CUDA_CHECK(cudaEventCreate(&start));
  CUDA_CHECK(cudaEventCreate(&stop));

  CUDA_CHECK(cudaEventRecord(start));

  // Launch kernel - single block for SM saturation testing
  hvm_kernel_mt<<<1, num_threads>>>(
    global_heap,    // Single heap: [book | thread0 | thread1 | ...]
    global_stack,   // Single stack: [thread0 | thread1 | ...]
    BOOK,           // Book dict (name -> loc)
    book_size,
    main_ref,
    outputs,
    itrs_out,
    heap_used_out,
    stats_out,
    num_threads,
    heap_per_thr,
    stack_per_thr
  );

  CUDA_CHECK(cudaEventRecord(stop));
  CUDA_CHECK(cudaDeviceSynchronize());

  cudaError_t err = cudaGetLastError();
  if (err != cudaSuccess) {
    fprintf(stderr, "Kernel error: %s\n", cudaGetErrorString(err));
    return 1;
  }

  float ms = 0;
  CUDA_CHECK(cudaEventElapsedTime(&ms, start, stop));

  // Sum iterations from all threads
  u64 total_itrs = 0;
  for (int i = 0; i < num_threads; i++) {
    total_itrs += itrs_out[i];
  }

  // Copy thread 0's heap back for printing (just enough to print the result)
  // For the interleaved layout, we need to de-interleave thread 0's data
  // For simplicity, just print the raw result term for now
  printf("Output: ");
  // Note: Cannot print full term tree since heap is in device memory with interleaved layout
  // Just show the result term value
  Term result = outputs[0];
  printf("tag=%u ext=%u val=%u", tag(result), ext(result), val(result));
  if (tag(result) == LAM) printf(" (LAM)");
  else if (tag(result) == ERA) printf(" (ERA)");
  else if (tag(result) == NAM) printf(" (NAM)");
  printf("\n");


  printf("- Itrs: %llu total (%llu per thread)\n", total_itrs, itrs_out[0]);
  printf("- Time: %.3f seconds\n", ms / 1000.0);
  if (ms > 0) {
    printf("- Perf: %.2f M interactions/s\n", total_itrs / (ms * 1000.0));
  }

  // Print memory usage stats
  printf("\nMemory allocation summary:\n");
  printf("  Total GPU memory: %.1f GB\n", total_mem / (1024.0*1024*1024));
  printf("  Heap total: %.1f MB (%llu terms × %d threads)\n",
         heap_bytes / (1024.0*1024), heap_per_thr, num_threads);
  printf("  Stack total: %.1f MB (%llu entries × %d threads)\n",
         stack_bytes / (1024.0*1024), stack_per_thr, num_threads);
  printf("  Book data: %u terms (%.1f KB)\n", book_size, book_size * 8 / 1024.0);
  printf("\nPer-thread allocation:\n");
  printf("  Heap: %llu terms (%.1f MB)\n", heap_per_thr, heap_per_thr * 8 / (1024.0*1024));
  printf("  Stack: %llu entries (%.1f MB) - split 50/50 for WNF/SNF\n",
         stack_per_thr, stack_per_thr * 8 / (1024.0*1024));
  printf("\nActual usage (thread 0):\n");
  printf("  Heap used: %u terms (%.1f KB) - %.2f%% of allocated\n",
         heap_used_out[0], heap_used_out[0] * 8 / 1024.0,
         100.0 * heap_used_out[0] / heap_per_thr);
  u64 wnf_alloc = stack_per_thr / 2;  // WNF gets first half of stack
  u64 snf_alloc = stack_per_thr / 2 / sizeof(u64);  // SNF entries are 8 bytes each (loc + depth)
  printf("  WNF stack max: %u entries (%.1f KB) - %.2f%% of allocated (%llu entries)\n",
         stats_out[0].max_wnf, stats_out[0].max_wnf * 8 / 1024.0,
         100.0 * stats_out[0].max_wnf / wnf_alloc, wnf_alloc);
  printf("  SNF stack max: %u entries (%.1f KB) - %.2f%% of allocated (%llu entries)\n",
         stats_out[0].max_snf, stats_out[0].max_snf * 8 / 1024.0,
         100.0 * stats_out[0].max_snf / snf_alloc, snf_alloc);

  // Cleanup
  CUDA_CHECK(cudaEventDestroy(start));
  CUDA_CHECK(cudaEventDestroy(stop));
  CUDA_CHECK(cudaFree(BOOK));
  free(HEAP);  // HEAP was allocated with malloc, not cudaMallocManaged
  CUDA_CHECK(cudaFree(global_heap));
  CUDA_CHECK(cudaFree(global_stack));
  CUDA_CHECK(cudaFree(outputs));
  CUDA_CHECK(cudaFree(itrs_out));
  CUDA_CHECK(cudaFree(heap_used_out));
  CUDA_CHECK(cudaFree(stats_out));

  return 0;
}
