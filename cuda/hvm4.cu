// HVM4 Runtime Implementation in CUDA
// ====================================
//
// This file implements the HVM4, an Interaction Calculus runtime, ported from
// the C implementation. It uses CUDA unified memory for seamless CPU/GPU access.
//
// Term Pointer Layout (64-bit)
// ----------------------------
// | SUB  (1 bit)   ::= marks heap slot as containing a substitution
// | TAG  (7 bits)  ::= constructor variant (APP, LAM, SUP, etc.)
// | EXT  (24 bits) ::= dup label, ctr name, or ref name
// | VAL  (32 bits) ::= heap address or unboxed value
//
// Memory Model
// ------------
// Uses CUDA Unified Memory (cudaMallocManaged) so that the same pointers
// work on both CPU and GPU without explicit transfers.
//
// Currently runs with 1 block, 1 thread for correctness testing.

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

#define BOOK_CAP  (1ULL << 20)  // ~1M entries
#define HEAP_CAP  (1ULL << 24)  // ~16M terms (~128MB)
#define STACK_CAP (1ULL << 20)  // ~1M stack frames (~8MB)

// Globals (Unified Memory - Host side)
// ====================================

static u32  *BOOK;
static Term *HEAP;

// Host-side globals for parsing
static u64 H_ALLOC = 1;

// Device-side globals
__device__ u32  D_S_POS;
__device__ u64  D_ALLOC;
__device__ u64  D_ITRS;
__device__ Term *D_STACK;
__device__ Term *D_HEAP;
__device__ u32  *D_BOOK;

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

// Device Allocation
// =================

dfn u64 heap_alloc(u64 size) {
  u64 at = D_ALLOC;
  D_ALLOC += size;
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

// Device Term Constructors
// ========================

dfn Term NewAt(u32 loc, u8 tag, u32 ext, u32 ari, Term *args) {
  for (u32 i = 0; i < ari; i++) {
    D_HEAP[loc + i] = args[i];
  }
  return new_term(0, tag, ext, loc);
}

dfn Term New(u8 tag, u32 ext, u32 ari, Term *args) {
  return NewAt(heap_alloc(ari), tag, ext, ari, args);
}

dfn Term Var(u32 loc) {
  return new_term(0, VAR, 0, loc);
}

dfn Term Ref(u32 nam) {
  return new_term(0, REF, nam, 0);
}

dfn Term Nam(u32 nam) {
  return new_term(0, NAM, 0, nam);
}

dfn Term Era(void) {
  return new_term(0, ERA, 0, 0);
}

dfn Term Co0(u32 lab, u32 loc) {
  return new_term(0, CO0, lab, loc);
}

dfn Term Co1(u32 lab, u32 loc) {
  return new_term(0, CO1, lab, loc);
}

dfn Term LamAt(u32 loc, Term bod) {
  Term args[1] = {bod};
  return NewAt(loc, LAM, 0, 1, args);
}

dfn Term Lam(Term bod) {
  return LamAt(heap_alloc(1), bod);
}

dfn Term AppAt(u32 loc, Term fun, Term arg) {
  Term args[2] = {fun, arg};
  return NewAt(loc, APP, 0, 2, args);
}

dfn Term App(Term fun, Term arg) {
  return AppAt(heap_alloc(2), fun, arg);
}

dfn Term SupAt(u32 loc, u32 lab, Term tm0, Term tm1) {
  Term args[2] = {tm0, tm1};
  return NewAt(loc, SUP, lab, 2, args);
}

dfn Term Sup(u32 lab, Term tm0, Term tm1) {
  return SupAt(heap_alloc(2), lab, tm0, tm1);
}

dfn Term DryAt(u32 loc, Term tm0, Term tm1) {
  Term args[2] = {tm0, tm1};
  return NewAt(loc, DRY, 0, 2, args);
}

dfn Term Dry(Term tm0, Term tm1) {
  return DryAt(heap_alloc(2), tm0, tm1);
}

dfn Term DupAt(u32 loc, u32 lab, Term v, Term bod) {
  Term args[2] = {v, bod};
  return NewAt(loc, DUP, lab, 2, args);
}

dfn Term Dup(u32 lab, Term v, Term bod) {
  return DupAt(heap_alloc(2), lab, v, bod);
}

dfn Term MatAt(u32 loc, u32 nam, Term v, Term nxt) {
  Term args[2] = {v, nxt};
  return NewAt(loc, MAT, nam, 2, args);
}

dfn Term Mat(u32 nam, Term v, Term nxt) {
  return MatAt(heap_alloc(2), nam, v, nxt);
}

dfn Term CtrAt(u32 loc, u32 nam, u32 ari, Term *args) {
  return NewAt(loc, CTR + ari, nam, ari, args);
}

dfn Term Ctr(u32 nam, u32 ari, Term *args) {
  return CtrAt(heap_alloc(ari), nam, ari, args);
}

// Cloning (Device)
// ================

dfn Copy clone_at(u32 loc, u32 lab) {
  return (Copy){ Co0(lab, loc), Co1(lab, loc) };
}

dfn Copy clone(u32 lab, Term v) {
  u64 loc     = heap_alloc(1);
  D_HEAP[loc] = v;
  return clone_at(loc, lab);
}

dfn void clone_many(u32 lab, Term *src, u32 n, Term *dst0, Term *dst1) {
  for (u32 i = 0; i < n; i++) {
    Copy c  = clone(lab, src[i]);
    dst0[i] = c.k0;
    dst1[i] = c.k1;
  }
}

// Substitution Helpers (Device)
// =============================

dfn void subst_var(u32 loc, Term v) {
  D_HEAP[loc] = mark_sub(v);
}

dfn Term subst_cop(u8 side, u32 loc, Term r0, Term r1) {
  D_HEAP[loc] = mark_sub(side == 0 ? r1 : r0);
  return side == 0 ? r0 : r1;
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

fn void str_term_go(Term term, u32 depth);

fn void str_term_go(Term term, u32 depth) {
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
      str_term_go(HEAP[loc], depth + 1);
      break;
    }
    case APP:
    case DRY: {
      Term spine[256];
      u32  len  = 0;
      Term curr = term;
      while ((tag(curr) == APP || tag(curr) == DRY) && len < 256) {
        u32 loc = val(curr);
        spine[len++] = HEAP[loc + 1];
        curr = HEAP[loc];
      }
      if (tag(curr) == LAM) {
        str_putc('(');
        str_term_go(curr, depth);
        str_putc(')');
      } else {
        str_term_go(curr, depth);
      }
      str_putc('(');
      for (u32 i = 0; i < len; i++) {
        if (i > 0) {
          str_putc(',');
        }
        str_term_go(spine[len - 1 - i], depth);
      }
      str_putc(')');
      break;
    }
    case SUP: {
      u32 loc = val(term);
      str_putc('&');
      str_name(ext(term));
      str_putc('{');
      str_term_go(HEAP[loc + 0], depth);
      str_putc(',');
      str_term_go(HEAP[loc + 1], depth);
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
      str_term_go(HEAP[loc + 0], depth);
      str_putc(';');
      str_term_go(HEAP[loc + 1], depth + 1);
      break;
    }
    case MAT: {
      u32 loc = val(term);
      str_puts("λ{#");
      str_name(ext(term));
      str_putc(':');
      str_term_go(HEAP[loc + 0], depth);
      str_putc(';');
      str_term_go(HEAP[loc + 1], depth);
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
          str_term_go(HEAP[loc + i], depth);
        }
        str_putc('}');
      }
      break;
    }
  }
}

fn void print_term(Term term) {
  STR_MODE = STR_LOG;
  str_term_go(term, 0);
}

fn void term_buf_init(void) {
  TERM_BUF_CAP = 65536;
  TERM_BUF     = (char*)malloc(TERM_BUF_CAP);
  TERM_BUF_POS = 0;
}

fn void term_buf_free(void) {
  free(TERM_BUF);
  TERM_BUF     = NULL;
  TERM_BUF_POS = 0;
  TERM_BUF_CAP = 0;
}

fn char *term_to_str(Term term) {
  STR_MODE     = STR_BUF;
  TERM_BUF_POS = 0;
  str_term_go(term, 0);
  return TERM_BUF;
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

// Beta Interactions (Device)
// ==========================

dfn Term app_era(void) {
  D_ITRS++;
  return Era();
}

dfn Term app_stuck(Term fun, Term arg) {
  D_ITRS++;
  return Dry(fun, arg);
}

dfn Term app_lam(Term lam, Term arg) {
  D_ITRS++;
  u32  loc  = val(lam);
  Term body = D_HEAP[loc];
  subst_var(loc, arg);
  return body;
}

dfn Term app_sup(Term app, Term sup) {
  D_ITRS++;
  u32  app_loc = val(app);
  u32  sup_loc = val(sup);
  u32  lab     = ext(sup);
  Term arg     = D_HEAP[app_loc + 1];
  Term tm1     = D_HEAP[sup_loc + 1];
  u64  loc     = heap_alloc(3);
  D_HEAP[loc + 2] = arg;
  Copy D = clone_at(loc + 2, lab);
  D_HEAP[sup_loc + 1] = D.k0;
  Term ap0 = new_term(0, APP, 0, sup_loc);
  Term ap1 = AppAt(loc, tm1, D.k1);
  return SupAt(app_loc, lab, ap0, ap1);
}

// Match Interactions (Device)
// ===========================

dfn Term app_mat_sup(Term mat, Term sup) {
  D_ITRS++;
  u32  lab = ext(sup);
  Copy M   = clone(lab, mat);
  u32  loc = val(sup);
  Term a   = D_HEAP[loc + 0];
  Term b   = D_HEAP[loc + 1];
  return Sup(lab, App(M.k0, a), App(M.k1, b));
}

dfn Term app_mat_ctr(Term mat, Term ctr) {
  D_ITRS++;
  u32 ari = tag(ctr) - CTR;
  if (ext(mat) == ext(ctr)) {
    Term res = D_HEAP[val(mat)];
    for (u32 i = 0; i < ari; i++) {
      res = App(res, D_HEAP[val(ctr) + i]);
    }
    return res;
  } else {
    return App(D_HEAP[val(mat) + 1], ctr);
  }
}

// Dup Interactions (Device)
// =========================

dfn Term dup_lam(u32 lab, u32 loc, u8 side, Term lam) {
  D_ITRS++;
  u32  lam_loc = val(lam);
  Term bod     = D_HEAP[lam_loc];
  u64  a       = heap_alloc(5);
  D_HEAP[a + 4]  = bod;
  Copy B  = clone_at(a + 4, lab);
  Term su = SupAt(a + 2, lab, Var(a), Var(a + 1));
  Term l0 = LamAt(a + 0, B.k0);
  Term l1 = LamAt(a + 1, B.k1);
  subst_var(lam_loc, su);
  return subst_cop(side, loc, l0, l1);
}

dfn Term dup_sup(u32 lab, u32 loc, u8 side, Term sup) {
  D_ITRS++;
  u32 sup_loc = val(sup);
  u32 sup_lab = ext(sup);
  if (lab == sup_lab) {
    Term tm0 = D_HEAP[sup_loc + 0];
    Term tm1 = D_HEAP[sup_loc + 1];
    return subst_cop(side, loc, tm0, tm1);
  } else {
    Copy A  = clone_at(sup_loc + 0, lab);
    Copy B  = clone_at(sup_loc + 1, lab);
    u64  a  = heap_alloc(4);
    Term s0 = SupAt(a + 0, sup_lab, A.k0, B.k0);
    Term s1 = SupAt(a + 2, sup_lab, A.k1, B.k1);
    return subst_cop(side, loc, s0, s1);
  }
}

dfn Term dup_node(u32 lab, u32 loc, u8 side, Term term) {
  D_ITRS++;
  u32 ari = arity_of(term);
  if (ari == 0) {
    subst_var(loc, term);
    return term;
  }
  u32  t_loc = val(term);
  u32  t_ext = ext(term);
  u8   t_tag = tag(term);
  Term args0[16], args1[16];
  for (u32 i = 0; i < ari; i++) {
    Copy A   = clone(lab, D_HEAP[t_loc + i]);
    args0[i] = A.k0;
    args1[i] = A.k1;
  }
  Term r0 = New(t_tag, t_ext, ari, args0);
  Term r1 = New(t_tag, t_ext, ari, args1);
  return subst_cop(side, loc, r0, r1);
}

// Alloc Helpers (Device)
// ======================

dfn u32 bind_at(u32 ls, u32 idx) {
  for (u32 i = 0; i < idx && ls != 0; i++) {
    ls = (u32)(D_HEAP[ls] & 0xFFFFFFFF);
  }
  return (ls != 0) ? (u32)(D_HEAP[ls] >> 32) : 0;
}

dfn u32 make_bind(u32 tail, u32 loc) {
  u64 entry = heap_alloc(1);
  D_HEAP[entry] = ((u64)loc << 32) | tail;
  return (u32)entry;
}

dfn Term make_alo(u32 ls_loc, u32 tm_loc) {
  u64 loc     = heap_alloc(1);
  D_HEAP[loc] = ((u64)ls_loc << 32) | tm_loc;
  return new_term(0, ALO, 0, loc);
}

// Alloc Interactions (Device)
// ===========================

dfn Term alo_var(u32 ls_loc, u32 idx) {
  u32 bind = bind_at(ls_loc, idx);
  return bind ? Var(bind) : new_term(0, VAR, 0, idx);
}

dfn Term alo_cop(u32 ls_loc, u32 idx, u32 lab, u8 side) {
  u32 bind = bind_at(ls_loc, idx);
  u8  tg   = side == 0 ? CO0 : CO1;
  return bind ? new_term(0, tg, lab, bind) : new_term(0, tg, lab, idx);
}

dfn Term alo_lam(u32 ls_loc, u32 book_body_loc) {
  u64 lam_body     = heap_alloc(1);
  u32 new_bind     = make_bind(ls_loc, (u32)lam_body);
  D_HEAP[lam_body] = make_alo(new_bind, book_body_loc);
  return new_term(0, LAM, 0, lam_body);
}

dfn Term alo_dup(u32 ls_loc, u32 book_loc, u32 lab) {
  u64 dup_val     = heap_alloc(1);
  u32 new_bind    = make_bind(ls_loc, (u32)dup_val);
  D_HEAP[dup_val] = make_alo(ls_loc, book_loc + 0);
  return Dup(lab, make_alo(ls_loc, book_loc + 0), make_alo(new_bind, book_loc + 1));
}

dfn Term alo_node(u32 ls_loc, u32 loc, u8 tg, u32 ex, u32 ari) {
  Term args[16];
  for (u32 i = 0; i < ari; i++) {
    args[i] = make_alo(ls_loc, loc + i);
  }
  return New(tg, ex, ari, args);
}

// WNF (Device)
// ============

__device__ Term wnf(Term term) {
  D_S_POS = 0;
  Term next = term;
  Term whnf;

  enter: {
    switch (tag(next)) {
      case VAR: {
        u32 loc = val(next);
        if (sub_of(D_HEAP[loc])) {
          next = clear_sub(D_HEAP[loc]);
          goto enter;
        }
        whnf = next;
        goto apply;
      }

      case CO0:
      case CO1: {
        u32 loc = val(next);
        if (sub_of(D_HEAP[loc])) {
          next = clear_sub(D_HEAP[loc]);
          goto enter;
        }
        Term dup_val = D_HEAP[loc];
        D_STACK[D_S_POS++] = next;
        next = dup_val;
        goto enter;
      }

      case APP: {
        u32  loc = val(next);
        Term fun = D_HEAP[loc];
        D_STACK[D_S_POS++] = next;
        next = fun;
        goto enter;
      }

      case DUP: {
        u32  loc  = val(next);
        Term body = D_HEAP[loc + 1];
        next = body;
        goto enter;
      }

      case REF: {
        u32 nam = ext(next);
        if (D_BOOK[nam] != 0) {
          next = make_alo(0, D_BOOK[nam]);
          goto enter;
        }
        whnf = next;
        goto apply;
      }

      case ALO: {
        u32  alo_loc = val(next);
        u64  pair    = D_HEAP[alo_loc];
        u32  tm_loc  = (u32)(pair & 0xFFFFFFFF);
        u32  ls_loc  = (u32)(pair >> 32);
        Term bk      = D_HEAP[tm_loc];

        switch (tag(bk)) {
          case VAR: {
            next = alo_var(ls_loc, val(bk));
            goto enter;
          }
          case CO0:
          case CO1: {
            next = alo_cop(ls_loc, val(bk), ext(bk), tag(bk) == CO0 ? 0 : 1);
            goto enter;
          }
          case LAM: {
            next = alo_lam(ls_loc, val(bk));
            goto enter;
          }
          case APP:
          case SUP:
          case MAT:
          case DRY: {
            next = alo_node(ls_loc, val(bk), tag(bk), ext(bk), arity_of(bk));
            goto enter;
          }
          case DUP: {
            next = alo_dup(ls_loc, val(bk), ext(bk));
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
              next = alo_node(ls_loc, val(bk), tag(bk), ext(bk), arity_of(bk));
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
    while (D_S_POS > 0) {
      Term frame = D_STACK[--D_S_POS];

      switch (tag(frame)) {
        case APP: {
          u32  loc = val(frame);
          Term arg = D_HEAP[loc + 1];

          switch (tag(whnf)) {
            case ERA: {
              whnf = app_era();
              continue;
            }
            case NAM:
            case DRY: {
              whnf = app_stuck(whnf, arg);
              continue;
            }
            case LAM: {
              whnf = app_lam(whnf, arg);
              next = whnf;
              goto enter;
            }
            case SUP: {
              whnf = app_sup(frame, whnf);
              next = whnf;
              goto enter;
            }
            case MAT: {
              D_STACK[D_S_POS++] = whnf;
              next = arg;
              goto enter;
            }
            default: {
              whnf = App(whnf, arg);
              continue;
            }
          }
        }

        case MAT: {
          Term mat = frame;
          switch (tag(whnf)) {
            case ERA: {
              whnf = app_era();
              continue;
            }
            case SUP: {
              whnf = app_mat_sup(mat, whnf);
              next = whnf;
              goto enter;
            }
            default: {
              if (tag(whnf) >= CTR && tag(whnf) <= CTR + CTR_MAX_ARI) {
                whnf = app_mat_ctr(mat, whnf);
                next = whnf;
                goto enter;
              }
              whnf = App(mat, whnf);
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
              whnf = dup_lam(lab, loc, side, whnf);
              next = whnf;
              goto enter;
            }
            case SUP: {
              whnf = dup_sup(lab, loc, side, whnf);
              next = whnf;
              goto enter;
            }
            case ERA:
            case NAM: {
              whnf = dup_node(lab, loc, side, whnf);
              continue;
            }
            case MAT:
            case DRY: {
              whnf = dup_node(lab, loc, side, whnf);
              next = whnf;
              goto enter;
            }
            default: {
              if (tag(whnf) >= CTR && tag(whnf) <= CTR + CTR_MAX_ARI) {
                whnf = dup_node(lab, loc, side, whnf);
                next = whnf;
                goto enter;
              }
              u64 new_loc     = heap_alloc(1);
              D_HEAP[new_loc] = whnf;
              subst_var(loc, new_term(0, side == 0 ? CO1 : CO0, lab, new_loc));
              whnf            = new_term(0, side == 0 ? CO0 : CO1, lab, new_loc);
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

// SNF (Device)
// ============

__device__ Term snf(Term term, u32 depth) {
  term = wnf(term);
  u32 ari = arity_of(term);
  if (ari == 0) {
    return term;
  }
  u64 loc = val(term);
  if (tag(term) == LAM) {
    Term body = D_HEAP[loc];
    subst_var(loc, Nam(depth + 1));
    D_HEAP[loc] = snf(body, depth + 1);
  } else {
    for (u32 i = 0; i < ari; i++) {
      D_HEAP[loc + i] = snf(D_HEAP[loc + i], depth);
    }
  }
  return term;
}

// Kernel
// ======

__global__ void hvm_kernel(Term *heap, u32 *book, Term input, Term *output, u64 init_alloc, Term *stack) {
  // Initialize device globals
  D_HEAP  = heap;
  D_BOOK  = book;
  D_ALLOC = init_alloc;
  D_ITRS  = 0;
  D_S_POS = 0;
  D_STACK = stack;

  // Run SNF
  Term result = snf(input, 0);
  *output = result;
}

// Get iterations from device
__global__ void get_itrs(u64 *out) {
  *out = D_ITRS;
}

// Main
// ====

int main(int argc, char **argv) {
  // Simple test: parse and evaluate @main

  // Allocate unified memory
  CUDA_CHECK(cudaMallocManaged(&BOOK, BOOK_CAP * sizeof(u32)));
  CUDA_CHECK(cudaMallocManaged(&HEAP, HEAP_CAP * sizeof(Term)));

  Term *d_stack;
  CUDA_CHECK(cudaMallocManaged(&d_stack, STACK_CAP * sizeof(Term)));

  Term *d_output;
  CUDA_CHECK(cudaMallocManaged(&d_output, sizeof(Term)));

  u64 *d_itrs;
  CUDA_CHECK(cudaMallocManaged(&d_itrs, sizeof(u64)));

  // Initialize memory
  memset(BOOK, 0, BOOK_CAP * sizeof(u32));
  memset(HEAP, 0, HEAP_CAP * sizeof(Term));

  // Parse input - default test (identity function)
  const char *test_src = "@main = λx.x";
  char *src = NULL;

  if (argc > 1) {
    // Read file
    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
      fprintf(stderr, "Error: could not open '%s'\n", argv[1]);
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
    src = strdup(test_src);
  }

  PState s = {
    .file = (char*)(argc > 1 ? argv[1] : "<inline>"),
    .src  = src,
    .pos  = 0,
    .len  = (u32)strlen(src),
    .line = 1,
    .col  = 1
  };
  parse_def(&s);
  free(src);

  // Get @main name hash
  u32 main_name = 0;
  const char *p = "main";
  while (*p) {
    main_name = ((main_name << 6) + char_to_b64(*p)) & EXT_MASK;
    p++;
  }

  // Check @main exists
  if (BOOK[main_name] == 0) {
    fprintf(stderr, "Error: @main not defined\n");
    return 1;
  }

  // Create @main reference
  Term main_ref = new_term(0, REF, main_name, 0);

  printf("Input: ");
  print_term(HEAP[BOOK[main_name]]);
  printf("\n");

  // Increase GPU stack size for deep recursion in SNF
  CUDA_CHECK(cudaDeviceSetLimit(cudaLimitStackSize, 32 * 1024));

  // Sync before kernel launch
  CUDA_CHECK(cudaDeviceSynchronize());

  // Time the kernel
  cudaEvent_t start, stop;
  CUDA_CHECK(cudaEventCreate(&start));
  CUDA_CHECK(cudaEventCreate(&stop));

  CUDA_CHECK(cudaEventRecord(start));

  // Launch kernel with 1 block, 1 thread
  hvm_kernel<<<1, 1>>>(HEAP, BOOK, main_ref, d_output, H_ALLOC, d_stack);

  CUDA_CHECK(cudaEventRecord(stop));
  CUDA_CHECK(cudaDeviceSynchronize());

  // Check for errors
  cudaError_t err = cudaGetLastError();
  if (err != cudaSuccess) {
    fprintf(stderr, "Kernel error: %s\n", cudaGetErrorString(err));
    return 1;
  }

  // Get timing
  float ms = 0;
  CUDA_CHECK(cudaEventElapsedTime(&ms, start, stop));

  // Get iteration count
  get_itrs<<<1, 1>>>(d_itrs);
  CUDA_CHECK(cudaDeviceSynchronize());

  // Print result
  printf("Output: ");
  print_term(*d_output);
  printf("\n");

  // Print stats
  printf("- Itrs: %llu interactions\n", *d_itrs);
  printf("- Time: %.3f seconds\n", ms / 1000.0);
  if (ms > 0) {
    printf("- Perf: %.2f M interactions/s\n", (*d_itrs) / (ms * 1000.0));
  }

  // Cleanup
  CUDA_CHECK(cudaEventDestroy(start));
  CUDA_CHECK(cudaEventDestroy(stop));
  CUDA_CHECK(cudaFree(BOOK));
  CUDA_CHECK(cudaFree(HEAP));
  CUDA_CHECK(cudaFree(d_stack));
  CUDA_CHECK(cudaFree(d_output));
  CUDA_CHECK(cudaFree(d_itrs));

  return 0;
}
