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

// Function Definition Macro
// =========================

#define fn static inline

// Tags
// ====

#define REF  0
#define ALO  1
#define ERA  2
#define CO0  3
#define CO1  4
#define VAR  5
#define LAM  6
#define APP  7
#define SUP  8
#define DUP  9
#define MAT 10
#define C00 11
#define C01 12
#define C02 13
#define C03 14
#define C04 15
#define C05 16
#define C06 17
#define C07 18
#define C08 19
#define C09 20
#define C10 21
#define C11 22
#define C12 23
#define C13 24
#define C14 25
#define C15 26
#define C16 27
#define NUM 28

// Special constructor names for stuck terms
// =========================================

#define _VAR_ 198380  // name_to_int("VAR")
#define _APP_ 113322  // name_to_int("APP")

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

#define HEAP_CAP (1ULL << 32)
#define BOOK_CAP (1ULL << 24)
#define WNF_CAP  (1ULL << 32)

// Heap Globals
// ============

static Term *HEAP;
static u64   ALLOC = 1;

// Book Globals
// ============

static u32 *BOOK;

// WNF Globals
// ===========

static Term *STACK;
static u32   S_POS = 1;
static u64   ITRS  = 0;
static int   DEBUG = 0;

// Nick Alphabet
// =============

static const char *nick_alphabet = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$";

// Stringifier Modes
// =================

#define STR_LOG 0
#define STR_BUF 1

// Stringifier Globals
// ===================

static u8    STR_MODE     = STR_LOG;
static char *TERM_BUF     = NULL;
static u32   TERM_BUF_POS = 0;
static u32   TERM_BUF_CAP = 0;

// Parser Types
// ============

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

// Parser Globals
// ==============

static char  *PARSE_SEEN_FILES[1024];
static u32    PARSE_SEEN_FILES_LEN = 0;
static PBind  PARSE_BINDS[16384];
static u32    PARSE_BINDS_LEN = 0;

// Term
// ====
#include "term/new.c"
#include "term/sub.c"
#include "term/tag.c"
#include "term/ext.c"
#include "term/val.c"
#include "term/arity.c"
#include "term/mark.c"
#include "term/unmark.c"

// Heap
// ====

#include "heap/alloc.c"

// Term Constructors
// =================

#include "term/new/_.c"
#include "term/new/var.c"
#include "term/new/ref.c"
#include "term/new/era.c"
#include "term/new/co0.c"
#include "term/new/co1.c"
#include "term/new/lam.c"
#include "term/new/app.c"
#include "term/new/sup.c"
#include "term/new/dup.c"
#include "term/new/mat.c"
#include "term/new/ctr.c"
#include "term/new/num.c"
#include "term/clone_at.c"
#include "term/clone.c"
#include "term/clone_many.c"

// Heap Substitution
// =================

#include "heap/subst_var.c"
#include "heap/subst_cop.c"

// Nick
// ====

#include "nick/letter_to_b64.c"
#include "nick/is_init.c"
#include "nick/is_char.c"

// System
// ======

#include "sys/error.c"
#include "sys/path_join.c"
#include "sys/file_read.c"

// Print
// =====

#include "print/str_putc.c"
#include "print/str_puts.c"
#include "print/str_name.c"
#include "print/str_uint.c"
#include "print/is_app.c"
#include "print/str_term_go.c"
#include "print/term.c"
#include "print/term_buf_init.c"
#include "print/term_buf_free.c"
#include "print/term_to_str.c"

// Parse
// =====

#include "parse/error.c"
#include "parse/at_end.c"
#include "parse/peek_at.c"
#include "parse/peek.c"
#include "parse/advance.c"
#include "parse/starts_with.c"
#include "parse/match.c"
#include "parse/is_space.c"
#include "parse/skip_comment.c"
#include "parse/skip.c"
#include "parse/consume.c"
#include "parse/bind_push.c"
#include "parse/bind_pop.c"
#include "parse/bind_lookup.c"
#include "parse/name.c"
#include "parse/term/lam.c"
#include "parse/term/dup.c"
#include "parse/term/sup.c"
#include "parse/term/ctr.c"
#include "parse/term/ref.c"
#include "parse/term/par.c"
#include "parse/term/num.c"
#include "parse/term/var.c"
#include "parse/term/app.c"
#include "parse/term/_.c"
#include "parse/include.c"
#include "parse/def.c"

// WNF
// ===

#include "wnf/app_era.c"
#include "wnf/app_ctr.c"
#include "wnf/app_lam.c"
#include "wnf/app_sup.c"
#include "wnf/app_mat_sup.c"
#include "wnf/app_mat_ctr.c"
#include "wnf/dup_lam.c"
#include "wnf/dup_sup.c"
#include "wnf/dup_node.c"
#include "wnf/alo_var.c"
#include "wnf/alo_cop.c"
#include "wnf/alo_lam.c"
#include "wnf/alo_dup.c"
#include "wnf/alo_node.c"
#include "wnf/_.c"

// SNF
// ===

#include "snf/_.c"

// Collapse
// ========

#include "collapse/inject.c"
#include "collapse/_.c"
