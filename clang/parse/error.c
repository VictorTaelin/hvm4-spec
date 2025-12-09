// Parse Error Types
// =================

typedef enum {
  ERR_EXPECTED,
  ERR_UNDEFINED_VAR,
  ERR_DUP_REQUIRES_SUBSCRIPT,
  ERR_DYN_DUP_REQUIRES_SUBSCRIPT,
  ERR_AFFINE,
  ERR_AFFINE_DUP,
  ERR_AFFINE_SIDE,
  ERR_TOO_MANY_ARGS,
  ERR_DEF_OR_INCLUDE,
} ParseError;

typedef struct {
  ParseError err;
  union {
    struct { const char *expected; char detected; } expected;
    struct { u32 nam; } var;
    struct { u32 nam; u32 uses; const char *hint; } affine;
    struct { u32 nam; u32 uses; } affine_dup;
    struct { u32 nam; u32 uses; int side; } affine_side;
  };
} ParseErrorCtx;

// Helper macros for constructing error contexts
#define PERR_EXPECTED(exp, det) \
  (ParseErrorCtx){.err = ERR_EXPECTED, .expected = {.expected = (exp), .detected = (det)}}

#define PERR_UNDEFINED_VAR(n) \
  (ParseErrorCtx){.err = ERR_UNDEFINED_VAR, .var = {.nam = (n)}}

#define PERR_DUP_REQUIRES_SUBSCRIPT(n) \
  (ParseErrorCtx){.err = ERR_DUP_REQUIRES_SUBSCRIPT, .var = {.nam = (n)}}

#define PERR_DYN_DUP_REQUIRES_SUBSCRIPT(n) \
  (ParseErrorCtx){.err = ERR_DYN_DUP_REQUIRES_SUBSCRIPT, .var = {.nam = (n)}}

#define PERR_AFFINE(n, u, h) \
  (ParseErrorCtx){.err = ERR_AFFINE, .affine = {.nam = (n), .uses = (u), .hint = (h)}}

#define PERR_AFFINE_DUP(n, u) \
  (ParseErrorCtx){.err = ERR_AFFINE_DUP, .affine_dup = {.nam = (n), .uses = (u)}}

#define PERR_AFFINE_SIDE(n, u, sd) \
  (ParseErrorCtx){.err = ERR_AFFINE_SIDE, .affine_side = {.nam = (n), .uses = (u), .side = (sd)}}

#define PERR_TOO_MANY_ARGS() \
  (ParseErrorCtx){.err = ERR_TOO_MANY_ARGS}

#define PERR_DEF_OR_INCLUDE() \
  (ParseErrorCtx){.err = ERR_DEF_OR_INCLUDE}

fn void parse_error(PState *s, ParseErrorCtx ctx) {
  fprintf(stderr, "\033[1;31mPARSE_ERROR\033[0m (%s:%d:%d)\n", s->file, s->line, s->col);

  switch (ctx.err) {
    case ERR_EXPECTED:
      fprintf(stderr, "- expected: %s\n", ctx.expected.expected);
      if (ctx.expected.detected == 0) {
        fprintf(stderr, "- detected: EOF\n");
      } else {
        fprintf(stderr, "- detected: '%c'\n", ctx.expected.detected);
      }
      break;

    case ERR_UNDEFINED_VAR:
      fprintf(stderr, "- undefined variable '");
      print_name(stderr, ctx.var.nam);
      fprintf(stderr, "'\n");
      break;

    case ERR_DUP_REQUIRES_SUBSCRIPT:
      fprintf(stderr, "- dup variable '");
      print_name(stderr, ctx.var.nam);
      fprintf(stderr, "' requires subscript 0 or 1\n");
      break;

    case ERR_DYN_DUP_REQUIRES_SUBSCRIPT:
      fprintf(stderr, "- dynamic dup variable '");
      print_name(stderr, ctx.var.nam);
      fprintf(stderr, "' requires subscript 0 or 1\n");
      break;

    case ERR_AFFINE:
      fprintf(stderr, "- variable '");
      print_name(stderr, ctx.affine.nam);
      fprintf(stderr, "' used %d times (not cloned)\n", ctx.affine.uses);
      fprintf(stderr, "- hint: use %s to allow multiple uses\n", ctx.affine.hint);
      break;

    case ERR_AFFINE_DUP:
      fprintf(stderr, "- dup variable '");
      print_name(stderr, ctx.affine_dup.nam);
      fprintf(stderr, "' used %d times (max 2 with 0 and 1)\n", ctx.affine_dup.uses);
      break;

    case ERR_AFFINE_SIDE:
      fprintf(stderr, "- dup variable '");
      print_name(stderr, ctx.affine_side.nam);
      fprintf(stderr, "%s' used %d times (not cloned)\n",
              ctx.affine_side.side == 0 ? "0" : "1", ctx.affine_side.uses);
      fprintf(stderr, "- hint: use &");
      print_name(stderr, ctx.affine_side.nam);
      fprintf(stderr, " to allow multiple uses\n");
      break;

    case ERR_TOO_MANY_ARGS:
      fprintf(stderr, "- too many arguments\n");
      break;

    case ERR_DEF_OR_INCLUDE:
      fprintf(stderr, "- expected: definition or #include\n");
      break;
  }

  exit(1);
}
