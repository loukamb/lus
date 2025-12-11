/*
** lcode2.c - Lus AST Code Generator Implementation
** Self-contained code generator that produces bytecode from AST.
** Does NOT depend on lcode.c internals.
*/

#define lcode2_c
#define LUA_CORE

#include "lprefix.h"

#include <stdio.h>
#include <string.h>

#include "lua.h"

#include "lcode2.h"
#include "ldo.h"
#include "lfunc.h"
#include "lgc.h"
#include "lmem.h"
#include "lobject.h"
#include "lopcodes.h"
#include "lstring.h"
#include "ltable.h"

/*
** {==================================================================
** Internal Code Generator State
** ===================================================================
*/

/* Maximum number of local variables in a function */
#define MAXVARS 200

/* Maximum number of constants in a function */
#define MAXK 65535

/* No jump target */
#define NO_JUMP (-1)

/* Local variable info */
typedef struct LocalVar {
  TString *name;
  int reg;     /* register holding this variable */
  int startpc; /* first PC where active */
  int endpc;   /* last PC where active (or -1 if still active) */
} LocalVar;

/* Upvalue info */
typedef struct UpvalInfo {
  TString *name;
  lu_byte instack; /* in local stack of enclosing function */
  lu_byte idx;     /* index in enclosing function */
  lu_byte kind;    /* kind of variable (VDKREG for _ENV) */
} UpvalInfo;

/* Code generator function state (self-contained, no FuncState dependency) */
typedef struct GenState {
  lua_State *L;
  struct GenState *prev;    /* enclosing function */
  Proto *f;                 /* function being generated */
  lusM_Arena *arena;        /* AST arena */
  LocalVar locals[MAXVARS]; /* local variable info */
  int nlocals;              /* number of local variables */
  int nactvar;              /* number of currently active locals */
  UpvalInfo upvalues[256];  /* upvalue info */
  int nups;                 /* number of upvalues */
  int nk;                   /* number of constants (actual) */
  int freereg;              /* first free register */
  int pc;                   /* next instruction index */
  int lasttarget;           /* last jump target */
  int linenumber;           /* current line for debug info */
} GenState;

/* Expression result kind */
typedef enum {
  EK_VOID,   /* no value */
  EK_NIL,    /* nil constant */
  EK_TRUE,   /* true constant */
  EK_FALSE,  /* false constant */
  EK_INT,    /* integer constant */
  EK_FLT,    /* float constant */
  EK_STR,    /* string constant */
  EK_REG,    /* value in register */
  EK_RELOC,  /* instruction that needs patching with dest register */
  EK_K,      /* constant in K */
  EK_UPVAL,  /* upvalue */
  EK_GLOBAL, /* global via _ENV */
  EK_INDEX,  /* indexed value */
  EK_CALL,   /* function call result */
  EK_VARARG  /* vararg expression */
} ExpKind;

/* Expression result descriptor */
typedef struct ExpResult {
  ExpKind kind;
  union {
    lua_Integer ival;
    lua_Number fval;
    TString *sval;
    int reg; /* register number */
    int pc;  /* instruction pc */
    int k;   /* constant index */
    struct {
      int t;       /* table register or upvalue */
      int key;     /* key register, constant, or string index */
      int isupval; /* 1 if table is upvalue */
    } ind;
  } u;
  int t; /* jump true list */
  int f; /* jump false list */
} ExpResult;

/* }================================================================== */

/*
** {==================================================================
** Proto Builder Helpers
** ===================================================================
*/

/* Ensure code array has space */
static void growcode(GenState *gs) {
  Proto *f = gs->f;
  if (gs->pc >= f->sizecode) {
    int newsize = f->sizecode * 2;
    if (newsize < 64)
      newsize = 64;
    luaM_reallocvector(gs->L, f->code, f->sizecode, newsize, Instruction);
    f->sizecode = newsize;
  }
}

/* Emit an instruction */
static int emit(GenState *gs, Instruction i) {
  growcode(gs);
  gs->f->code[gs->pc] = i;
  return gs->pc++;
}

/* Instruction creation macros - use official CREATE_* macros from lopcodes.h */
#define EM_ABC(o, a, b, c) CREATE_ABCk(o, a, b, c, 0)

#define EM_ABx(o, a, bx) CREATE_ABx(o, a, bx)

/* For sJ, we need to add the offset to convert signed to unsigned
 * representation */
#define EM_sJ(o, offset)                                                       \
  ((cast_Inst(o) << POS_OP) | (cast_Inst((offset) + OFFSET_sJ) << POS_sJ))

/* Emit ABC instruction */
static int emitABC(GenState *gs, OpCode o, int a, int b, int c) {
  Instruction i = EM_ABC(o, a, b, c);
  fprintf(stderr,
          "DEBUG emitABC: op=%d a=%d b=%d c=%d => inst=0x%08x decoded_op=%d\n",
          (int)o, a, b, c, (unsigned)i, (int)GET_OPCODE(i));
  return emit(gs, i);
}

/* Emit ABx instruction */
static int emitABx(GenState *gs, OpCode o, int a, int bx) {
  return emit(gs, EM_ABx(o, a, bx));
}

/* Emit AsBx instruction (signed Bx) */
static int emitAsBx(GenState *gs, OpCode o, int a, int sbx) {
  /* Convert signed sbx to unsigned representation */
  Instruction i = EM_ABx(o, a, sbx + OFFSET_sBx);
  fprintf(stderr,
          "DEBUG emitAsBx: op=%d a=%d sbx=%d => inst=0x%08x decoded_op=%d\n",
          (int)o, a, sbx, (unsigned)i, (int)GET_OPCODE(i));
  return emit(gs, i);
}

/* Emit sJ instruction (signed jump) */
static int emitsJ(GenState *gs, OpCode o, int offset) {
  return emit(gs, EM_sJ(o, offset));
}

/* Add constant to function, return index */
static int addK(GenState *gs, TValue *v) {
  Proto *f = gs->f;
  lua_State *L = gs->L;

  /* Check if constant already exists */
  for (int i = 0; i < gs->nk; i++) {
    TValue *k = &f->k[i];
    if (ttypetag(v) == ttypetag(k)) {
      if (ttisinteger(v) && ivalue(v) == ivalue(k))
        return i;
      if (ttisfloat(v) && fltvalue(v) == fltvalue(k))
        return i;
      if (ttisstring(v) && eqshrstr(tsvalue(v), tsvalue(k)))
        return i;
    }
  }

  /* Add new constant */
  int idx = gs->nk;
  fprintf(stderr, "DEBUG addK: idx=%d nk=%d sizek=%d k=%p\n", idx, gs->nk,
          f->sizek, (void *)f->k);
  luaM_growvector(L, f->k, idx, f->sizek, TValue, MAXK, "constants");
  gs->nk++;
  fprintf(stderr, "DEBUG addK: after grow nk=%d sizek=%d k=%p\n", gs->nk,
          f->sizek, (void *)f->k);
  setobj(L, &f->k[idx], v);
  return idx;
}

/* Add integer constant */
static int addKint(GenState *gs, lua_Integer n) {
  TValue v;
  setivalue(&v, n);
  return addK(gs, &v);
}

/* Add float constant */
static int addKflt(GenState *gs, lua_Number n) {
  TValue v;
  setfltvalue(&v, n);
  return addK(gs, &v);
}

/* Add string constant */
static int addKstr(GenState *gs, TString *s) {
  TValue v;
  setsvalue(gs->L, &v, s);
  return addK(gs, &v);
}

/* Reserve registers */
static void reserveregs(GenState *gs, int n) {
  gs->freereg += n;
  if (gs->freereg > gs->f->maxstacksize)
    gs->f->maxstacksize = cast_byte(gs->freereg);
}

/* Get a free register */
static int getreg(GenState *gs) {
  int reg = gs->freereg;
  reserveregs(gs, 1);
  return reg;
}

/* }================================================================== */

/*
** {==================================================================
** Variable Resolution
** ===================================================================
*/

/* Search for local variable by name */
static int searchlocal(GenState *gs, TString *name) {
  for (int i = gs->nactvar - 1; i >= 0; i--) {
    if (luaS_eqstr(gs->locals[i].name, name))
      return gs->locals[i].reg;
  }
  return -1; /* not found */
}

/* Search for upvalue by name */
static int searchupval(GenState *gs, TString *name) {
  for (int i = 0; i < gs->nups; i++) {
    if (luaS_eqstr(gs->upvalues[i].name, name))
      return i;
  }
  return -1; /* not found */
}

/* Add new upvalue */
static int newupval(GenState *gs, TString *name, int instack, int idx) {
  int n = gs->nups++;
  gs->upvalues[n].name = name;
  gs->upvalues[n].instack = cast_byte(instack);
  gs->upvalues[n].idx = cast_byte(idx);
  return n;
}

/* Resolve a variable name */
static void resolvevar(GenState *gs, TString *name, ExpResult *e) {
  int reg = searchlocal(gs, name);
  if (reg >= 0) {
    e->kind = EK_REG;
    e->u.reg = reg;
    return;
  }

  int up = searchupval(gs, name);
  if (up >= 0) {
    e->kind = EK_UPVAL;
    e->u.reg = up;
    return;
  }

  /* Check enclosing functions for upvalue */
  if (gs->prev != NULL) {
    int local = searchlocal(gs->prev, name);
    if (local >= 0) {
      int n = newupval(gs, name, 1, local);
      e->kind = EK_UPVAL;
      e->u.reg = n;
      return;
    }
    /* Could search deeper, simplified for now */
  }

  /* Not found - treat as global via _ENV */
  e->kind = EK_GLOBAL;
  e->u.ind.t = 0; /* _ENV is upvalue 0 */
  e->u.ind.key = addKstr(gs, name);
  e->u.ind.isupval = 1;
}

/* Create new local variable */
static int newlocal(GenState *gs, TString *name) {
  (void)gs;
  (void)name; /* for future use */
  int reg = getreg(gs);
  int idx = gs->nlocals++;
  gs->locals[idx].name = name;
  gs->locals[idx].reg = reg;
  gs->locals[idx].startpc = gs->pc;
  gs->locals[idx].endpc = -1;
  gs->nactvar++;
  return reg;
}

/* }================================================================== */

/*
** {==================================================================
** Expression Code Generation
** ===================================================================
*/

/* Forward declarations */
static void genexpr(GenState *gs, lus_Node *node, ExpResult *e);
static void genstmt(GenState *gs, lus_Node *node);
static void genblock(GenState *gs, lus_BlockNode *block);

/* Initialize expression result */
static void initexp(ExpResult *e, ExpKind kind) {
  e->kind = kind;
  e->t = NO_JUMP;
  e->f = NO_JUMP;
}

/* Discharge expression to a register */
static int exp2reg(GenState *gs, ExpResult *e) {
  switch (e->kind) {
  case EK_VOID: {
    /* Uninitialized or void expression - treat as nil */
    int reg = getreg(gs);
    emitABC(gs, OP_LOADNIL, reg, 0, 0);
    e->kind = EK_REG;
    e->u.reg = reg;
    return reg;
  }
  case EK_NIL: {
    int reg = getreg(gs);
    emitABC(gs, OP_LOADNIL, reg, 0, 0);
    e->kind = EK_REG;
    e->u.reg = reg;
    return reg;
  }
  case EK_TRUE: {
    int reg = getreg(gs);
    emitABC(gs, OP_LOADTRUE, reg, 0, 0);
    e->kind = EK_REG;
    e->u.reg = reg;
    return reg;
  }
  case EK_FALSE: {
    int reg = getreg(gs);
    emitABC(gs, OP_LOADFALSE, reg, 0, 0);
    e->kind = EK_REG;
    e->u.reg = reg;
    return reg;
  }
  case EK_INT: {
    int reg = getreg(gs);
    lua_Integer n = e->u.ival;
    if (n >= -(1 << 15) && n < (1 << 15)) {
      emitAsBx(gs, OP_LOADI, reg, cast_int(n));
    } else {
      int k = addKint(gs, n);
      emitABx(gs, OP_LOADK, reg, k);
    }
    e->kind = EK_REG;
    e->u.reg = reg;
    return reg;
  }
  case EK_FLT: {
    int reg = getreg(gs);
    int k = addKflt(gs, e->u.fval);
    emitABx(gs, OP_LOADK, reg, k);
    e->kind = EK_REG;
    e->u.reg = reg;
    return reg;
  }
  case EK_STR: {
    int reg = getreg(gs);
    int k = addKstr(gs, e->u.sval);
    emitABx(gs, OP_LOADK, reg, k);
    e->kind = EK_REG;
    e->u.reg = reg;
    return reg;
  }
  case EK_K: {
    int reg = getreg(gs);
    emitABx(gs, OP_LOADK, reg, e->u.k);
    e->kind = EK_REG;
    e->u.reg = reg;
    return reg;
  }
  case EK_REG:
    return e->u.reg;
  case EK_UPVAL: {
    int reg = getreg(gs);
    emitABC(gs, OP_GETUPVAL, reg, e->u.reg, 0);
    e->kind = EK_REG;
    e->u.reg = reg;
    return reg;
  }
  case EK_GLOBAL: {
    int reg = getreg(gs);
    emitABC(gs, OP_GETTABUP, reg, e->u.ind.t, e->u.ind.key);
    e->kind = EK_REG;
    e->u.reg = reg;
    return reg;
  }
  case EK_INDEX: {
    int reg = getreg(gs);
    if (e->u.ind.isupval) {
      emitABC(gs, OP_GETTABUP, reg, e->u.ind.t, e->u.ind.key);
    } else {
      emitABC(gs, OP_GETTABLE, reg, e->u.ind.t, e->u.ind.key);
    }
    e->kind = EK_REG;
    e->u.reg = reg;
    return reg;
  }
  case EK_RELOC: {
    /* Patch instruction with dest register */
    int reg = getreg(gs);
    Instruction *i = &gs->f->code[e->u.pc];
    *i = (*i & 0x7F) | (cast(Instruction, reg) << 7);
    e->kind = EK_REG;
    e->u.reg = reg;
    return reg;
  }
  case EK_CALL: {
    /* Call already emitted, result is in base register */
    e->kind = EK_REG;
    return e->u.reg;
  }
  case EK_VARARG: {
    /* VARARG already emitted, result is in base register */
    e->kind = EK_REG;
    return e->u.reg;
  }
  default:
    return getreg(gs);
  }
}

/* Discharge expression to next free register */
static int exp2nextreg(GenState *gs, ExpResult *e) {
  int reg = gs->freereg;
  int r = exp2reg(gs, e);
  if (r != reg) {
    emitABC(gs, OP_MOVE, reg, r, 0);
    reserveregs(gs, 1);
    return reg;
  }
  return r;
}

/* Generate nil literal */
static void gennil(GenState *gs, lus_Node *node, ExpResult *e) {
  (void)gs;
  (void)node;
  initexp(e, EK_NIL);
}

/* Generate boolean literal */
static void genbool(GenState *gs, lus_Node *node, ExpResult *e) {
  (void)gs;
  initexp(e, node->type == LUS_NODE_TRUE ? EK_TRUE : EK_FALSE);
}

/* Generate integer literal */
static void genint(GenState *gs, lus_Node *node, ExpResult *e) {
  (void)gs;
  lus_IntegerNode *n = (lus_IntegerNode *)node;
  initexp(e, EK_INT);
  e->u.ival = n->value;
}

/* Generate float literal */
static void genfloat(GenState *gs, lus_Node *node, ExpResult *e) {
  (void)gs;
  lus_FloatNode *n = (lus_FloatNode *)node;
  initexp(e, EK_FLT);
  e->u.fval = n->value;
}

/* Generate string literal */
static void genstring(GenState *gs, lus_Node *node, ExpResult *e) {
  (void)gs;
  lus_StringNode *n = (lus_StringNode *)node;
  initexp(e, EK_STR);
  e->u.sval = n->value;
}

/* Generate variable reference */
static void genvar(GenState *gs, lus_Node *node, ExpResult *e) {
  lus_VarNode *n = (lus_VarNode *)node;
  initexp(e, EK_VOID);
  resolvevar(gs, n->name, e);
}

/* Generate vararg */
static void genvararg(GenState *gs, lus_Node *node, ExpResult *e) {
  (void)node;
  int reg = getreg(gs);
  emitABC(gs, OP_VARARG, reg, 2, 0); /* 2 = one result */
  initexp(e, EK_VARARG);
  e->u.reg = reg;
}

/* Binary operator to opcode mapping */
static OpCode binopr2op(int op) {
  /* Matches parser's operator indices */
  static const OpCode ops[] = {
      OP_ADD,  OP_SUB, OP_MUL,  OP_MOD, OP_POW, OP_DIV,    OP_IDIV,
      OP_BAND, OP_BOR, OP_BXOR, OP_SHL, OP_SHR, OP_CONCAT,
      /* comparison ops handled separately */
  };
  if (op >= 0 && op < 12)
    return ops[op];
  return OP_ADD;
}

/* Generate binary operation */
static void genbinop(GenState *gs, lus_Node *node, ExpResult *e) {
  lus_BinOpNode *n = (lus_BinOpNode *)node;
  int op = n->op;

  ExpResult left, right;
  initexp(&left, EK_VOID);
  genexpr(gs, n->left, &left);
  int lreg = exp2reg(gs, &left);
  initexp(&right, EK_VOID);
  genexpr(gs, n->right, &right);
  int rreg = exp2reg(gs, &right);

  if (op <= 11) {
    /* Arithmetic / bitwise */
    int dest = lreg; /* reuse left register */
    emitABC(gs, binopr2op(op), dest, lreg, rreg);
    initexp(e, EK_REG);
    e->u.reg = dest;
  } else if (op == 12) {
    /* Concat */
    emitABC(gs, OP_CONCAT, lreg, rreg, 0);
    initexp(e, EK_REG);
    e->u.reg = lreg;
  } else if (op >= 13 && op <= 18) {
    /* Comparison: EQ, LT, LE, NE, GT, GE */
    OpCode cmpop;
    int flip = 0;
    switch (op) {
    case 13:
      cmpop = OP_EQ;
      break;
    case 14:
      cmpop = OP_LT;
      break;
    case 15:
      cmpop = OP_LE;
      break;
    case 16:
      cmpop = OP_EQ;
      flip = 1;
      break; /* NE = not EQ */
    case 17:
      cmpop = OP_LT;
      flip = 1;
      break; /* GT = LT flipped */
    case 18:
      cmpop = OP_LE;
      flip = 1;
      break; /* GE = LE flipped */
    default:
      cmpop = OP_EQ;
    }
    if (op == 17 || op == 18) {
      /* Swap operands for GT/GE */
      int tmp = lreg;
      lreg = rreg;
      rreg = tmp;
    }
    emitABC(gs, cmpop, flip, lreg, rreg);
    emitsJ(gs, OP_JMP, 1); /* skip next */
    int fpc = gs->pc;
    int dest = getreg(gs);
    emitABC(gs, OP_LOADFALSE, dest, 0, 0);
    emitsJ(gs, OP_JMP, 1); /* skip load true */
    gs->f->code[fpc - 1] = EM_sJ(OP_JMP, gs->pc - fpc);
    emitABC(gs, OP_LOADTRUE, dest, 0, 0);
    initexp(e, EK_REG);
    e->u.reg = dest;
  } else if (op == 19) {
    /* AND */
    int dest = lreg;
    emitABC(gs, OP_TEST, lreg, 0, 0);
    int jf = emitsJ(gs, OP_JMP, 0); /* jump if false */
    emitABC(gs, OP_MOVE, dest, rreg, 0);
    int j = emitsJ(gs, OP_JMP, 0); /* skip else */
    gs->f->code[jf] = EM_sJ(OP_JMP, gs->pc - jf - 1);
    /* false case: keep lreg */
    gs->f->code[j] = EM_sJ(OP_JMP, gs->pc - j - 1);
    initexp(e, EK_REG);
    e->u.reg = dest;
  } else if (op == 20) {
    /* OR */
    int dest = lreg;
    emitABC(gs, OP_TEST, lreg, 0, 1); /* skip if true */
    int jt = emitsJ(gs, OP_JMP, 0);   /* jump if true */
    emitABC(gs, OP_MOVE, dest, rreg, 0);
    int j = emitsJ(gs, OP_JMP, 0); /* skip else */
    gs->f->code[jt] = EM_sJ(OP_JMP, gs->pc - jt - 1);
    /* true case: keep lreg */
    gs->f->code[j] = EM_sJ(OP_JMP, gs->pc - j - 1);
    initexp(e, EK_REG);
    e->u.reg = dest;
  } else {
    initexp(e, EK_NIL);
  }

  gs->freereg = lreg + 1; /* free right operand reg */
}

/* Generate unary operation */
static void genunop(GenState *gs, lus_Node *node, ExpResult *e) {
  lus_UnOpNode *n = (lus_UnOpNode *)node;
  ExpResult operand;
  initexp(&operand, EK_VOID);
  genexpr(gs, n->operand, &operand);
  int reg = exp2reg(gs, &operand);

  OpCode op;
  switch (n->op) {
  case 0:
    op = OP_UNM;
    break; /* - */
  case 1:
    op = OP_BNOT;
    break; /* ~ */
  case 2:
    op = OP_NOT;
    break; /* not */
  case 3:
    op = OP_LEN;
    break; /* # */
  default:
    op = OP_UNM;
  }

  emitABC(gs, op, reg, reg, 0);
  initexp(e, EK_REG);
  e->u.reg = reg;
}

/* Generate function call */
static void gencall(GenState *gs, lus_Node *node, ExpResult *e) {
  lus_CallNode *n = (lus_CallNode *)node;

  ExpResult func;
  initexp(&func, EK_VOID);
  genexpr(gs, n->func, &func);
  int base = exp2reg(gs, &func);

  /* Generate arguments */
  int nargs = 0;
  for (lus_Node *arg = n->args.head; arg != NULL; arg = arg->next) {
    ExpResult ae;
    initexp(&ae, EK_VOID);
    genexpr(gs, arg, &ae);
    exp2nextreg(gs, &ae);
    nargs++;
  }

  /* Emit call: OP_CALL A B C
     A = base, B = nargs+1, C = nresults+1 */
  emitABC(gs, OP_CALL, base, nargs + 1, 2); /* 2 = one result */

  gs->freereg = base + 1; /* call leaves one result */
  initexp(e, EK_CALL);
  e->u.reg = base;
}

/* Generate method call */
static void genmethodcall(GenState *gs, lus_Node *node, ExpResult *e) {
  lus_MethodCallNode *n = (lus_MethodCallNode *)node;

  ExpResult obj;
  initexp(&obj, EK_VOID);
  genexpr(gs, n->object, &obj);
  int base = exp2reg(gs, &obj);

  /* SELF: R(A+1) = R(B); R(A) = R(B)[RK(C)] */
  int k = addKstr(gs, n->method);
  emitABC(gs, OP_SELF, base, base, k);

  /* Generate arguments */
  int nargs = 1; /* self is implicit first arg */
  for (lus_Node *arg = n->args.head; arg != NULL; arg = arg->next) {
    ExpResult ae;
    initexp(&ae, EK_VOID);
    genexpr(gs, arg, &ae);
    exp2nextreg(gs, &ae);
    nargs++;
  }

  emitABC(gs, OP_CALL, base, nargs + 1, 2);

  gs->freereg = base + 1;
  initexp(e, EK_CALL);
  e->u.reg = base;
}

/* Generate table index */
static void genindex(GenState *gs, lus_Node *node, ExpResult *e) {
  lus_IndexNode *n = (lus_IndexNode *)node;

  ExpResult tbl, key;
  initexp(&tbl, EK_VOID);
  genexpr(gs, n->table, &tbl);
  int treg = exp2reg(gs, &tbl);
  initexp(&key, EK_VOID);
  genexpr(gs, n->key, &key);
  int kreg = exp2reg(gs, &key);

  initexp(e, EK_INDEX);
  e->u.ind.t = treg;
  e->u.ind.key = kreg;
  e->u.ind.isupval = 0;
}

/* Generate field access */
static void genfield(GenState *gs, lus_Node *node, ExpResult *e) {
  lus_FieldNode *n = (lus_FieldNode *)node;

  ExpResult tbl;
  initexp(&tbl, EK_VOID);
  genexpr(gs, n->table, &tbl);
  int treg = exp2reg(gs, &tbl);
  int k = addKstr(gs, n->field);

  int dest = getreg(gs);
  emitABC(gs, OP_GETFIELD, dest, treg, k);
  initexp(e, EK_REG);
  e->u.reg = dest;
}

/* Generate function expression (closure) */
static void genfunction(GenState *gs, lus_Node *node, ExpResult *e) {
  lus_FunctionNode *fn = (lus_FunctionNode *)node;
  lua_State *L = gs->L;

  /* Create new Proto for nested function */
  Proto *f = luaF_newproto(L);
  luaC_objbarrier(L, gs->f, f);

  /* Initialize child GenState */
  GenState child;
  memset(&child, 0, sizeof(GenState));
  child.L = L;
  child.prev = gs;
  child.f = f;
  child.arena = gs->arena;
  child.nlocals = 0;
  child.nactvar = 0;
  child.nups = 1; /* _ENV is upvalue 0 */
  child.freereg = 0;
  child.pc = 0;
  child.linenumber = node->line;

  /* Setup _ENV upvalue (inherited from parent) */
  child.upvalues[0].name = luaS_newliteral(L, "_ENV");
  child.upvalues[0].instack = 0; /* from enclosing upvalue */
  child.upvalues[0].idx = 0;     /* _ENV is upvalue 0 in parent too */

  /* Initialize Proto with pre-allocated code array */
  f->source = gs->f->source;
  f->maxstacksize = 2;
  f->flag = fn->isvararg ? PF_VAHID : 0;
  f->numparams = cast_byte(fn->params.count);
  f->sizecode = 64;
  f->code = luaM_newvector(L, 64, Instruction);
  f->sizek = 0;
  f->k = NULL;
  f->sizep = 0;
  f->p = NULL;

  /* Reserve registers for parameters and register them as locals */
  int nparams = fn->params.count;
  reserveregs(&child, nparams);
  child.nactvar = nparams;

  int paramidx = 0;
  for (lus_Node *param = fn->params.head; param != NULL; param = param->next) {
    lus_VarNode *v = (lus_VarNode *)param;
    child.locals[paramidx].name = v->name;
    child.locals[paramidx].reg = paramidx;
    child.locals[paramidx].startpc = 0;
    child.locals[paramidx].endpc = -1;
    child.nlocals++;
    paramidx++;
  }

  /* Compile function body */
  genblock(&child, (lus_BlockNode *)fn->body);

  /* Add implicit return */
  emitABC(&child, OP_RETURN0, 0, 0, 0);

  /* Finalize Proto - shrink code array to actual size */
  if (child.pc < f->sizecode) {
    luaM_reallocvector(L, f->code, f->sizecode, child.pc, Instruction);
  }
  f->sizecode = child.pc;

  /* Setup upvalue info */
  f->sizeupvalues = child.nups;
  f->upvalues = luaM_newvector(L, child.nups, Upvaldesc);
  for (int i = 0; i < child.nups; i++) {
    f->upvalues[i].name = child.upvalues[i].name;
    f->upvalues[i].instack = child.upvalues[i].instack;
    f->upvalues[i].idx = child.upvalues[i].idx;
  }

  /* Add to parent's function list */
  int idx = gs->f->sizep;
  luaM_growvector(L, gs->f->p, idx, gs->f->sizep, Proto *, SHRT_MAX,
                  "functions");
  gs->f->p[idx] = f;

  /* Emit OP_CLOSURE */
  int reg = getreg(gs);
  emitABx(gs, OP_CLOSURE, reg, idx);

  initexp(e, EK_REG);
  e->u.reg = reg;
}

/* Generate table constructor */
static void gentable(GenState *gs, lus_Node *node, ExpResult *e) {
  lus_TableNode *n = (lus_TableNode *)node;

  int reg = getreg(gs);
  int pc = emitABC(gs, OP_NEWTABLE, reg, 0, 0);
  emitABC(gs, OP_EXTRAARG, n->narray, 0, 0); /* size hints */

  int arrayidx = 0;
  for (lus_TableField *f = n->fields; f != NULL; f = f->next) {
    ExpResult val;
    genexpr(gs, f->value, &val);
    int vreg = exp2reg(gs, &val);

    if (f->key == NULL) {
      /* Array element */
      arrayidx++;
      emitABC(gs, OP_SETI, reg, arrayidx, vreg);
    } else {
      /* Hash element */
      ExpResult key;
      genexpr(gs, f->key, &key);
      if (key.kind == EK_STR) {
        int k = addKstr(gs, key.u.sval);
        emitABC(gs, OP_SETFIELD, reg, k, vreg);
      } else {
        int kreg = exp2reg(gs, &key);
        emitABC(gs, OP_SETTABLE, reg, kreg, vreg);
      }
    }
    gs->freereg = reg + 1;
  }

  (void)pc; /* could patch size hints */
  initexp(e, EK_REG);
  e->u.reg = reg;
}

/* Generate expression */
static void genexpr(GenState *gs, lus_Node *node, ExpResult *e) {
  gs->linenumber = node->line;

  switch (node->type) {
  case LUS_NODE_NIL:
    gennil(gs, node, e);
    break;
  case LUS_NODE_TRUE:
  case LUS_NODE_FALSE:
    genbool(gs, node, e);
    break;
  case LUS_NODE_INTEGER:
    genint(gs, node, e);
    break;
  case LUS_NODE_FLOAT:
    genfloat(gs, node, e);
    break;
  case LUS_NODE_STRING:
    genstring(gs, node, e);
    break;
  case LUS_NODE_VAR:
  case LUS_NODE_UPVAL:
  case LUS_NODE_GLOBAL:
    genvar(gs, node, e);
    break;
  case LUS_NODE_VARARG:
    genvararg(gs, node, e);
    break;
  case LUS_NODE_BINOP:
    genbinop(gs, node, e);
    break;
  case LUS_NODE_UNOP:
    genunop(gs, node, e);
    break;
  case LUS_NODE_CALL:
    gencall(gs, node, e);
    break;
  case LUS_NODE_METHODCALL:
    genmethodcall(gs, node, e);
    break;
  case LUS_NODE_INDEX:
    genindex(gs, node, e);
    break;
  case LUS_NODE_FIELD:
    genfield(gs, node, e);
    break;
  case LUS_NODE_TABLE:
    gentable(gs, node, e);
    break;
  case LUS_NODE_FUNCTION:
    genfunction(gs, node, e);
    break;
  default:
    initexp(e, EK_NIL);
    break;
  }
}

/* }================================================================== */

/*
** {==================================================================
** Statement Code Generation
** ===================================================================
*/

/* Generate return statement */
static void genreturn(GenState *gs, lus_Node *node) {
  lus_ReturnNode *n = (lus_ReturnNode *)node;
  int first = gs->freereg;
  int nret = 0;

  for (lus_Node *expr = n->exprs.head; expr != NULL; expr = expr->next) {
    ExpResult e;
    initexp(&e, EK_VOID);
    genexpr(gs, expr, &e);
    exp2nextreg(gs, &e);
    nret++;
  }

  if (nret == 0) {
    emitABC(gs, OP_RETURN0, 0, 0, 0);
  } else if (nret == 1) {
    emitABC(gs, OP_RETURN1, first, 0, 0);
  } else {
    emitABC(gs, OP_RETURN, first, nret + 1, 0);
  }
}

/* Generate local declaration */
static void genlocal(GenState *gs, lus_Node *node) {
  lus_LocalNode *n = (lus_LocalNode *)node;
  int nvars = n->vars.count;
  (void)nvars; /* used for potential multi-result handling */

  /* Generate expressions first */
  int base = gs->freereg;
  int i = 0;
  for (lus_Node *expr = n->exprs.head; expr != NULL; expr = expr->next, i++) {
    ExpResult e;
    genexpr(gs, expr, &e);
    exp2nextreg(gs, &e);
  }

  /* Fill missing with nil */
  for (; i < nvars; i++) {
    emitABC(gs, OP_LOADNIL, gs->freereg, 0, 0);
    reserveregs(gs, 1);
  }

  /* Register local variables */
  i = 0;
  for (lus_Node *var = n->vars.head; var != NULL; var = var->next, i++) {
    lus_VarNode *v = (lus_VarNode *)var;
    int idx = gs->nlocals++;
    gs->locals[idx].name = v->name;
    gs->locals[idx].reg = base + i;
    gs->locals[idx].startpc = gs->pc;
    gs->locals[idx].endpc = -1;
    gs->nactvar++;
  }
}

/* Generate assignment */
static void genassign(GenState *gs, lus_Node *node) {
  lus_AssignNode *n = (lus_AssignNode *)node;

  /* Evaluate all expressions to temp registers */
  int base = gs->freereg;
  for (lus_Node *expr = n->exprs.head; expr != NULL; expr = expr->next) {
    ExpResult e;
    genexpr(gs, expr, &e);
    exp2nextreg(gs, &e);
  }

  /* Store to variables */
  int i = 0;
  for (lus_Node *var = n->vars.head; var != NULL; var = var->next, i++) {
    ExpResult tgt;
    initexp(&tgt, EK_VOID);
    genexpr(gs, var, &tgt);
    int src = base + i;

    switch (tgt.kind) {
    case EK_REG:
      emitABC(gs, OP_MOVE, tgt.u.reg, src, 0);
      break;
    case EK_UPVAL:
      emitABC(gs, OP_SETUPVAL, src, tgt.u.reg, 0);
      break;
    case EK_GLOBAL:
      emitABC(gs, OP_SETTABUP, tgt.u.ind.t, tgt.u.ind.key, src);
      break;
    case EK_INDEX:
      if (tgt.u.ind.isupval) {
        emitABC(gs, OP_SETTABUP, tgt.u.ind.t, tgt.u.ind.key, src);
      } else {
        emitABC(gs, OP_SETTABLE, tgt.u.ind.t, tgt.u.ind.key, src);
      }
      break;
    default:
      break;
    }
  }

  gs->freereg = base; /* free temp registers */
}

/* Generate if statement */
static void genif(GenState *gs, lus_Node *node) {
  lus_IfNode *n = (lus_IfNode *)node;
  int exitjumps[64]; /* jump to end */
  int nexits = 0;

  for (lus_IfBranch *br = n->branches; br != NULL; br = br->next) {
    if (br->cond != NULL) {
      ExpResult cond;
      initexp(&cond, EK_VOID);
      genexpr(gs, br->cond, &cond);
      int creg = exp2reg(gs, &cond);
      emitABC(gs, OP_TEST, creg, 0, 0);
      int jf = emitsJ(gs, OP_JMP, 0); /* jump if false */

      genblock(gs, (lus_BlockNode *)br->body);

      if (br->next != NULL) {
        exitjumps[nexits++] = emitsJ(gs, OP_JMP, 0); /* jump to end */
      }

      /* Patch false jump */
      gs->f->code[jf] = EM_sJ(OP_JMP, gs->pc - jf - 1);
    } else {
      /* else branch */
      genblock(gs, (lus_BlockNode *)br->body);
    }
  }

  /* Patch exit jumps */
  for (int i = 0; i < nexits; i++) {
    gs->f->code[exitjumps[i]] = EM_sJ(OP_JMP, gs->pc - exitjumps[i] - 1);
  }
}

/* Generate while statement */
static void genwhile(GenState *gs, lus_Node *node) {
  lus_WhileNode *n = (lus_WhileNode *)node;
  int loopstart = gs->pc;

  ExpResult cond;
  initexp(&cond, EK_VOID);
  genexpr(gs, n->cond, &cond);
  int creg = exp2reg(gs, &cond);
  emitABC(gs, OP_TEST, creg, 0, 0);
  int jf = emitsJ(gs, OP_JMP, 0); /* exit loop */

  genblock(gs, (lus_BlockNode *)n->body);

  /* Jump back to start */
  emitsJ(gs, OP_JMP, loopstart - gs->pc - 1);

  /* Patch exit */
  gs->f->code[jf] = EM_sJ(OP_JMP, gs->pc - jf - 1);
}

/* Generate repeat statement */
static void genrepeat(GenState *gs, lus_Node *node) {
  lus_RepeatNode *n = (lus_RepeatNode *)node;
  int loopstart = gs->pc;

  genblock(gs, (lus_BlockNode *)n->body);

  ExpResult cond;
  initexp(&cond, EK_VOID);
  genexpr(gs, n->cond, &cond);
  int creg = exp2reg(gs, &cond);
  emitABC(gs, OP_TEST, creg, 0, 1);           /* test for true */
  emitsJ(gs, OP_JMP, loopstart - gs->pc - 1); /* loop if false */
}

/* Generate numeric for */
static void genfor(GenState *gs, lus_Node *node) {
  lus_ForNode *n = (lus_ForNode *)node;

  /* Evaluate init, limit, step */
  int base = gs->freereg;
  ExpResult e;
  initexp(&e, EK_VOID);

  genexpr(gs, n->init, &e);
  exp2nextreg(gs, &e); /* init */

  genexpr(gs, n->limit, &e);
  exp2nextreg(gs, &e); /* limit */

  if (n->step != NULL) {
    genexpr(gs, n->step, &e);
  } else {
    initexp(&e, EK_INT);
    e.u.ival = 1;
  }
  exp2nextreg(gs, &e); /* step */

  /* Loop variable register */
  int loopvar = gs->freereg;
  reserveregs(gs, 1);

  /* OP_FORPREP */
  int prep = emitABx(gs, OP_FORPREP, base, 0);

  /* Register loop variable as local */
  int idx = gs->nlocals++;
  gs->locals[idx].name = n->var->name;
  gs->locals[idx].reg = loopvar;
  gs->locals[idx].startpc = gs->pc;
  gs->locals[idx].endpc = -1;
  gs->nactvar++;

  int loopstart = gs->pc;

  genblock(gs, (lus_BlockNode *)n->body);

  /* OP_FORLOOP */
  int looppc = emitABx(gs, OP_FORLOOP, base, 0);

  /* Patch jumps */
  int offset = gs->pc - prep - 1;
  gs->f->code[prep] = CREATE_ABx(OP_FORPREP, base, offset);
  offset = loopstart - looppc - 1;
  gs->f->code[looppc] = CREATE_ABx(OP_FORLOOP, base, offset);

  gs->nactvar--;
  gs->freereg = base;
}

/* Generate statement */
static void genstmt(GenState *gs, lus_Node *node) {
  if (node == NULL)
    return;
  gs->linenumber = node->line;

  switch (node->type) {
  case LUS_NODE_RETURN:
    genreturn(gs, node);
    break;
  case LUS_NODE_LOCAL:
    genlocal(gs, node);
    break;
  case LUS_NODE_ASSIGN:
    genassign(gs, node);
    break;
  case LUS_NODE_IF:
    genif(gs, node);
    break;
  case LUS_NODE_WHILE:
    genwhile(gs, node);
    break;
  case LUS_NODE_REPEAT:
    genrepeat(gs, node);
    break;
  case LUS_NODE_FOR:
    genfor(gs, node);
    break;
  case LUS_NODE_BLOCK:
    genblock(gs, (lus_BlockNode *)node);
    break;
  case LUS_NODE_CALL:
  case LUS_NODE_METHODCALL: {
    /* Expression as statement */
    ExpResult e;
    initexp(&e, EK_VOID);
    genexpr(gs, node, &e);
    gs->freereg = e.u.reg; /* discard result */
    break;
  }
  default:
    break;
  }
}

/* Generate block */
static void genblock(GenState *gs, lus_BlockNode *block) {
  int savedactvar = gs->nactvar;
  int savedfreereg = gs->freereg;

  for (lus_Node *stmt = block->stmts.head; stmt != NULL; stmt = stmt->next) {
    genstmt(gs, stmt);
  }

  /* Close locals */
  gs->nactvar = savedactvar;
  gs->freereg = savedfreereg;
}

/* }================================================================== */

/*
** {==================================================================
** Main Code Generator API
** ===================================================================
*/

Proto *lus_codegen(lua_State *L, lus_Node *ast, const char *source) {
  if (ast == NULL || ast->type != LUS_NODE_BLOCK)
    return NULL;

  lus_BlockNode *block = (lus_BlockNode *)ast;
  GenState gs;

  /* Create Proto */
  Proto *f = luaF_newproto(L);

  /* Initialize GenState */
  memset(&gs, 0, sizeof(GenState));
  gs.L = L;
  gs.prev = NULL;
  gs.f = f;
  gs.arena = block->arena;
  gs.nlocals = 0;
  gs.nactvar = 0;
  gs.nups = 1; /* _ENV is upvalue 0 */
  gs.freereg = 0;
  gs.pc = 0;
  gs.lasttarget = 0;
  gs.linenumber = 1;

  /* Setup _ENV upvalue */
  gs.upvalues[0].name = luaS_newliteral(L, "_ENV");
  gs.upvalues[0].instack = 1;
  gs.upvalues[0].idx = 0;
  gs.upvalues[0].kind = VDKREG;

  /* Initialize Proto with pre-allocated arrays */
  f->source = luaS_new(L, source);
  f->maxstacksize = 2;
  f->flag = PF_VAHID; /* vararg (main chunk) */
  f->numparams = 0;
  /* Pre-allocate code array to avoid NULL pointer in emit */
  f->sizecode = 64;
  f->code = luaM_newvector(L, 64, Instruction);
  f->sizek = 0;
  f->k = NULL;
  f->sizep = 0;
  f->p = NULL;

  /* Compile block */
  genblock(&gs, block);

  /* Add implicit return */
  emitABC(&gs, OP_RETURN0, 0, 0, 0);

  /* Finalize Proto - shrink code array to actual size */
  fprintf(stderr, "DEBUG lus_codegen: finalizing pc=%d sizecode=%d\n", gs.pc,
          f->sizecode);
  for (int dbg_i = 0; dbg_i < gs.pc; dbg_i++) {
    Instruction dbg_inst = f->code[dbg_i];
    fprintf(stderr, "  [%d] inst=0x%08x op=%d\n", dbg_i, (unsigned)dbg_inst,
            (int)GET_OPCODE(dbg_inst));
  }
  if (gs.pc < f->sizecode) {
    f->code = luaM_reallocvector(L, f->code, f->sizecode, gs.pc, Instruction);
  }
  f->sizecode = gs.pc;

  /* Finalize constants - set actual count and shrink array */
  if (gs.nk < f->sizek) {
    f->k = luaM_reallocvector(L, f->k, f->sizek, gs.nk, TValue);
  }
  f->sizek = gs.nk;

  /* Setup upvalue info */
  f->sizeupvalues = gs.nups;
  f->upvalues = luaM_newvector(L, gs.nups, Upvaldesc);
  for (int i = 0; i < gs.nups; i++) {
    f->upvalues[i].name = gs.upvalues[i].name;
    f->upvalues[i].instack = gs.upvalues[i].instack;
    f->upvalues[i].idx = gs.upvalues[i].idx;
  }

  return f;
}

void lus_codegen_func(lus_CodeGenState *cs, lus_FunctionNode *fn) {
  (void)cs;
  (void)fn;
  /* Would recursively compile nested functions */
}

/* }================================================================== */

/*
** {==================================================================
** CFG Construction
** ===================================================================
*/

lus_CFG *lus_build_cfg(lua_State *L, Proto *p) {
  lusM_Arena *a = lusM_newarena(L, 0);
  lus_CFG *cfg = cast(lus_CFG *, lusM_arenaalloc(a, sizeof(lus_CFG)));

  cfg->arena = a;
  cfg->blocks = NULL;
  cfg->entry = NULL;
  cfg->nblocks = 0;

  if (p->sizecode == 0)
    return cfg;

  /* First pass: identify leaders */
  lu_byte *isleader = cast(lu_byte *, lusM_arenaalloc(a, p->sizecode));
  memset(isleader, 0, p->sizecode);
  isleader[0] = 1;

  for (int pc = 0; pc < p->sizecode; pc++) {
    Instruction i = p->code[pc];
    OpCode op = GET_OPCODE(i);

    switch (op) {
    case OP_JMP: {
      int offset = GETARG_sJ(i);
      int target = pc + 1 + offset;
      if (target >= 0 && target < p->sizecode)
        isleader[target] = 1;
      if (pc + 1 < p->sizecode)
        isleader[pc + 1] = 1;
      break;
    }
    case OP_FORLOOP:
    case OP_FORPREP:
    case OP_TFORLOOP: {
      int offset = GETARG_Bx(i);
      int target = pc + 1 + offset;
      if (target >= 0 && target < p->sizecode)
        isleader[target] = 1;
      if (pc + 1 < p->sizecode)
        isleader[pc + 1] = 1;
      break;
    }
    case OP_EQ:
    case OP_LT:
    case OP_LE:
    case OP_EQK:
    case OP_EQI:
    case OP_LTI:
    case OP_LEI:
    case OP_GTI:
    case OP_GEI:
    case OP_TEST:
    case OP_TESTSET: {
      if (pc + 2 < p->sizecode)
        isleader[pc + 2] = 1;
      break;
    }
    case OP_RETURN:
    case OP_RETURN0:
    case OP_RETURN1:
    case OP_TAILCALL: {
      if (pc + 1 < p->sizecode)
        isleader[pc + 1] = 1;
      break;
    }
    default:
      break;
    }
  }

  /* Second pass: create blocks */
  lus_CFGBlock *tail = NULL;
  lus_CFGBlock *current = NULL;
  int blockid = 0;

  for (int pc = 0; pc < p->sizecode; pc++) {
    if (isleader[pc]) {
      lus_CFGBlock *b =
          cast(lus_CFGBlock *, lusM_arenaalloc(a, sizeof(lus_CFGBlock)));
      b->id = blockid++;
      b->startpc = pc;
      b->endpc = pc;
      b->next = NULL;
      b->truejmp = NULL;
      b->falsejmp = NULL;

      if (tail == NULL) {
        cfg->blocks = b;
        cfg->entry = b;
      } else {
        tail->next = b;
        tail->endpc = pc - 1;
      }
      tail = b;
      current = b;
      cfg->nblocks++;
    } else if (current != NULL) {
      current->endpc = pc;
    }
  }

  /* Third pass: link successors */
  for (lus_CFGBlock *b = cfg->blocks; b != NULL; b = b->next) {
    if (b->endpc >= 0 && b->endpc < p->sizecode) {
      Instruction i = p->code[b->endpc];
      OpCode op = GET_OPCODE(i);

      switch (op) {
      case OP_JMP: {
        int offset = GETARG_sJ(i);
        int target = b->endpc + 1 + offset;
        for (lus_CFGBlock *t = cfg->blocks; t != NULL; t = t->next) {
          if (t->startpc == target) {
            b->truejmp = t;
            break;
          }
        }
        break;
      }
      case OP_FORLOOP:
      case OP_FORPREP:
      case OP_TFORLOOP: {
        int offset = GETARG_Bx(i);
        int target = b->endpc + 1 + offset;
        b->truejmp = b->next;
        for (lus_CFGBlock *t = cfg->blocks; t != NULL; t = t->next) {
          if (t->startpc == target) {
            b->falsejmp = t;
            break;
          }
        }
        break;
      }
      case OP_RETURN:
      case OP_RETURN0:
      case OP_RETURN1:
      case OP_TAILCALL:
        break;
      default:
        b->truejmp = b->next;
        break;
      }
    }
  }

  return cfg;
}

void lus_free_cfg(lus_CFG *cfg) {
  if (cfg != NULL && cfg->arena != NULL) {
    lusM_freearena(cfg->arena);
  }
}

/* }================================================================== */

/*
** {==================================================================
** CFG Visualization
** ===================================================================
*/

static const char *opname(OpCode op) {
  static const char *const opnames[] = {
      "MOVE",      "LOADI",      "LOADF",    "LOADK",    "LOADKX",
      "LOADFALSE", "LFALSESKIP", "LOADTRUE", "LOADNIL",  "GETUPVAL",
      "SETUPVAL",  "GETTABUP",   "GETTABLE", "GETI",     "GETFIELD",
      "SETTABUP",  "SETTABLE",   "SETI",     "SETFIELD", "NEWTABLE",
      "SELF",      "ADDI",       "ADDK",     "SUBK",     "MULK",
      "MODK",      "POWK",       "DIVK",     "IDIVK",    "BANDK",
      "BORK",      "BXORK",      "SHRI",     "SHLI",     "ADD",
      "SUB",       "MUL",        "MOD",      "POW",      "DIV",
      "IDIV",      "BAND",       "BOR",      "BXOR",     "SHL",
      "SHR",       "MMBIN",      "MMBINI",   "MMBINK",   "UNM",
      "BNOT",      "NOT",        "LEN",      "CONCAT",   "CLOSE",
      "TBC",       "JMP",        "EQ",       "LT",       "LE",
      "EQK",       "EQI",        "LTI",      "LEI",      "GTI",
      "GEI",       "TEST",       "TESTSET",  "CALL",     "TAILCALL",
      "RETURN",    "RETURN0",    "RETURN1",  "FORLOOP",  "FORPREP",
      "TFORPREP",  "TFORCALL",   "TFORLOOP", "SETLIST",  "CLOSURE",
      "VARARG",    "VARARGPREP", "EXTRAARG"};
  if (op < sizeof(opnames) / sizeof(opnames[0]))
    return opnames[op];
  return "???";
}

int lus_cfg_to_dot(FILE *out, lus_CFG *cfg, Proto *p) {
  if (out == NULL || cfg == NULL)
    return -1;

  fprintf(out, "digraph CFG {\n");
  fprintf(out, "  rankdir=TB;\n");
  fprintf(out, "  node [shape=box, fontname=\"Courier\"];\n");
  fprintf(out, "  edge [fontname=\"Courier\"];\n\n");

  for (lus_CFGBlock *b = cfg->blocks; b != NULL; b = b->next) {
    fprintf(out, "  block%d [label=\"B%d", b->id, b->id);
    if (p != NULL) {
      fprintf(out, "\\n");
      for (int pc = b->startpc; pc <= b->endpc && pc < p->sizecode; pc++) {
        Instruction i = p->code[pc];
        OpCode op = GET_OPCODE(i);
        fprintf(out, "%d: %s\\l", pc, opname(op));
      }
    }
    fprintf(out, "\"];\n");
  }

  fprintf(out, "\n");

  for (lus_CFGBlock *b = cfg->blocks; b != NULL; b = b->next) {
    if (b->truejmp != NULL) {
      fprintf(out, "  block%d -> block%d", b->id, b->truejmp->id);
      if (b->falsejmp != NULL)
        fprintf(out, " [label=\"T\"]");
      fprintf(out, ";\n");
    }
    if (b->falsejmp != NULL) {
      fprintf(out, "  block%d -> block%d [label=\"F\", style=dashed];\n", b->id,
              b->falsejmp->id);
    }
  }

  fprintf(out, "}\n");
  return 0;
}

/* }================================================================== */
