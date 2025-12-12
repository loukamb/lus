/*
** $Id: lparser.c $
** Lua Parser
** See Copyright Notice in lua.h
*/

#define lparser_c
#define LUA_CORE

#include "lprefix.h"

#include <limits.h>
#include <string.h>

#include "lua.h"

#include "last.h"
#include "lcode.h"
#include "ldebug.h"
#include "ldo.h"
#include "lenum.h"
#include "lfunc.h"
#include "llex.h"
#include "lmem.h"
#include "lobject.h"
#include "lopcodes.h"
#include "lparser.h"
#include "lstate.h"
#include "lstring.h"
#include "ltable.h"

/* maximum number of variable declarations per function (must be
   smaller than 250, due to the bytecode format) */
#define MAXVARS 200

#define hasmultret(k) ((k) == VCALL || (k) == VVARARG || (k) == VCATCH)

/* because all strings are unified by the scanner, the parser
   can use pointer equality for string equality */
#define eqstr(a, b) ((a) == (b))

/*
** nodes for block list (list of active blocks)
*/
typedef struct BlockCnt {
  struct BlockCnt *previous; /* chain */
  int firstlabel;            /* index of first label in this block */
  int firstgoto;             /* index of first pending goto in this block */
  short nactvar;             /* number of active declarations at block entry */
  lu_byte upval;     /* true if some variable in the block is an upvalue */
  lu_byte isloop;    /* 1 if 'block' is a loop; 2 if it has pending breaks */
  lu_byte insidetbc; /* true if inside the scope of a to-be-closed var. */
} BlockCnt;

/*
** prototypes for recursive non-terminal functions
*/
static void statement(LexState *ls);
static void expr(LexState *ls, expdesc *v);
static void catchexpr(LexState *ls, expdesc *v);
static void catchstat(LexState *ls, int line);

static l_noret error_expected(LexState *ls, int token) {
  luaX_syntaxerror(
      ls, luaO_pushfstring(ls->L, "%s expected", luaX_token2str(ls, token)));
}

static l_noret errorlimit(FuncState *fs, int limit, const char *what) {
  lua_State *L = fs->ls->L;
  const char *msg;
  int line = fs->f->linedefined;
  const char *where = (line == 0)
                          ? "main function"
                          : luaO_pushfstring(L, "function at line %d", line);
  msg = luaO_pushfstring(L, "too many %s (limit is %d) in %s", what, limit,
                         where);
  luaX_syntaxerror(fs->ls, msg);
}

void luaY_checklimit(FuncState *fs, int v, int l, const char *what) {
  if (l_unlikely(v > l))
    errorlimit(fs, l, what);
}

/*
** Test whether next token is 'c'; if so, skip it.
*/
static int testnext(LexState *ls, int c) {
  if (ls->t.token == c) {
    luaX_next(ls);
    return 1;
  } else
    return 0;
}

/*
** Check that next token is 'c'.
*/
static void check(LexState *ls, int c) {
  if (ls->t.token != c)
    error_expected(ls, c);
}

/*
** Check that next token is 'c' and skip it.
*/
static void checknext(LexState *ls, int c) {
  check(ls, c);
  luaX_next(ls);
}

#define check_condition(ls, c, msg)                                            \
  {                                                                            \
    if (!(c))                                                                  \
      luaX_syntaxerror(ls, msg);                                               \
  }

/*
** Check that next token is 'what' and skip it. In case of error,
** raise an error that the expected 'what' should match a 'who'
** in line 'where' (if that is not the current line).
*/
static void check_match(LexState *ls, int what, int who, int where) {
  if (l_unlikely(!testnext(ls, what))) {
    if (where == ls->linenumber) /* all in the same line? */
      error_expected(ls, what);  /* do not need a complex message */
    else {
      luaX_syntaxerror(
          ls, luaO_pushfstring(ls->L, "%s expected (to close %s at line %d)",
                               luaX_token2str(ls, what),
                               luaX_token2str(ls, who), where));
    }
  }
}

static TString *str_checkname(LexState *ls) {
  TString *ts;
  check(ls, TK_NAME);
  ts = ls->t.seminfo.ts;
  luaX_next(ls);
  return ts;
}

static void init_exp(expdesc *e, expkind k, int i) {
  e->f = e->t = NO_JUMP;
  e->k = k;
  e->u.info = i;
  e->ast = NULL;
}

static void codestring(expdesc *e, TString *s) {
  e->f = e->t = NO_JUMP;
  e->k = VKSTR;
  e->u.strval = s;
  e->ast = NULL;
}

static void codename(LexState *ls, expdesc *e) {
  codestring(e, str_checkname(ls));
}

/*
** Register a new local variable in the active 'Proto' (for debug
** information).
*/
static short registerlocalvar(LexState *ls, FuncState *fs, TString *varname) {
  Proto *f = fs->f;
  int oldsize = f->sizelocvars;
  luaM_growvector(ls->L, f->locvars, fs->ndebugvars, f->sizelocvars, LocVar,
                  SHRT_MAX, "local variables");
  while (oldsize < f->sizelocvars)
    f->locvars[oldsize++].varname = NULL;
  f->locvars[fs->ndebugvars].varname = varname;
  f->locvars[fs->ndebugvars].startpc = fs->pc;
  luaC_objbarrier(ls->L, f, varname);
  return fs->ndebugvars++;
}

/*
** Create a new variable with the given 'name' and given 'kind'.
** Return its index in the function.
*/
static int new_varkind(LexState *ls, TString *name, lu_byte kind) {
  lua_State *L = ls->L;
  FuncState *fs = ls->fs;
  Dyndata *dyd = ls->dyd;
  Vardesc *var;
  luaM_growvector(L, dyd->actvar.arr, dyd->actvar.n + 1, dyd->actvar.size,
                  Vardesc, SHRT_MAX, "variable declarations");
  var = &dyd->actvar.arr[dyd->actvar.n++];
  var->vd.kind = kind; /* default */
  var->vd.name = name;
  return dyd->actvar.n - 1 - fs->firstlocal;
}

/*
** Create a new local variable with the given 'name' and regular kind.
*/
static int new_localvar(LexState *ls, TString *name) {
  return new_varkind(ls, name, VDKREG);
}

#define new_localvarliteral(ls, v)                                             \
  new_localvar(ls, luaX_newstring(ls, "" v, (sizeof(v) / sizeof(char)) - 1));

/*
** Return the "variable description" (Vardesc) of a given variable.
** (Unless noted otherwise, all variables are referred to by their
** compiler indices.)
*/
static Vardesc *getlocalvardesc(FuncState *fs, int vidx) {
  return &fs->ls->dyd->actvar.arr[fs->firstlocal + vidx];
}

/*
** Convert 'nvar', a compiler index level, to its corresponding
** register. For that, search for the highest variable below that level
** that is in a register and uses its register index ('ridx') plus one.
*/
static lu_byte reglevel(FuncState *fs, int nvar) {
  while (nvar-- > 0) {
    Vardesc *vd = getlocalvardesc(fs, nvar); /* get previous variable */
    if (varinreg(vd))                        /* is in a register? */
      return cast_byte(vd->vd.ridx + 1);
  }
  return 0; /* no variables in registers */
}

/*
** Return the number of variables in the register stack for the given
** function.
*/
lu_byte luaY_nvarstack(FuncState *fs) { return reglevel(fs, fs->nactvar); }

/*
** Get the debug-information entry for current variable 'vidx'.
*/
static LocVar *localdebuginfo(FuncState *fs, int vidx) {
  Vardesc *vd = getlocalvardesc(fs, vidx);
  if (!varinreg(vd))
    return NULL; /* no debug info. for constants */
  else {
    int idx = vd->vd.pidx;
    lua_assert(idx < fs->ndebugvars);
    return &fs->f->locvars[idx];
  }
}

/*
** Create an expression representing variable 'vidx'
*/
static void init_var(FuncState *fs, expdesc *e, int vidx) {
  e->f = e->t = NO_JUMP;
  e->k = VLOCAL;
  e->u.var.vidx = cast_short(vidx);
  e->u.var.ridx = getlocalvardesc(fs, vidx)->vd.ridx;
}

/*
** Raises an error if variable described by 'e' is read only; moreover,
** if 'e' is t[exp] where t is the vararg parameter, change it to index
** a real table. (Virtual vararg tables cannot be changed.)
*/
static void check_readonly(LexState *ls, expdesc *e) {
  FuncState *fs = ls->fs;
  TString *varname = NULL; /* to be set if variable is const */
  switch (e->k) {
  case VCONST: {
    varname = ls->dyd->actvar.arr[e->u.info].vd.name;
    break;
  }
  case VLOCAL:
  case VVARGVAR: {
    Vardesc *vardesc = getlocalvardesc(fs, e->u.var.vidx);
    if (vardesc->vd.kind != VDKREG) /* not a regular variable? */
      varname = vardesc->vd.name;
    break;
  }
  case VUPVAL: {
    Upvaldesc *up = &fs->f->upvalues[e->u.info];
    if (up->kind != VDKREG)
      varname = up->name;
    break;
  }
  case VVARGIND: {
    needvatab(fs->f); /* function will need a vararg table */
    e->k = VINDEXED;
  } /* FALLTHROUGH */
  case VINDEXUP:
  case VINDEXSTR:
  case VINDEXED: {   /* global variable */
    if (e->u.ind.ro) /* read-only? */
      varname = tsvalue(&fs->f->k[e->u.ind.keystr]);
    break;
  }
  default:
    lua_assert(e->k == VINDEXI); /* this one doesn't need any check */
    return;                      /* integer index cannot be read-only */
  }
  if (varname)
    luaK_semerror(ls, "attempt to assign to const variable '%s'",
                  getstr(varname));
}

/*
** Start the scope for the last 'nvars' created variables.
*/
static void adjustlocalvars(LexState *ls, int nvars) {
  FuncState *fs = ls->fs;
  int reglevel = luaY_nvarstack(fs);
  int i;
  for (i = 0; i < nvars; i++) {
    int vidx = fs->nactvar++;
    Vardesc *var = getlocalvardesc(fs, vidx);
    var->vd.ridx = cast_byte(reglevel++);
    var->vd.pidx = registerlocalvar(ls, fs, var->vd.name);
    luaY_checklimit(fs, reglevel, MAXVARS, "local variables");
  }
}

/*
** Close the scope for all variables up to level 'tolevel'.
** (debug info.)
*/
static void removevars(FuncState *fs, int tolevel) {
  fs->ls->dyd->actvar.n -= (fs->nactvar - tolevel);
  while (fs->nactvar > tolevel) {
    LocVar *var = localdebuginfo(fs, --fs->nactvar);
    if (var) /* does it have debug information? */
      var->endpc = fs->pc;
  }
}

/*
** Search the upvalues of the function 'fs' for one
** with the given 'name'.
*/
static int searchupvalue(FuncState *fs, TString *name) {
  int i;
  Upvaldesc *up = fs->f->upvalues;
  for (i = 0; i < fs->nups; i++) {
    if (eqstr(up[i].name, name))
      return i;
  }
  return -1; /* not found */
}

static Upvaldesc *allocupvalue(FuncState *fs) {
  Proto *f = fs->f;
  int oldsize = f->sizeupvalues;
  luaY_checklimit(fs, fs->nups + 1, MAXUPVAL, "upvalues");
  luaM_growvector(fs->ls->L, f->upvalues, fs->nups, f->sizeupvalues, Upvaldesc,
                  MAXUPVAL, "upvalues");
  while (oldsize < f->sizeupvalues)
    f->upvalues[oldsize++].name = NULL;
  return &f->upvalues[fs->nups++];
}

static int newupvalue(FuncState *fs, TString *name, expdesc *v) {
  Upvaldesc *up = allocupvalue(fs);
  FuncState *prev = fs->prev;
  if (v->k == VLOCAL) {
    up->instack = 1;
    up->idx = v->u.var.ridx;
    up->kind = getlocalvardesc(prev, v->u.var.vidx)->vd.kind;
    lua_assert(eqstr(name, getlocalvardesc(prev, v->u.var.vidx)->vd.name));
  } else {
    up->instack = 0;
    up->idx = cast_byte(v->u.info);
    up->kind = prev->f->upvalues[v->u.info].kind;
    lua_assert(eqstr(name, prev->f->upvalues[v->u.info].name));
  }
  up->name = name;
  luaC_objbarrier(fs->ls->L, fs->f, name);
  return fs->nups - 1;
}

/*
** Look for an active variable with the name 'n' in the
** function 'fs'. If found, initialize 'var' with it and return
** its expression kind; otherwise return -1. While searching,
** var->u.info==-1 means that the preambular global declaration is
** active (the default while there is no other global declaration);
** var->u.info==-2 means there is no active collective declaration
** (some previous global declaration but no collective declaration);
** and var->u.info>=0 points to the inner-most (the first one found)
** collective declaration, if there is one.
*/
static int searchvar(FuncState *fs, TString *n, expdesc *var) {
  int i;
  for (i = cast_int(fs->nactvar) - 1; i >= 0; i--) {
    Vardesc *vd = getlocalvardesc(fs, i);
    if (varglobal(vd)) {         /* global declaration? */
      if (vd->vd.name == NULL) { /* collective declaration? */
        if (var->u.info < 0)     /* no previous collective declaration? */
          var->u.info = fs->firstlocal + i; /* this is the first one */
      } else {                              /* global name */
        if (eqstr(n, vd->vd.name)) {        /* found? */
          init_exp(var, VGLOBAL, fs->firstlocal + i);
          return VGLOBAL;
        } else if (var->u.info == -1) /* active preambular declaration? */
          var->u.info = -2;           /* invalidate preambular declaration */
      }
    } else if (eqstr(n, vd->vd.name)) { /* found? */
      if (vd->vd.kind == RDKCTC)        /* compile-time constant? */
        init_exp(var, VCONST, fs->firstlocal + i);
      else { /* local variable */
        init_var(fs, var, i);
        if (vd->vd.kind == RDKVAVAR) /* vararg parameter? */
          var->k = VVARGVAR;
      }
      return cast_int(var->k);
    }
  }
  return -1; /* not found */
}

/*
** Mark block where variable at given level was defined
** (to emit close instructions later).
*/
static void markupval(FuncState *fs, int level) {
  BlockCnt *bl = fs->bl;
  while (bl->nactvar > level)
    bl = bl->previous;
  bl->upval = 1;
  fs->needclose = 1;
}

/*
** Mark that current block has a to-be-closed variable.
*/
static void marktobeclosed(FuncState *fs) {
  BlockCnt *bl = fs->bl;
  bl->upval = 1;
  bl->insidetbc = 1;
  fs->needclose = 1;
}

/*
** Find a variable with the given name 'n'. If it is an upvalue, add
** this upvalue into all intermediate functions. If it is a global, set
** 'var' as 'void' as a flag.
*/
static void singlevaraux(FuncState *fs, TString *n, expdesc *var, int base) {
  int v = searchvar(fs, n, var); /* look up variables at current level */
  if (v >= 0) {                  /* found? */
    if (!base) {
      if (var->k == VVARGVAR)      /* vararg parameter? */
        luaK_vapar2local(fs, var); /* change it to a regular local */
      if (var->k == VLOCAL)
        markupval(fs, var->u.var.vidx); /* will be used as an upvalue */
    }
    /* else nothing else to be done */
  } else { /* not found at current level; try upvalues */
    int idx = searchupvalue(fs, n);             /* try existing upvalues */
    if (idx < 0) {                              /* not found? */
      if (fs->prev != NULL)                     /* more levels? */
        singlevaraux(fs->prev, n, var, 0);      /* try upper levels */
      if (var->k == VLOCAL || var->k == VUPVAL) /* local or upvalue? */
        idx = newupvalue(fs, n, var);           /* will be a new upvalue */
      else      /* it is a global or a constant */
        return; /* don't need to do anything at this level */
    }
    init_exp(var, VUPVAL, idx); /* new or old upvalue */
  }
}

static void buildglobal(LexState *ls, TString *varname, expdesc *var) {
  FuncState *fs = ls->fs;
  expdesc key;
  init_exp(var, VGLOBAL, -1);         /* global by default */
  singlevaraux(fs, ls->envn, var, 1); /* get environment variable */
  if (var->k == VGLOBAL)
    luaK_semerror(ls, "_ENV is global when accessing variable '%s'",
                  getstr(varname));
  luaK_exp2anyregup(fs, var);  /* _ENV could be a constant */
  codestring(&key, varname);   /* key is variable name */
  luaK_indexed(fs, var, &key); /* 'var' represents _ENV[varname] */
}

/*
** Find a variable with the given name 'n', handling global variables
** too.
*/
static void buildvar(LexState *ls, TString *varname, expdesc *var) {
  FuncState *fs = ls->fs;
  init_exp(var, VGLOBAL, -1); /* global by default */
  singlevaraux(fs, varname, var, 1);
  if (var->k == VGLOBAL) { /* global name? */
    int info = var->u.info;
    /* global by default in the scope of a global declaration? */
    if (info == -2)
      luaK_semerror(ls, "variable '%s' not declared", getstr(varname));
    buildglobal(ls, varname, var);
    if (info != -1 && ls->dyd->actvar.arr[info].vd.kind == GDKCONST)
      var->u.ind.ro = 1; /* mark variable as read-only */
    else                 /* anyway must be a global */
      lua_assert(info == -1 || ls->dyd->actvar.arr[info].vd.kind == GDKREG);
  }
}

static void singlevar(LexState *ls, expdesc *var) {
  buildvar(ls, str_checkname(ls), var);
}

/*
** Adjust the number of results from an expression list 'e' with 'nexps'
** expressions to 'nvars' values.
*/
static void adjust_assign(LexState *ls, int nvars, int nexps, expdesc *e) {
  FuncState *fs = ls->fs;
  int needed = nvars - nexps; /* extra values needed */
  luaK_checkstack(fs, needed);
  if (hasmultret(e->k)) {   /* last expression has multiple returns? */
    int extra = needed + 1; /* discount last expression itself */
    if (extra < 0)
      extra = 0;
    luaK_setreturns(fs, e, extra); /* last exp. provides the difference */
  } else {
    if (e->k != VVOID)                   /* at least one expression? */
      luaK_exp2nextreg(fs, e);           /* close last expression */
    if (needed > 0)                      /* missing values? */
      luaK_nil(fs, fs->freereg, needed); /* complete with nils */
  }
  if (needed > 0)
    luaK_reserveregs(fs, needed); /* registers for extra values */
  else /* adding 'needed' is actually a subtraction */
    fs->freereg = cast_byte(fs->freereg + needed); /* remove extra values */
}

#define enterlevel(ls) luaE_incCstack(ls->L)

#define leavelevel(ls) ((ls)->L->nCcalls--)

/*
** Generates an error that a goto jumps into the scope of some
** variable declaration.
*/
static l_noret jumpscopeerror(LexState *ls, Labeldesc *gt) {
  TString *tsname = getlocalvardesc(ls->fs, gt->nactvar)->vd.name;
  const char *varname = (tsname != NULL) ? getstr(tsname) : "*";
  luaK_semerror(ls, "<goto %s> at line %d jumps into the scope of '%s'",
                getstr(gt->name), gt->line, varname); /* raise the error */
}

/*
** Closes the goto at index 'g' to given 'label' and removes it
** from the list of pending gotos.
** If it jumps into the scope of some variable, raises an error.
** The goto needs a CLOSE if it jumps out of a block with upvalues,
** or out of the scope of some variable and the block has upvalues
** (signaled by parameter 'bup').
*/
static void closegoto(LexState *ls, int g, Labeldesc *label, int bup) {
  int i;
  FuncState *fs = ls->fs;
  Labellist *gl = &ls->dyd->gt; /* list of gotos */
  Labeldesc *gt = &gl->arr[g];  /* goto to be resolved */
  lua_assert(eqstr(gt->name, label->name));
  if (l_unlikely(gt->nactvar < label->nactvar)) /* enter some scope? */
    jumpscopeerror(ls, gt);
  if (gt->close || (label->nactvar < gt->nactvar && bup)) { /* needs close? */
    lu_byte stklevel = reglevel(fs, label->nactvar);
    /* move jump to CLOSE position */
    fs->f->code[gt->pc + 1] = fs->f->code[gt->pc];
    /* put CLOSE instruction at original position */
    fs->f->code[gt->pc] = CREATE_ABCk(OP_CLOSE, stklevel, 0, 0, 0);
    gt->pc++; /* must point to jump instruction */
  }
  luaK_patchlist(ls->fs, gt->pc, label->pc); /* goto jumps to label */
  for (i = g; i < gl->n - 1; i++)            /* remove goto from pending list */
    gl->arr[i] = gl->arr[i + 1];
  gl->n--;
}

/*
** Search for an active label with the given name, starting at
** index 'ilb' (so that it can search for all labels in current block
** or all labels in current function).
*/
static Labeldesc *findlabel(LexState *ls, TString *name, int ilb) {
  Dyndata *dyd = ls->dyd;
  for (; ilb < dyd->label.n; ilb++) {
    Labeldesc *lb = &dyd->label.arr[ilb];
    if (eqstr(lb->name, name)) /* correct label? */
      return lb;
  }
  return NULL; /* label not found */
}

/*
** Adds a new label/goto in the corresponding list.
*/
static int newlabelentry(LexState *ls, Labellist *l, TString *name, int line,
                         int pc) {
  int n = l->n;
  luaM_growvector(ls->L, l->arr, n, l->size, Labeldesc, SHRT_MAX,
                  "labels/gotos");
  l->arr[n].name = name;
  l->arr[n].line = line;
  l->arr[n].nactvar = ls->fs->nactvar;
  l->arr[n].close = 0;
  l->arr[n].pc = pc;
  l->n = n + 1;
  return n;
}

/*
** Create an entry for the goto and the code for it. As it is not known
** at this point whether the goto may need a CLOSE, the code has a jump
** followed by an CLOSE. (As the CLOSE comes after the jump, it is a
** dead instruction; it works as a placeholder.) When the goto is closed
** against a label, if it needs a CLOSE, the two instructions swap
** positions, so that the CLOSE comes before the jump.
*/
static int newgotoentry(LexState *ls, TString *name, int line) {
  FuncState *fs = ls->fs;
  int pc = luaK_jump(fs);              /* create jump */
  luaK_codeABC(fs, OP_CLOSE, 0, 1, 0); /* spaceholder, marked as dead */
  return newlabelentry(ls, &ls->dyd->gt, name, line, pc);
}

/*
** Create a new label with the given 'name' at the given 'line'.
** 'last' tells whether label is the last non-op statement in its
** block. Solves all pending gotos to this new label and adds
** a close instruction if necessary.
** Returns true iff it added a close instruction.
*/
static void createlabel(LexState *ls, TString *name, int line, int last) {
  FuncState *fs = ls->fs;
  Labellist *ll = &ls->dyd->label;
  int l = newlabelentry(ls, ll, name, line, luaK_getlabel(fs));
  if (last) { /* label is last no-op statement in the block? */
    /* assume that locals are already out of scope */
    ll->arr[l].nactvar = fs->bl->nactvar;
  }
}

/*
** Traverse the pending gotos of the finishing block checking whether
** each match some label of that block. Those that do not match are
** "exported" to the outer block, to be solved there. In particular,
** its 'nactvar' is updated with the level of the inner block,
** as the variables of the inner block are now out of scope.
*/
static void solvegotos(FuncState *fs, BlockCnt *bl) {
  LexState *ls = fs->ls;
  Labellist *gl = &ls->dyd->gt;
  int outlevel = reglevel(fs, bl->nactvar); /* level outside the block */
  int igt = bl->firstgoto; /* first goto in the finishing block */
  while (igt < gl->n) {    /* for each pending goto */
    Labeldesc *gt = &gl->arr[igt];
    /* search for a matching label in the current block */
    Labeldesc *lb = findlabel(ls, gt->name, bl->firstlabel);
    if (lb != NULL)                      /* found a match? */
      closegoto(ls, igt, lb, bl->upval); /* close and remove goto */
    else {                               /* adjust 'goto' for outer block */
      /* block has variables to be closed and goto escapes the scope of
         some variable? */
      if (bl->upval && reglevel(fs, gt->nactvar) > outlevel)
        gt->close = 1;           /* jump may need a close */
      gt->nactvar = bl->nactvar; /* correct level for outer block */
      igt++;                     /* go to next goto */
    }
  }
  ls->dyd->label.n = bl->firstlabel; /* remove local labels */
}

static void enterblock(FuncState *fs, BlockCnt *bl, lu_byte isloop) {
  bl->isloop = isloop;
  bl->nactvar = fs->nactvar;
  bl->firstlabel = fs->ls->dyd->label.n;
  bl->firstgoto = fs->ls->dyd->gt.n;
  bl->upval = 0;
  /* inherit 'insidetbc' from enclosing block */
  bl->insidetbc = (fs->bl != NULL && fs->bl->insidetbc);
  bl->previous = fs->bl; /* link block in function's block list */
  fs->bl = bl;
  lua_assert(fs->freereg == luaY_nvarstack(fs));
}

/*
** generates an error for an undefined 'goto'.
*/
static l_noret undefgoto(LexState *ls, Labeldesc *gt) {
  /* breaks are checked when created, cannot be undefined */
  lua_assert(!eqstr(gt->name, ls->brkn));
  luaK_semerror(ls, "no visible label '%s' for <goto> at line %d",
                getstr(gt->name), gt->line);
}

static void leaveblock(FuncState *fs) {
  BlockCnt *bl = fs->bl;
  LexState *ls = fs->ls;
  lu_byte stklevel = reglevel(fs, bl->nactvar); /* level outside block */
  if (bl->previous && bl->upval)                /* need a 'close'? */
    luaK_codeABC(fs, OP_CLOSE, stklevel, 0, 0);
  fs->freereg = stklevel;                 /* free registers */
  removevars(fs, bl->nactvar);            /* remove block locals */
  lua_assert(bl->nactvar == fs->nactvar); /* back to level on entry */
  if (bl->isloop == 2)                    /* has to fix pending breaks? */
    createlabel(ls, ls->brkn, 0, 0);
  solvegotos(fs, bl);
  if (bl->previous == NULL) {          /* was it the last block? */
    if (bl->firstgoto < ls->dyd->gt.n) /* still pending gotos? */
      undefgoto(ls, &ls->dyd->gt.arr[bl->firstgoto]); /* error */
  }
  fs->bl = bl->previous; /* current block now is previous one */
}

/*
** adds a new prototype into list of prototypes
*/
static Proto *addprototype(LexState *ls) {
  Proto *clp;
  lua_State *L = ls->L;
  FuncState *fs = ls->fs;
  Proto *f = fs->f; /* prototype of current function */
  if (fs->np >= f->sizep) {
    int oldsize = f->sizep;
    luaM_growvector(L, f->p, fs->np, f->sizep, Proto *, MAXARG_Bx, "functions");
    while (oldsize < f->sizep)
      f->p[oldsize++] = NULL;
  }
  f->p[fs->np++] = clp = luaF_newproto(L);
  luaC_objbarrier(L, f, clp);
  return clp;
}

/*
** codes instruction to create new closure in parent function.
** The OP_CLOSURE instruction uses the last available register,
** so that, if it invokes the GC, the GC knows which registers
** are in use at that time.

*/
static void codeclosure(LexState *ls, expdesc *v) {
  FuncState *fs = ls->fs->prev;
  init_exp(v, VRELOC, luaK_codeABx(fs, OP_CLOSURE, 0, fs->np - 1));
  luaK_exp2nextreg(fs, v); /* fix it at the last register */
}

static void open_func(LexState *ls, FuncState *fs, BlockCnt *bl) {
  lua_State *L = ls->L;
  Proto *f = fs->f;
  fs->prev = ls->fs; /* linked list of funcstates */
  fs->ls = ls;
  ls->fs = fs;
  fs->pc = 0;
  fs->previousline = f->linedefined;
  fs->iwthabs = 0;
  fs->lasttarget = 0;
  fs->freereg = 0;
  fs->nk = 0;
  fs->nabslineinfo = 0;
  fs->np = 0;
  fs->nups = 0;
  fs->ndebugvars = 0;
  fs->nactvar = 0;
  fs->needclose = 0;
  fs->firstlocal = ls->dyd->actvar.n;
  fs->firstlabel = ls->dyd->label.n;
  fs->bl = NULL;
  f->source = ls->source;
  luaC_objbarrier(L, f, f->source);
  f->maxstacksize = 2;                  /* registers 0/1 are always valid */
  fs->kcache = luaH_new(L);             /* create table for function */
  sethvalue2s(L, L->top.p, fs->kcache); /* anchor it */
  luaD_inctop(L);
  enterblock(fs, bl, 0);
}

static void close_func(LexState *ls) {
  lua_State *L = ls->L;
  FuncState *fs = ls->fs;
  Proto *f = fs->f;
  luaK_ret(fs, luaY_nvarstack(fs), 0); /* final return */
  leaveblock(fs);
  lua_assert(fs->bl == NULL);
  luaK_finish(fs);
  luaM_shrinkvector(L, f->code, f->sizecode, fs->pc, Instruction);
  luaM_shrinkvector(L, f->lineinfo, f->sizelineinfo, fs->pc, ls_byte);
  luaM_shrinkvector(L, f->abslineinfo, f->sizeabslineinfo, fs->nabslineinfo,
                    AbsLineInfo);
  luaM_shrinkvector(L, f->k, f->sizek, fs->nk, TValue);
  luaM_shrinkvector(L, f->p, f->sizep, fs->np, Proto *);
  luaM_shrinkvector(L, f->locvars, f->sizelocvars, fs->ndebugvars, LocVar);
  luaM_shrinkvector(L, f->upvalues, f->sizeupvalues, fs->nups, Upvaldesc);
  ls->fs = fs->prev;
  L->top.p--; /* pop kcache table */
  luaC_checkGC(L);
}

/*
** {======================================================================
** GRAMMAR RULES
** =======================================================================
*/

/*
** check whether current token is in the follow set of a block.
** 'until' closes syntactical blocks, but do not close scope,
** so it is handled in separate.
*/
static int block_follow(LexState *ls, int withuntil) {
  switch (ls->t.token) {
  case TK_ELSE:
  case TK_ELSEIF:
  case TK_END:
  case TK_EOS:
    return 1;
  case TK_UNTIL:
    return withuntil;
  default:
    return 0;
  }
}

static void statlist(LexState *ls) {
  /* statlist -> { stat [';'] } */
  while (!block_follow(ls, 1)) {
    if (ls->t.token == TK_RETURN) {
      statement(ls);
      return; /* 'return' must be last statement */
    }
    statement(ls);
  }
}

static void fieldsel(LexState *ls, expdesc *v) {
  /* fieldsel -> ['.' | ':'] NAME */
  FuncState *fs = ls->fs;
  expdesc key;
  int line = ls->linenumber;
  LusAstNode *table_ast = v->ast; /* save before overwrite */
  luaK_exp2anyregup(fs, v);
  luaX_next(ls);                     /* skip the dot or colon */
  TString *fname = ls->t.seminfo.ts; /* capture field name */
  codename(ls, &key);
  luaK_indexed(fs, v, &key);
  /* Create AST_FIELD node */
  if (AST_ACTIVE(ls)) {
    LusAstNode *field = lusA_newnode(ls->ast, AST_FIELD, line);
    field->u.index.table = table_ast;
    LusAstNode *keynode = lusA_newnode(ls->ast, AST_STRING, line);
    keynode->u.str = fname;
    field->u.index.key = keynode;
    v->ast = field;
  }
}

static void yindex(LexState *ls, expdesc *v) {
  /* index -> '[' expr ']' */
  luaX_next(ls); /* skip the '[' */
  expr(ls, v);
  LusAstNode *key_ast = v->ast; /* save key AST */
  luaK_exp2val(ls->fs, v);
  checknext(ls, ']');
  /* key_ast stored in v->ast; caller will use it for AST_INDEX */
  v->ast = key_ast;
}

/*
** {======================================================================
** Rules for Constructors
** =======================================================================
*/

typedef struct ConsControl {
  expdesc v;      /* last list item read */
  expdesc *t;     /* table descriptor */
  int nh;         /* total number of 'record' elements */
  int na;         /* number of array elements already stored */
  int tostore;    /* number of array elements pending to be stored */
  int maxtostore; /* maximum number of pending elements */
} ConsControl;

/*
** Maximum number of elements in a constructor, to control the following:
** * counter overflows;
** * overflows in 'extra' for OP_NEWTABLE and OP_SETLIST;
** * overflows when adding multiple returns in OP_SETLIST.
*/
#define MAX_CNST (INT_MAX / 2)
#if MAX_CNST / (MAXARG_vC + 1) > MAXARG_Ax
#undef MAX_CNST
#define MAX_CNST (MAXARG_Ax * (MAXARG_vC + 1))
#endif

static void recfield(LexState *ls, ConsControl *cc) {
  /* recfield -> (NAME | '['exp']') = exp */
  FuncState *fs = ls->fs;
  lu_byte reg = ls->fs->freereg;
  expdesc tab, key, val;
  if (ls->t.token == TK_NAME)
    codename(ls, &key);
  else /* ls->t.token == '[' */
    yindex(ls, &key);
  cc->nh++;
  checknext(ls, '=');
  tab = *cc->t;
  luaK_indexed(fs, &tab, &key);
  expr(ls, &val);
  luaK_storevar(fs, &tab, &val);
  fs->freereg = reg; /* free registers */
}

static void closelistfield(FuncState *fs, ConsControl *cc) {
  lua_assert(cc->tostore > 0);
  luaK_exp2nextreg(fs, &cc->v);
  cc->v.k = VVOID;
  if (cc->tostore >= cc->maxtostore) {
    luaK_setlist(fs, cc->t->u.info, cc->na, cc->tostore); /* flush */
    cc->na += cc->tostore;
    cc->tostore = 0; /* no more items pending */
  }
}

static void lastlistfield(FuncState *fs, ConsControl *cc) {
  if (cc->tostore == 0)
    return;
  if (hasmultret(cc->v.k)) {
    luaK_setmultret(fs, &cc->v);
    luaK_setlist(fs, cc->t->u.info, cc->na, LUA_MULTRET);
    cc->na--; /* do not count last expression (unknown number of elements) */
  } else {
    if (cc->v.k != VVOID)
      luaK_exp2nextreg(fs, &cc->v);
    luaK_setlist(fs, cc->t->u.info, cc->na, cc->tostore);
  }
  cc->na += cc->tostore;
}

static void listfield(LexState *ls, ConsControl *cc) {
  /* listfield -> exp */
  expr(ls, &cc->v);
  cc->tostore++;
}

static void field(LexState *ls, ConsControl *cc) {
  /* field -> listfield | recfield */
  switch (ls->t.token) {
  case TK_NAME: {                  /* may be 'listfield' or 'recfield' */
    if (luaX_lookahead(ls) != '=') /* expression? */
      listfield(ls, cc);
    else
      recfield(ls, cc);
    break;
  }
  case '[': {
    recfield(ls, cc);
    break;
  }
  default: {
    listfield(ls, cc);
    break;
  }
  }
}

/*
** Compute a limit for how many registers a constructor can use before
** emitting a 'SETLIST' instruction, based on how many registers are
** available.
*/
static int maxtostore(FuncState *fs) {
  int numfreeregs = MAX_FSTACK - fs->freereg;
  if (numfreeregs >= 160)     /* "lots" of registers? */
    return numfreeregs / 5;   /* use up to 1/5 of them */
  else if (numfreeregs >= 80) /* still "enough" registers? */
    return 10;                /* one 'SETLIST' instruction for each 10 values */
  else                        /* save registers for potential more nesting */
    return 1;
}

static void constructor(LexState *ls, expdesc *t) {
  /* constructor -> '{' [ field { sep field } [sep] ] '}'
     sep -> ',' | ';' */
  FuncState *fs = ls->fs;
  int line = ls->linenumber;
  int pc = luaK_codevABCk(fs, OP_NEWTABLE, 0, 0, 0, 0);
  ConsControl cc;
  luaK_code(fs, 0); /* space for extra arg. */
  cc.na = cc.nh = cc.tostore = 0;
  cc.t = t;
  init_exp(t, VNONRELOC, fs->freereg); /* table will be at stack top */
  luaK_reserveregs(fs, 1);
  init_exp(&cc.v, VVOID, 0); /* no value (yet) */
  checknext(ls, '{' /*}*/);
  cc.maxtostore = maxtostore(fs);
  do {
    if (ls->t.token == /*{*/ '}')
      break;
    if (cc.v.k != VVOID)       /* is there a previous list item? */
      closelistfield(fs, &cc); /* close it */
    field(ls, &cc);
    luaY_checklimit(fs, cc.tostore + cc.na + cc.nh, MAX_CNST,
                    "items in a constructor");
  } while (testnext(ls, ',') || testnext(ls, ';'));
  check_match(ls, /*{*/ '}', '{' /*}*/, line);
  lastlistfield(fs, &cc);
  luaK_settablesize(fs, pc, t->u.info, cc.na, cc.nh);
  /* Create AST_TABLE node */
  if (AST_ACTIVE(ls)) {
    t->ast = lusA_newnode(ls->ast, AST_TABLE, line);
    /* Fields are not tracked individually for now */
    t->ast->u.table.fields = NULL;
  }
}

/* }====================================================================== */

static void setvararg(FuncState *fs) {
  fs->f->flag |= PF_VAHID; /* by default, use hidden vararg arguments */
  luaK_codeABC(fs, OP_VARARGPREP, 0, 0, 0);
}

static void parlist(LexState *ls) {
  /* parlist -> [ {NAME ','} (NAME | '...') ] */
  FuncState *fs = ls->fs;
  Proto *f = fs->f;
  int nparams = 0;
  int varargk = 0;
  if (ls->t.token != ')') { /* is 'parlist' not empty? */
    do {
      switch (ls->t.token) {
      case TK_NAME: {
        new_localvar(ls, str_checkname(ls));
        nparams++;
        break;
      }
      case TK_DOTS: {
        varargk = 1;
        luaX_next(ls); /* skip '...' */
        if (ls->t.token == TK_NAME)
          new_varkind(ls, str_checkname(ls), RDKVAVAR);
        else
          new_localvarliteral(ls, "(vararg table)");
        break;
      }
      default:
        luaX_syntaxerror(ls, "<name> or '...' expected");
      }
    } while (!varargk && testnext(ls, ','));
  }
  adjustlocalvars(ls, nparams);
  f->numparams = cast_byte(fs->nactvar);
  if (varargk) {
    setvararg(fs);          /* declared vararg */
    adjustlocalvars(ls, 1); /* vararg parameter */
  }
  /* reserve registers for parameters (plus vararg parameter, if present) */
  luaK_reserveregs(fs, fs->nactvar);
}

static void body(LexState *ls, expdesc *e, int ismethod, int line) {
  /* body ->  '(' parlist ')' block END */
  FuncState new_fs;
  BlockCnt bl;
  LusAstNode *saved_curblock = NULL;
  LusAstNode *saved_curnode = NULL;

  new_fs.f = addprototype(ls);
  new_fs.f->linedefined = line;
  open_func(ls, &new_fs, &bl);

  /* Save parent context and set up function body collection */
  if (AST_ACTIVE(ls) && ls->ast->curnode) {
    saved_curblock = ls->ast->curblock;
    saved_curnode = ls->ast->curnode;
    ls->ast->curblock = ls->ast->curnode;
    ls->ast->curnode = NULL; /* clear for nested statements */
  }

  checknext(ls, '(');
  if (ismethod) {
    new_localvarliteral(ls, "self"); /* create 'self' parameter */
    adjustlocalvars(ls, 1);
  }
  parlist(ls);
  checknext(ls, ')');
  statlist(ls);
  new_fs.f->lastlinedefined = ls->linenumber;
  check_match(ls, TK_END, TK_FUNCTION, line);

  /* Restore parent context */
  if (AST_ACTIVE(ls)) {
    ls->ast->curblock = saved_curblock;
    ls->ast->curnode = saved_curnode;
  }

  codeclosure(ls, e);
  close_func(ls);
}

static int explist(LexState *ls, expdesc *v) {
  /* explist -> expr { ',' expr } */
  int n = 1; /* at least one expression */
  LusAstNode *first_expr = NULL;
  LusAstNode *last_expr = NULL;

  expr(ls, v);
  if (AST_ACTIVE(ls) && v->ast) {
    first_expr = last_expr = v->ast;
  }
  while (testnext(ls, ',')) {
    luaK_exp2nextreg(ls->fs, v);
    expr(ls, v);
    if (AST_ACTIVE(ls) && v->ast) {
      if (last_expr) {
        last_expr->next = v->ast;
        last_expr = v->ast;
      } else {
        first_expr = last_expr = v->ast;
      }
    }
    n++;
  }
  /* Store the first expression in v->ast so caller can access full list */
  if (AST_ACTIVE(ls)) {
    v->ast = first_expr;
  }
  return n;
}

static void funcargs(LexState *ls, expdesc *f) {
  FuncState *fs = ls->fs;
  expdesc args;
  int base, nparams;
  int line = ls->linenumber;
  LusAstNode *func_ast = f->ast; /* save function AST before overwrite */
  LusAstNode *args_ast = NULL;

  switch (ls->t.token) {
  case '(': { /* funcargs -> '(' [ explist ] ')' */
    luaX_next(ls);
    if (ls->t.token == ')') { /* arg list is empty? */
      args.k = VVOID;
      args.ast = NULL;
    } else {
      explist(ls, &args);
      args_ast = args.ast;
      if (hasmultret(args.k))
        luaK_setmultret(fs, &args);
    }
    check_match(ls, ')', '(', line);
    break;
  }
  case '{' /*}*/: { /* funcargs -> constructor */
    constructor(ls, &args);
    args_ast = args.ast;
    break;
  }
  case TK_STRING: { /* funcargs -> STRING */
    codestring(&args, ls->t.seminfo.ts);
    if (AST_ACTIVE(ls)) {
      args_ast = lusA_newnode(ls->ast, AST_STRING, line);
      args_ast->u.str = ls->t.seminfo.ts;
    }
    luaX_next(ls); /* must use 'seminfo' before 'next' */
    break;
  }
  default: {
    luaX_syntaxerror(ls, "function arguments expected");
  }
  }
  lua_assert(f->k == VNONRELOC);
  base = f->u.info; /* base register for call */
  if (hasmultret(args.k))
    nparams = LUA_MULTRET; /* open call */
  else {
    if (args.k != VVOID)
      luaK_exp2nextreg(fs, &args); /* close last argument */
    nparams = fs->freereg - (base + 1);
  }
  init_exp(f, VCALL, luaK_codeABC(fs, OP_CALL, base, nparams + 1, 2));
  luaK_fixline(fs, line);

  /* Create call AST node */
  if (AST_ACTIVE(ls)) {
    LusAstNode *call = lusA_newnode(ls->ast, AST_CALLEXPR, line);
    call->u.call.func = func_ast;
    call->u.call.args = args_ast;
    call->u.call.method = NULL;
    f->ast = call;
  }

  /* call removes function and arguments and leaves one result (unless
     changed later) */
  fs->freereg = cast_byte(base + 1);
}

/*
** {======================================================================
** Expression parsing
** =======================================================================
*/

static void primaryexp(LexState *ls, expdesc *v) {
  /* primaryexp -> NAME | '(' expr ')' */
  switch (ls->t.token) {
  case '(': {
    int line = ls->linenumber;
    luaX_next(ls);
    expr(ls, v);
    check_match(ls, ')', '(', line);
    luaK_dischargevars(ls->fs, v);
    return;
  }
  case TK_NAME: {
    int line = ls->linenumber;
    TString *name =
        ls->t.seminfo.ts; /* capture name before singlevar consumes it */
    singlevar(ls, v);
    /* Create AST_NAME node */
    if (AST_ACTIVE(ls)) {
      v->ast = lusA_newnode(ls->ast, AST_NAME, line);
      v->ast->u.str = name;
    }
    return;
  }
  default: {
    luaX_syntaxerror(ls, "unexpected symbol");
  }
  }
}

/*
** Helper: discharge an expression to a specific register.
** Used by optional chaining to keep result in the same register.
** Ensures freereg is properly restored after discharge.
*/
static void discharge2basereg(FuncState *fs, expdesc *e, int basereg) {
  luaK_dischargevars(fs, e);
  if (e->k == VRELOC) {
    /* Set the destination of the pending instruction to basereg */
    Instruction *pc = &fs->f->code[e->u.info];
    SETARG_A(*pc, basereg);
    e->u.info = basereg;
    e->k = VNONRELOC;
  } else if (e->k == VNONRELOC && e->u.info != basereg) {
    /* Move from current register to basereg */
    luaK_codeABC(fs, OP_MOVE, basereg, e->u.info, 0);
    e->u.info = basereg;
  } else if (e->k != VNONRELOC) {
    /* Other constant/value cases: load into basereg */
    switch (e->k) {
    case VNIL:
      luaK_codeABC(fs, OP_LOADNIL, basereg, 0, 0);
      break;
    case VFALSE:
      luaK_codeABC(fs, OP_LOADFALSE, basereg, 0, 0);
      break;
    case VTRUE:
      luaK_codeABC(fs, OP_LOADTRUE, basereg, 0, 0);
      break;
    default:
      /* For other cases, discharge to next reg then move */
      luaK_exp2nextreg(fs, e);
      if (e->u.info != basereg) {
        luaK_codeABC(fs, OP_MOVE, basereg, e->u.info, 0);
        fs->freereg--; /* free the temp register */
      }
      break;
    }
    e->u.info = basereg;
    e->k = VNONRELOC;
  }
  /* Ensure freereg is just past basereg */
  fs->freereg = cast_byte(basereg + 1);
}

static void suffixedexp(LexState *ls, expdesc *v) {
  /* suffixedexp ->
       primaryexp { '?' | '.' NAME | '[' exp ']' | ':' NAME funcargs | funcargs
     } The '?' operator enables optional chaining: if the value is nil/false,
     all subsequent suffix operations short-circuit and return nil.

     Key: all operations in an optional chain write to the SAME register
     (basereg). When we jump (value is nil), the register already contains nil,
     so the result is correct without needing to explicitly load nil. */
  FuncState *fs = ls->fs;
  int niljumps = NO_JUMP; /* list of jumps for nil short-circuit */
  int basereg = -1; /* base register for optional chain (-1 = not in chain) */
  primaryexp(ls, v);
  for (;;) {
    switch (ls->t.token) {
    case '?': {      /* optional chaining marker */
      luaX_next(ls); /* skip '?' */
      /* Ensure value is in a register for testing */
      if (basereg == -1) {
        /* First '?' in chain: put value in a NEW register (not a local!) */
        luaK_exp2nextreg(fs, v); /* allocate fresh register */
        basereg = v->u.info;
      } else {
        /* Subsequent '?': discharge to basereg */
        discharge2basereg(fs, v, basereg);
      }
      /* Generate: if R[basereg] is falsy (nil/false), jump to end
      ** TEST A k: if (not R[A] == k) then pc++
      ** With k=0: if R[A] is truthy, skip next instruction (the JMP)
      ** So if R[A] is nil/false, we DON'T skip, and JMP executes */
      luaK_codeABCk(fs, OP_TEST, basereg, 0, 0, 0);
      luaK_concat(fs, &niljumps, luaK_jump(fs));
      break;
    }
    case '.': { /* fieldsel */
      fieldsel(ls, v);
      /* If in optional chain, discharge to basereg */
      if (basereg != -1)
        discharge2basereg(fs, v, basereg);
      break;
    }
    case '[': { /* '[' exp ']' */
      expdesc key;
      int line = ls->linenumber;
      LusAstNode *table_ast = v->ast; /* save table AST before overwrite */
      luaK_exp2anyregup(fs, v);
      yindex(ls, &key);
      luaK_indexed(fs, v, &key);
      /* Create AST_INDEX node */
      if (AST_ACTIVE(ls)) {
        LusAstNode *idx = lusA_newnode(ls->ast, AST_INDEX, line);
        idx->u.index.table = table_ast;
        idx->u.index.key = key.ast;
        v->ast = idx;
      }
      /* If in optional chain, discharge to basereg */
      if (basereg != -1)
        discharge2basereg(fs, v, basereg);
      break;
    }
    case ':': { /* ':' NAME funcargs */
      expdesc key;
      int line = ls->linenumber;
      LusAstNode *obj_ast = v->ast; /* save object AST */
      TString *method_name = NULL;
      luaX_next(ls);
      method_name = ls->t.seminfo.ts; /* capture method name */
      codename(ls, &key);
      luaK_self(fs, v, &key);
      funcargs(ls, v);
      /* Create AST_METHODCALL node */
      if (AST_ACTIVE(ls)) {
        LusAstNode *mcall = lusA_newnode(ls->ast, AST_METHODCALL, line);
        mcall->u.call.func = obj_ast;
        mcall->u.call.method = method_name;
        mcall->u.call.args = v->ast ? v->ast->u.call.args : NULL;
        v->ast = mcall;
      }
      /* Function call result: if in chain, ensure in basereg */
      if (basereg != -1) {
        luaK_exp2anyreg(fs, v);
        if (v->u.info != basereg) {
          luaK_codeABC(fs, OP_MOVE, basereg, v->u.info, 0);
          v->u.info = basereg;
        }
      }
      break;
    }
    case '(':
    case TK_STRING:
    case '{' /*}*/: { /* funcargs */
      luaK_exp2nextreg(fs, v);
      funcargs(ls, v);
      /* Function call result: if in chain, ensure in basereg */
      if (basereg != -1) {
        luaK_exp2anyreg(fs, v);
        if (v->u.info != basereg) {
          luaK_codeABC(fs, OP_MOVE, basereg, v->u.info, 0);
          v->u.info = basereg;
        }
      }
      break;
    }
    default: {
      /* Patch all nil-jumps to here (end of suffix chain) */
      if (niljumps != NO_JUMP)
        luaK_patchtohere(fs, niljumps);
      return;
    }
    }
  }
}

/*
** Maximum number of names in an enum declaration
*/
#define MAXENUMNAMES 255

/*
** Parse an enum expression: enum NAME { ',' NAME } end
** Creates an EnumRoot with all names, returns the first enum value.
*/
static void enumexpr(LexState *ls, expdesc *v) {
  FuncState *fs = ls->fs;
  lua_State *L = ls->L;
  TString *names[MAXENUMNAMES];
  int nnames = 0;
  int k;
  EnumRoot *root;
  Enum *e;
  TValue tv;

  /* skip 'enum' keyword */
  luaX_next(ls);

  /* parse names */
  do {
    if (nnames >= MAXENUMNAMES)
      luaX_syntaxerror(ls, "too many names in enum");
    names[nnames++] = str_checkname(ls);
  } while (testnext(ls, ','));

  /* expect 'end' */
  check_match(ls, TK_END, TK_ENUM, ls->linenumber);

  /* Create the enum root with all names */
  root = luaE_newroot(L, nnames);
  for (int i = 0; i < nnames; i++) {
    root->names[i] = names[i];
  }

  /* Create the first enum value (index 1) */
  e = luaE_new(L, root, 1);

  /* Add the enum value as a constant */
  setenumvalue(L, &tv, e);
  /* Use addk directly - enums are unique, no caching */
  luaM_growvector(L, fs->f->k, fs->nk, fs->f->sizek, TValue, MAXARG_Ax,
                  "constants");
  setobj(L, &fs->f->k[fs->nk], &tv);
  k = fs->nk++;
  luaC_barrier(L, fs->f, &tv);

  /* Return expression referencing the constant */
  init_exp(v, VK, k);
}

static void simpleexp(LexState *ls, expdesc *v) {
  /* simpleexp -> FLT | INT | STRING | NIL | TRUE | FALSE | ... |
                  constructor | FUNCTION body | CATCH expr | ENUM expr |
     suffixedexp */
  int line = ls->linenumber;
  switch (ls->t.token) {
  case TK_FLT: {
    init_exp(v, VKFLT, 0);
    v->u.nval = ls->t.seminfo.r;
    if (AST_ACTIVE(ls)) {
      v->ast = lusA_newnode(ls->ast, AST_NUMBER, line);
      v->ast->u.num.isint = 0;
      v->ast->u.num.val.n = ls->t.seminfo.r;
    }
    break;
  }
  case TK_INT: {
    init_exp(v, VKINT, 0);
    v->u.ival = ls->t.seminfo.i;
    if (AST_ACTIVE(ls)) {
      v->ast = lusA_newnode(ls->ast, AST_NUMBER, line);
      v->ast->u.num.isint = 1;
      v->ast->u.num.val.i = ls->t.seminfo.i;
    }
    break;
  }
  case TK_STRING: {
    codestring(v, ls->t.seminfo.ts);
    if (AST_ACTIVE(ls)) {
      v->ast = lusA_newnode(ls->ast, AST_STRING, line);
      v->ast->u.str = ls->t.seminfo.ts;
    }
    break;
  }
  case TK_NIL: {
    init_exp(v, VNIL, 0);
    if (AST_ACTIVE(ls)) {
      v->ast = lusA_newnode(ls->ast, AST_NIL, line);
    }
    break;
  }
  case TK_TRUE: {
    init_exp(v, VTRUE, 0);
    if (AST_ACTIVE(ls)) {
      v->ast = lusA_newnode(ls->ast, AST_TRUE, line);
    }
    break;
  }
  case TK_FALSE: {
    init_exp(v, VFALSE, 0);
    if (AST_ACTIVE(ls)) {
      v->ast = lusA_newnode(ls->ast, AST_FALSE, line);
    }
    break;
  }
  case TK_DOTS: { /* vararg */
    FuncState *fs = ls->fs;
    check_condition(ls, isvararg(fs->f),
                    "cannot use '...' outside a vararg function");
    init_exp(v, VVARARG, luaK_codeABC(fs, OP_VARARG, 0, fs->f->numparams, 1));
    if (AST_ACTIVE(ls)) {
      v->ast = lusA_newnode(ls->ast, AST_VARARG, line);
    }
    break;
  }
  case '{' /*}*/: { /* constructor */
    constructor(ls, v);
    return;
  }
  case TK_FUNCTION: {
    luaX_next(ls);
    body(ls, v, 0, ls->linenumber);
    return;
  }
  case TK_CATCH: { /* catch expression */
    catchexpr(ls, v);
    return;
  }
  case TK_ENUM: { /* enum expression */
    enumexpr(ls, v);
    return;
  }
  default: {
    suffixedexp(ls, v);
    return;
  }
  }
  luaX_next(ls);
}

static UnOpr getunopr(int op) {
  switch (op) {
  case TK_NOT:
    return OPR_NOT;
  case '-':
    return OPR_MINUS;
  case '~':
    return OPR_BNOT;
  case '#':
    return OPR_LEN;
  default:
    return OPR_NOUNOPR;
  }
}

/*
** Parse a catch expression: catch <expr>
** Returns two values: status (true/false) and result/error
** Result is placed in registers starting at the current freereg:
**   R[A] = status (true if success, false if error)
**   R[A+1] = result value (if success) or error message (if error)
**
** Generated code structure:
**   OP_CATCH A, offset     -- begin catch block, setup error handler
**   <expression code>      -- evaluates to R[A+1]
**   OP_ENDCATCH A, offset  -- success: R[A] = true, jump past error handler
**   (error path):          -- VM jumps here on error, R[A]=false, R[A+1]=error
**   (continue)
**
** The VM uses setjmp/longjmp for the protected execution. OP_CATCH sets up
** a longjmp target, and if an error occurs during expression evaluation,
** control returns to OP_CATCH which then handles the error path.
*/
static void catchexpr(LexState *ls, expdesc *v) {
  FuncState *fs = ls->fs;
  int base = fs->freereg; /* base register for result */
  int catchpc;
  int endcatchpc;
  int line = ls->linenumber;
  expdesc innerexp;

  luaX_next(ls); /* skip 'catch' */

  /* Reserve register for status (results will go in next registers) */
  luaK_reserveregs(fs, 1);

  /* Emit OP_CATCH with placeholder offset - this sets up the try block */
  catchpc = luaK_codeABx(fs, OP_CATCH, base, 0);

  /* Parse the expression, results go starting at base+1 */
  expr(ls, &innerexp);

  /* Handle inner expression based on type */
  if (hasmultret(innerexp.k)) {
    /* For multi-return (call/vararg), let it return all results.
    ** This will be adjusted by luaK_setreturns if a specific count is needed.
    */
    luaK_setmultret(fs, &innerexp);
  } else {
    /* Single-value expression - put it in base+1 */
    luaK_exp2nextreg(fs, &innerexp);
  }

  /* Emit OP_ENDCATCH: A=base, B=nresults (default 2), C=jump offset
   *(placeholder)
   ** The B field (nresults) will be updated by luaK_setreturns if needed. */
  endcatchpc = luaK_codeABC(fs, OP_ENDCATCH, base, 2, 0);

  /* Fix the OP_CATCH to jump to error path (current position) */
  {
    int offset = fs->pc - (catchpc + 1);
    SETARG_sBx(fs->f->code[catchpc], offset);
  }

  /* Error path is empty in bytecode - VM sets R[A] = false and R[A+1] = error
     and execution continues from here */

  /* Fix OP_ENDCATCH's C field (jump offset) to skip error path */
  {
    int offset = fs->pc - (endcatchpc + 1);
    SETARG_C(fs->f->code[endcatchpc], offset);
  }

  /* Reset freereg to base + 1, like VCALL does. Catch is a multi-return
  ** expression - on success returns (true, ...results), on error (false, err).
  ** The actual number of results depends on context (set via luaK_setreturns).
  */
  fs->freereg = cast_byte(base + 1);

  /* The expression result is VCATCH - a multi-return expression.
  ** Store the ENDCATCH PC in v->u.info so luaK_setreturns can update nresults.
  */
  init_exp(v, VCATCH, endcatchpc);

  luaK_fixline(fs, line);
}

static void catchstat(LexState *ls, int line) {
  FuncState *fs = ls->fs;
  int base = fs->freereg; /* base register for result */
  int catchpc;
  expdesc innerexp;

  luaX_next(ls); /* skip 'catch' */

  /* Reserve register for status (results will go in next registers) */
  luaK_reserveregs(fs, 1);

  /* Emit OP_CATCH with placeholder offset - this sets up the try block */
  catchpc = luaK_codeABx(fs, OP_CATCH, base, 0);

  /* Parse the expression, results go starting at base+1 */
  expr(ls, &innerexp);

  /* Handle inner expression discharge */
  if (hasmultret(innerexp.k)) {
    /* For multi-return (call/vararg), set to return 0 results. */
    luaK_setreturns(fs, &innerexp, 0);
  } else {
    /* Single-value expression - evaluate it (to base+1) but ignore result */
    luaK_exp2nextreg(fs, &innerexp);
  }

  /* Emit OP_ENDCATCH: A=base, B=1 (0 results), C=jump offset (placeholder) */
  luaK_codeABC(fs, OP_ENDCATCH, base, 1, 0);

  /* Fix the OP_CATCH to jump to error path (current position) */
  {
    int offset = fs->pc - (catchpc + 1);
    SETARG_sBx(fs->f->code[catchpc], offset);
  }

  /* Reset freereg - catch statement discards all results */
  fs->freereg = cast_byte(base);

  luaK_fixline(fs, line);
}

static BinOpr getbinopr(int op) {
  switch (op) {
  case '+':
    return OPR_ADD;
  case '-':
    return OPR_SUB;
  case '*':
    return OPR_MUL;
  case '%':
    return OPR_MOD;
  case '^':
    return OPR_POW;
  case '/':
    return OPR_DIV;
  case TK_IDIV:
    return OPR_IDIV;
  case '&':
    return OPR_BAND;
  case '|':
    return OPR_BOR;
  case '~':
    return OPR_BXOR;
  case TK_SHL:
    return OPR_SHL;
  case TK_SHR:
    return OPR_SHR;
  case TK_CONCAT:
    return OPR_CONCAT;
  case TK_NE:
    return OPR_NE;
  case TK_EQ:
    return OPR_EQ;
  case '<':
    return OPR_LT;
  case TK_LE:
    return OPR_LE;
  case '>':
    return OPR_GT;
  case TK_GE:
    return OPR_GE;
  case TK_AND:
    return OPR_AND;
  case TK_OR:
    return OPR_OR;
  default:
    return OPR_NOBINOPR;
  }
}

/*
** Priority table for binary operators.
*/
static const struct {
  lu_byte left;  /* left priority for each binary operator */
  lu_byte right; /* right priority */
} priority[] = {
    /* ORDER OPR */
    {10, 10}, {10, 10},         /* '+' '-' */
    {11, 11}, {11, 11},         /* '*' '%' */
    {14, 13},                   /* '^' (right associative) */
    {11, 11}, {11, 11},         /* '/' '//' */
    {6, 6},   {4, 4},   {5, 5}, /* '&' '|' '~' */
    {7, 7},   {7, 7},           /* '<<' '>>' */
    {9, 8},                     /* '..' (right associative) */
    {3, 3},   {3, 3},   {3, 3}, /* ==, <, <= */
    {3, 3},   {3, 3},   {3, 3}, /* ~=, >, >= */
    {2, 2},   {1, 1}            /* and, or */
};

#define UNARY_PRIORITY 12 /* priority for unary operators */

/*
** subexpr -> (simpleexp | unop subexpr) { binop subexpr }
** where 'binop' is any binary operator with a priority higher than 'limit'
*/
static BinOpr subexpr(LexState *ls, expdesc *v, int limit) {
  BinOpr op;
  UnOpr uop;
  enterlevel(ls);
  uop = getunopr(ls->t.token);
  if (uop != OPR_NOUNOPR) { /* prefix (unary) operator? */
    int line = ls->linenumber;
    LusAstNode *unop_node = NULL;
    luaX_next(ls); /* skip operator */
    subexpr(ls, v, UNARY_PRIORITY);
    luaK_prefix(ls->fs, uop, v, line);
    /* Build unary AST node */
    if (AST_ACTIVE(ls)) {
      unop_node = lusA_newnode(ls->ast, AST_UNOP, line);
      unop_node->u.unop.op = (int)uop;
      unop_node->u.unop.operand = v->ast;
      v->ast = unop_node;
    }
  } else
    simpleexp(ls, v);
  /* expand while operators have priorities higher than 'limit' */
  op = getbinopr(ls->t.token);
  while (op != OPR_NOBINOPR && priority[op].left > limit) {
    expdesc v2;
    BinOpr nextop;
    int line = ls->linenumber;
    LusAstNode *left_ast = v->ast;
    luaX_next(ls); /* skip operator */
    luaK_infix(ls->fs, op, v);
    /* read sub-expression with higher priority */
    nextop = subexpr(ls, &v2, priority[op].right);
    luaK_posfix(ls->fs, op, v, &v2, line);
    /* Build binary AST node */
    if (AST_ACTIVE(ls)) {
      LusAstNode *binop_node = lusA_newnode(ls->ast, AST_BINOP, line);
      binop_node->u.binop.op = (int)op;
      binop_node->u.binop.left = left_ast;
      binop_node->u.binop.right = v2.ast;
      v->ast = binop_node;
    }
    op = nextop;
  }
  leavelevel(ls);
  return op; /* return first untreated operator */
}

static void expr(LexState *ls, expdesc *v) { subexpr(ls, v, 0); }

/* }==================================================================== */

/*
** {======================================================================
** Rules for Statements
** =======================================================================
*/

static void block(LexState *ls) {
  /* block -> statlist */
  FuncState *fs = ls->fs;
  BlockCnt bl;
  enterblock(fs, &bl, 0);
  statlist(ls);
  leaveblock(fs);
}

/*
** structure to chain all variables in the left-hand side of an
** assignment
*/
struct LHS_assign {
  struct LHS_assign *prev;
  expdesc v; /* variable (global, local, upvalue, or indexed) */
};

/*
** check whether, in an assignment to an upvalue/local variable, the
** upvalue/local variable is begin used in a previous assignment to a
** table. If so, save original upvalue/local value in a safe place and
** use this safe copy in the previous assignment.
*/
static void check_conflict(LexState *ls, struct LHS_assign *lh, expdesc *v) {
  FuncState *fs = ls->fs;
  lu_byte extra = fs->freereg; /* eventual position to save local variable */
  int conflict = 0;
  for (; lh; lh = lh->prev) {    /* check all previous assignments */
    if (vkisindexed(lh->v.k)) {  /* assignment to table field? */
      if (lh->v.k == VINDEXUP) { /* is table an upvalue? */
        if (v->k == VUPVAL && lh->v.u.ind.t == v->u.info) {
          conflict = 1; /* table is the upvalue being assigned now */
          lh->v.k = VINDEXSTR;
          lh->v.u.ind.t = extra; /* assignment will use safe copy */
        }
      } else { /* table is a register */
        if (v->k == VLOCAL && lh->v.u.ind.t == v->u.var.ridx) {
          conflict = 1;          /* table is the local being assigned now */
          lh->v.u.ind.t = extra; /* assignment will use safe copy */
        }
        /* is index the local being assigned? */
        if (lh->v.k == VINDEXED && v->k == VLOCAL &&
            lh->v.u.ind.idx == v->u.var.ridx) {
          conflict = 1;
          lh->v.u.ind.idx = extra; /* previous assignment will use safe copy */
        }
      }
    }
  }
  if (conflict) {
    /* copy upvalue/local value to a temporary (in position 'extra') */
    if (v->k == VLOCAL)
      luaK_codeABC(fs, OP_MOVE, extra, v->u.var.ridx, 0);
    else
      luaK_codeABC(fs, OP_GETUPVAL, extra, v->u.info, 0);
    luaK_reserveregs(fs, 1);
  }
}

/* Create code to store the "top" register in 'var' */
static void storevartop(FuncState *fs, expdesc *var) {
  expdesc e;
  init_exp(&e, VNONRELOC, fs->freereg - 1);
  luaK_storevar(fs, var, &e); /* will also free the top register */
}

/*
** Parse and compile a multiple assignment. The first "variable"
** (a 'suffixedexp') was already read by the caller.
**
** assignment -> suffixedexp restassign
** restassign -> ',' suffixedexp restassign | '=' explist
*/
/*
** Handle 'from' table deconstruction in assignments.
** Collects variable names from LHS_assign chain, then assigns t.name to each.
*/
static void assignfrom(LexState *ls, struct LHS_assign *lh, int nvars) {
  FuncState *fs = ls->fs;
  expdesc tbl;
  int tblreg;
  int base;
  struct LHS_assign *cur;
  TString *varnames[MAXVARS];
  int i;

  /* Collect variable names in forward order (chain is reversed) */
  cur = lh;
  for (i = nvars - 1; i >= 0; i--) {
    /* Get the variable name from the LHS expression */
    if (cur->v.k == VLOCAL) {
      varnames[i] = getlocalvardesc(fs, cur->v.u.var.vidx)->vd.name;
    } else if (cur->v.k == VUPVAL) {
      varnames[i] = fs->f->upvalues[cur->v.u.info].name;
    } else if (cur->v.k == VINDEXUP && cur->v.u.ind.keystr) {
      varnames[i] = tsvalue(&fs->f->k[cur->v.u.ind.keystr]);
    } else {
      luaX_syntaxerror(ls, "'from' requires simple variable names");
      return;
    }
    cur = cur->prev;
  }

  /* Reserve registers for field values */
  base = fs->freereg;
  luaK_reserveregs(fs, nvars);

  /* Parse the source table expression into next register (after reserved) */
  expr(ls, &tbl);
  luaK_exp2nextreg(fs, &tbl);
  tblreg = fs->freereg - 1;

  /* Generate GETFIELD for each field into reserved registers */
  for (i = 0; i < nvars; i++) {
    expdesc key, val;
    init_exp(&val, VNONRELOC, tblreg);
    codestring(&key, varnames[i]);
    luaK_indexed(fs, &val, &key);
    luaK_dischargevars(fs, &val);
    {
      Instruction *pc = &getinstruction(fs, &val);
      SETARG_A(*pc, base + i);
    }
  }

  /* Restore freereg to after field values */
  fs->freereg = base + nvars;

  /* Store each value to its variable (in reverse order like restassign) */
  cur = lh;
  for (i = nvars - 1; i >= 0; i--) {
    storevartop(fs, &cur->v);
    cur = cur->prev;
  }
}

/*
** Returns 1 if 'from' was used (and handled all stores), 0 otherwise.
*/
static int restassign(LexState *ls, struct LHS_assign *lh, int nvars) {
  expdesc e;
  check_condition(ls, vkisvar(lh->v.k), "syntax error");
  check_readonly(ls, &lh->v);
  if (testnext(ls, ',')) { /* restassign -> ',' suffixedexp restassign */
    struct LHS_assign nv;
    nv.prev = lh;
    suffixedexp(ls, &nv.v);
    if (!vkisindexed(nv.v.k))
      check_conflict(ls, lh, &nv.v);
    enterlevel(ls); /* control recursion depth */
    if (restassign(ls, &nv, nvars + 1))
      return 1; /* 'from' handled everything, skip storevartop */
    leavelevel(ls);
  } else if (testnext(ls, TK_FROM)) { /* restassign -> 'from' expr */
    assignfrom(ls, lh, nvars);
    return 1; /* signal that 'from' handled all stores */
  } else {    /* restassign -> '=' explist */
    int nexps;
    checknext(ls, '=');
    nexps = explist(ls, &e);
    if (nexps != nvars)
      adjust_assign(ls, nvars, nexps, &e);
    else {
      luaK_setoneret(ls->fs, &e); /* close last expression */
      luaK_storevar(ls->fs, &lh->v, &e);
      return 0; /* avoid default */
    }
  }
  storevartop(ls->fs, &lh->v); /* default assignment */
  return 0;
}

static int cond(LexState *ls, LusAstNode **condast) {
  /* cond -> exp */
  expdesc v;
  expr(ls, &v); /* read condition */
  if (condast)
    *condast = v.ast; /* return AST if requested */
  if (v.k == VNIL)
    v.k = VFALSE; /* 'falses' are all equal here */
  luaK_goiftrue(ls->fs, &v);
  return v.f;
}

static void gotostat(LexState *ls, int line) {
  TString *name = str_checkname(ls); /* label's name */
  newgotoentry(ls, name, line);
}

/*
** Break statement. Semantically equivalent to "goto break".
*/
static void breakstat(LexState *ls, int line) {
  BlockCnt *bl; /* to look for an enclosing loop */
  for (bl = ls->fs->bl; bl != NULL; bl = bl->previous) {
    if (bl->isloop) /* found one? */
      goto ok;
  }
  luaX_syntaxerror(ls, "break outside loop");
ok:
  bl->isloop = 2; /* signal that block has pending breaks */
  luaX_next(ls);  /* skip break */
  newgotoentry(ls, ls->brkn, line);
}

/*
** Check whether there is already a label with the given 'name' at
** current function.
*/
static void checkrepeated(LexState *ls, TString *name) {
  Labeldesc *lb = findlabel(ls, name, ls->fs->firstlabel);
  if (l_unlikely(lb != NULL)) /* already defined? */
    luaK_semerror(ls, "label '%s' already defined on line %d", getstr(name),
                  lb->line); /* error */
}

static void labelstat(LexState *ls, TString *name, int line) {
  /* label -> '::' NAME '::' */
  checknext(ls, TK_DBCOLON); /* skip double colon */
  while (ls->t.token == ';' || ls->t.token == TK_DBCOLON)
    statement(ls);         /* skip other no-op statements */
  checkrepeated(ls, name); /* check for repeated labels */
  createlabel(ls, name, line, block_follow(ls, 0));
}

static void whilestat(LexState *ls, int line) {
  /* whilestat -> WHILE cond DO block END */
  FuncState *fs = ls->fs;
  int whileinit;
  int condexit;
  BlockCnt bl;
  LusAstNode *condast = NULL;
  LusAstNode *saved_curblock = NULL;

  luaX_next(ls); /* skip WHILE */
  whileinit = luaK_getlabel(fs);
  condexit = cond(ls, &condast); /* read condition, capture AST */

  /* Store condition and set up body collection */
  if (AST_ACTIVE(ls) && ls->ast->curnode) {
    ls->ast->curnode->u.loop.cond = condast;
    /* Set curblock to this while node so body statements become children */
    saved_curblock = ls->ast->curblock;
    ls->ast->curblock = ls->ast->curnode;
  }

  enterblock(fs, &bl, 1);
  checknext(ls, TK_DO);
  block(ls);
  luaK_jumpto(fs, whileinit);
  check_match(ls, TK_END, TK_WHILE, line);
  leaveblock(fs);

  /* Restore curblock */
  if (AST_ACTIVE(ls)) {
    ls->ast->curblock = saved_curblock;
  }

  luaK_patchtohere(fs, condexit); /* false conditions finish the loop */
}

static void repeatstat(LexState *ls, int line) {
  /* repeatstat -> REPEAT block UNTIL cond */
  int condexit;
  FuncState *fs = ls->fs;
  int repeat_init = luaK_getlabel(fs);
  BlockCnt bl1, bl2;
  LusAstNode *saved_curblock = NULL;
  LusAstNode *condast = NULL;

  /* Set up body collection if AST active */
  if (AST_ACTIVE(ls) && ls->ast->curnode) {
    saved_curblock = ls->ast->curblock;
    ls->ast->curblock = ls->ast->curnode;
  }

  enterblock(fs, &bl1, 1); /* loop block */
  enterblock(fs, &bl2, 0); /* scope block */
  luaX_next(ls);           /* skip REPEAT */
  statlist(ls);
  check_match(ls, TK_UNTIL, TK_REPEAT, line);
  condexit = cond(ls, &condast); /* read condition, capture AST */

  /* Store condition in curnode */
  if (AST_ACTIVE(ls) && ls->ast->curnode) {
    ls->ast->curnode->u.loop.cond = condast;
  }

  leaveblock(fs);                   /* finish scope */
  if (bl2.upval) {                  /* upvalues? */
    int exit = luaK_jump(fs);       /* normal exit must jump over fix */
    luaK_patchtohere(fs, condexit); /* repetition must close upvalues */
    luaK_codeABC(fs, OP_CLOSE, reglevel(fs, bl2.nactvar), 0, 0);
    condexit = luaK_jump(fs);   /* repeat after closing upvalues */
    luaK_patchtohere(fs, exit); /* normal exit comes to here */
  }
  luaK_patchlist(fs, condexit, repeat_init); /* close the loop */
  leaveblock(fs);                            /* finish loop */

  /* Restore curblock */
  if (AST_ACTIVE(ls)) {
    ls->ast->curblock = saved_curblock;
  }
}

/*
** Read an expression and generate code to put its results in next
** stack slot.
**
*/
static void exp1(LexState *ls) {
  expdesc e;
  expr(ls, &e);
  luaK_exp2nextreg(ls->fs, &e);
  lua_assert(e.k == VNONRELOC);
}

/*
** Fix for instruction at position 'pc' to jump to 'dest'.
** (Jump addresses are relative in Lua). 'back' true means
** a back jump.
*/
static void fixforjump(FuncState *fs, int pc, int dest, int back) {
  Instruction *jmp = &fs->f->code[pc];
  int offset = dest - (pc + 1);
  if (back)
    offset = -offset;
  if (l_unlikely(offset > MAXARG_Bx))
    luaX_syntaxerror(fs->ls, "control structure too long");
  SETARG_Bx(*jmp, offset);
}

/*
** Generate code for a 'for' loop.
*/
static void forbody(LexState *ls, int base, int line, int nvars, int isgen) {
  /* forbody -> DO block */
  static const OpCode forprep[2] = {OP_FORPREP, OP_TFORPREP};
  static const OpCode forloop[2] = {OP_FORLOOP, OP_TFORLOOP};
  BlockCnt bl;
  FuncState *fs = ls->fs;
  int prep, endfor;
  LusAstNode *saved_curblock = NULL;

  /* Set up body collection if AST active */
  if (AST_ACTIVE(ls) && ls->ast->curnode) {
    saved_curblock = ls->ast->curblock;
    ls->ast->curblock = ls->ast->curnode;
  }

  checknext(ls, TK_DO);
  prep = luaK_codeABx(fs, forprep[isgen], base, 0);
  fs->freereg--; /* both 'forprep' remove one register from the stack */
  enterblock(fs, &bl, 0); /* scope for declared variables */
  adjustlocalvars(ls, nvars);
  luaK_reserveregs(fs, nvars);
  block(ls);
  leaveblock(fs); /* end of scope for declared variables */
  fixforjump(fs, prep, luaK_getlabel(fs), 0);
  if (isgen) { /* generic for? */
    luaK_codeABC(fs, OP_TFORCALL, base, 0, nvars);
    luaK_fixline(fs, line);
  }
  endfor = luaK_codeABx(fs, forloop[isgen], base, 0);
  fixforjump(fs, endfor, prep + 1, 1);
  luaK_fixline(fs, line);

  /* Restore curblock */
  if (AST_ACTIVE(ls)) {
    ls->ast->curblock = saved_curblock;
  }
}

static void fornum(LexState *ls, TString *varname, int line) {
  /* fornum -> NAME = exp,exp[,exp] forbody */
  FuncState *fs = ls->fs;
  int base = fs->freereg;
  new_localvarliteral(ls, "(for state)");
  new_localvarliteral(ls, "(for state)");
  new_varkind(ls, varname, RDKCONST); /* control variable */
  checknext(ls, '=');
  exp1(ls); /* initial value */
  checknext(ls, ',');
  exp1(ls); /* limit */
  if (testnext(ls, ','))
    exp1(ls); /* optional step */
  else {      /* default step = 1 */
    luaK_int(fs, fs->freereg, 1);
    luaK_reserveregs(fs, 1);
  }
  adjustlocalvars(ls, 2); /* start scope for internal variables */
  forbody(ls, base, line, 1, 0);
}

static void forlist(LexState *ls, TString *indexname) {
  /* forlist -> NAME {,NAME} IN explist forbody */
  FuncState *fs = ls->fs;
  expdesc e;
  int nvars = 4; /* function, state, closing, control */
  int line;
  int base = fs->freereg;
  /* create internal variables */
  new_localvarliteral(ls, "(for state)"); /* iterator function */
  new_localvarliteral(ls, "(for state)"); /* state */
  new_localvarliteral(ls, "(for state)"); /* closing var. (after swap) */
  new_varkind(ls, indexname, RDKCONST);   /* control variable */
  /* other declared variables */
  while (testnext(ls, ',')) {
    new_localvar(ls, str_checkname(ls));
    nvars++;
  }
  checknext(ls, TK_IN);
  line = ls->linenumber;
  adjust_assign(ls, 4, explist(ls, &e), &e);
  adjustlocalvars(ls, 3); /* start scope for internal variables */
  marktobeclosed(fs);     /* last internal var. must be closed */
  luaK_checkstack(fs, 2); /* extra space to call iterator */
  forbody(ls, base, line, nvars - 3, 1);
}

static void forstat(LexState *ls, int line) {
  /* forstat -> FOR (fornum | forlist) END */
  FuncState *fs = ls->fs;
  TString *varname;
  BlockCnt bl;
  enterblock(fs, &bl, 1);      /* scope for loop and control variables */
  luaX_next(ls);               /* skip 'for' */
  varname = str_checkname(ls); /* first variable name */
  switch (ls->t.token) {
  case '=':
    fornum(ls, varname, line);
    break;
  case ',':
  case TK_IN:
    forlist(ls, varname);
    break;
  default:
    luaX_syntaxerror(ls, "'=' or 'in' expected");
  }
  check_match(ls, TK_END, TK_FOR, line);
  leaveblock(fs); /* loop scope ('break' jumps to this point) */
}

/*
** Check if condition is an assignment pattern: NAME followed by '=' or ','
** This distinguishes 'if a = 1 then' from 'if a == 1 then'
*/
static int isassigncond(LexState *ls) {
  if (ls->t.token == TK_NAME) {
    int lookahead = luaX_lookahead(ls);
    return (lookahead == '=' || lookahead == ',');
  }
  return 0;
}

/*
** Parse an assignment condition: NAME { ',' NAME } '=' explist
** Returns the false-jump list (jump when any variable is falsy).
** Variables are declared in the current scope (should be the outer if block).
*/
static int assigncond(LexState *ls) {
  FuncState *fs = ls->fs;
  int nvars = 0;
  int nexps;
  int base;
  expdesc e;
  int falsejump = NO_JUMP;
  int i;

  base = fs->freereg; /* remember base register for variables */

  /* Parse variable names */
  do {
    TString *vname = str_checkname(ls);
    new_localvar(ls, vname);
    nvars++;
  } while (testnext(ls, ','));

  checknext(ls, '=');

  /* Parse expressions */
  nexps = explist(ls, &e);
  adjust_assign(ls, nvars, nexps, &e);

  /* Activate variables (they now have values in registers base..base+nvars-1)
   */
  adjustlocalvars(ls, nvars);

  /* Generate test for each variable: if any is false/nil, jump to false path */
  for (i = 0; i < nvars; i++) {
    expdesc v;
    /* Create expression for this variable's register */
    init_exp(&v, VNONRELOC, base + i);
    luaK_goiftrue(fs, &v);            /* generates test, adds to v.f on false */
    luaK_concat(fs, &falsejump, v.f); /* accumulate false jumps */
  }

  return falsejump;
}

static void test_then_block(LexState *ls, int *escapelist) {
  /* test_then_block -> [IF | ELSEIF] cond THEN block */
  /* cond can be: expr | NAME {',' NAME} '=' explist (assignment condition) */
  FuncState *fs = ls->fs;
  int condexit;
  luaX_next(ls); /* skip IF or ELSEIF */

  /* Check if this is an assignment condition */
  if (isassigncond(ls)) {
    condexit = assigncond(ls); /* parse assignment and generate tests */
  } else {
    condexit = cond(ls, NULL); /* read normal condition */
  }

  checknext(ls, TK_THEN);
  block(ls); /* 'then' part */
  if (ls->t.token == TK_ELSE ||
      ls->t.token == TK_ELSEIF) /* followed by 'else'/'elseif'? */
    luaK_concat(fs, escapelist, luaK_jump(fs)); /* must jump over it */
  luaK_patchtohere(fs, condexit);
}

static void ifstat(LexState *ls, int line) {
  /* ifstat -> IF cond THEN block {ELSEIF cond THEN block} [ELSE block] END */
  /* With assignment conditions, variables declared in any condition are
  ** visible in all subsequent blocks (then, elseif, else). We create an
  ** outer block to hold these variables. */
  FuncState *fs = ls->fs;
  BlockCnt bl;
  int escapelist = NO_JUMP; /* exit list for finished parts */
  LusAstNode *saved_curblock = NULL;

  /* Set up body collection if AST active */
  if (AST_ACTIVE(ls) && ls->ast->curnode) {
    saved_curblock = ls->ast->curblock;
    ls->ast->curblock = ls->ast->curnode;
  }

  /* Enter outer block for condition variables */
  enterblock(fs, &bl, 0);

  test_then_block(ls, &escapelist); /* IF cond THEN block */
  while (ls->t.token == TK_ELSEIF)
    test_then_block(ls, &escapelist); /* ELSEIF cond THEN block */
  if (testnext(ls, TK_ELSE))
    block(ls); /* 'else' part */
  check_match(ls, TK_END, TK_IF, line);
  luaK_patchtohere(fs, escapelist); /* patch escape list to 'if' end */

  /* Leave outer block (closes condition variable scopes) */
  leaveblock(fs);

  /* Restore curblock */
  if (AST_ACTIVE(ls)) {
    ls->ast->curblock = saved_curblock;
  }
}

static void localfunc(LexState *ls) {
  expdesc b;
  FuncState *fs = ls->fs;
  int fvar = fs->nactvar;              /* function's variable index */
  new_localvar(ls, str_checkname(ls)); /* new local variable */
  adjustlocalvars(ls, 1);              /* enter its scope */
  body(ls, &b, 0, ls->linenumber);     /* function created in next register */
  /* debug information will only see the variable after this point! */
  localdebuginfo(fs, fvar)->startpc = fs->pc;
}

static lu_byte getvarattribute(LexState *ls, lu_byte df) {
  /* attrib -> ['<' NAME '>'] */
  if (testnext(ls, '<')) {
    TString *ts = str_checkname(ls);
    const char *attr = getstr(ts);
    checknext(ls, '>');
    if (strcmp(attr, "const") == 0)
      return RDKCONST; /* read-only variable */
    else if (strcmp(attr, "close") == 0)
      return RDKTOCLOSE; /* to-be-closed variable */
    else
      luaK_semerror(ls, "unknown attribute '%s'", attr);
  }
  return df; /* return default value */
}

static void checktoclose(FuncState *fs, int level) {
  if (level != -1) { /* is there a to-be-closed variable? */
    marktobeclosed(fs);
    luaK_codeABC(fs, OP_TBC, reglevel(fs, level), 0, 0);
  }
}

/*
** Handle 'from' table deconstruction for local variables.
** 'local a, b, c from tbl' is equivalent to 'local a, b, c = tbl.a, tbl.b,
*tbl.c'
** Variables must have been predeclared. 'firstidx' is the index of the first
** variable, 'nvars' is the count.
*/
static void localfrom(LexState *ls, int firstidx, int nvars) {
  FuncState *fs = ls->fs;
  expdesc tbl;
  int base = fs->freereg; /* base register for local variables */
  int tblreg;
  int i;

  /* Reserve registers for local variables first */
  luaK_reserveregs(fs, nvars);

  /* Parse the source table expression and put in temp register */
  expr(ls, &tbl);
  luaK_exp2nextreg(fs, &tbl);
  tblreg = fs->freereg - 1; /* table is now in this register */

  /* For each variable, access the corresponding field */
  for (i = 0; i < nvars; i++) {
    Vardesc *vd = getlocalvardesc(fs, firstidx + i);
    TString *fieldname = vd->vd.name;
    expdesc key, val;

    /* Create indexed expression for tbl.fieldname */
    init_exp(&val, VNONRELOC, tblreg);
    codestring(&key, fieldname);
    luaK_indexed(fs, &val, &key);

    /* Discharge to the variable's register */
    luaK_dischargevars(fs, &val);
    /* Now val is VRELOC with the GETFIELD instruction */
    /* Set the destination register to the variable's slot */
    {
      Instruction *pc = &getinstruction(fs, &val);
      SETARG_A(*pc, base + i);
    }
  }

  /* Restore freereg to after local variables (remove temp table register) */
  fs->freereg = base + nvars;

  /* Activate variables */
  adjustlocalvars(ls, nvars);
}

static void localstat(LexState *ls) {
  /* stat -> LOCAL NAME attrib { ',' NAME attrib } ['=' explist | 'from' expr]
   */
  FuncState *fs = ls->fs;
  int toclose = -1; /* index of to-be-closed variable (if any) */
  Vardesc *var;     /* last variable */
  int vidx;         /* index of last variable */
  int firstidx;     /* index of first variable */
  int nvars = 0;
  int nexps;
  expdesc e;
  LusAstNode *names_head = NULL;
  LusAstNode *names_tail = NULL;

  /* get prefixed attribute (if any); default is regular local variable */
  lu_byte defkind = getvarattribute(ls, VDKREG);
  do {                                           /* for each variable */
    TString *vname = str_checkname(ls);          /* get its name */
    lu_byte kind = getvarattribute(ls, defkind); /* postfixed attribute */
    vidx = new_varkind(ls, vname, kind);         /* predeclare it */

    /* Build AST name list */
    if (AST_ACTIVE(ls)) {
      LusAstNode *name_node = lusA_newnode(ls->ast, AST_NAME, ls->linenumber);
      name_node->u.str = vname;
      if (names_tail) {
        names_tail->next = name_node;
        names_tail = name_node;
      } else {
        names_head = names_tail = name_node;
      }
    }

    if (nvars == 0)
      firstidx = vidx;        /* remember first variable's index */
    if (kind == RDKTOCLOSE) { /* to-be-closed? */
      if (toclose != -1)      /* one already present? */
        luaK_semerror(ls, "multiple to-be-closed variables in local list");
      toclose = fs->nactvar + nvars;
    }
    nvars++;
  } while (testnext(ls, ','));

  if (testnext(ls, TK_FROM)) { /* table deconstruction? */
    if (toclose != -1)
      luaK_semerror(ls, "cannot use 'from' with to-be-closed variables");
    localfrom(ls, firstidx, nvars);
    /* Mark as from syntax in AST */
    if (AST_ACTIVE(ls) && ls->ast->curnode) {
      ls->ast->curnode->u.decl.names = names_head;
      ls->ast->curnode->u.decl.isfrom = 1;
    }
  } else if (testnext(ls, '=')) { /* initialization? */
    nexps = explist(ls, &e);
    /* Store expression AST in statement node */
    if (AST_ACTIVE(ls) && ls->ast->curnode) {
      ls->ast->curnode->u.decl.names = names_head;
      ls->ast->curnode->u.decl.values = e.ast;
      ls->ast->curnode->u.decl.isfrom = 0;
    }
    var = getlocalvardesc(fs, vidx);       /* retrieve last variable */
    if (nvars == nexps &&                  /* no adjustments? */
        var->vd.kind == RDKCONST &&        /* last variable is const? */
        luaK_exp2const(fs, &e, &var->k)) { /* compile-time constant? */
      var->vd.kind = RDKCTC;          /* variable is a compile-time constant */
      adjustlocalvars(ls, nvars - 1); /* exclude last variable */
      fs->nactvar++;                  /* but count it */
    } else {
      adjust_assign(ls, nvars, nexps, &e);
      adjustlocalvars(ls, nvars);
    }
  } else {
    e.k = VVOID;
    nexps = 0;
    /* Store names in AST (no values) */
    if (AST_ACTIVE(ls) && ls->ast->curnode) {
      ls->ast->curnode->u.decl.names = names_head;
      ls->ast->curnode->u.decl.values = NULL;
      ls->ast->curnode->u.decl.isfrom = 0;
    }
    adjust_assign(ls, nvars, nexps, &e);
    adjustlocalvars(ls, nvars);
  }
  checktoclose(fs, toclose);
}

static lu_byte getglobalattribute(LexState *ls, lu_byte df) {
  lu_byte kind = getvarattribute(ls, df);
  switch (kind) {
  case RDKTOCLOSE:
    luaK_semerror(ls, "global variables cannot be to-be-closed");
    return kind; /* to avoid warnings */
  case RDKCONST:
    return GDKCONST; /* adjust kind for global variable */
  default:
    return kind;
  }
}

static void checkglobal(LexState *ls, TString *varname, int line) {
  FuncState *fs = ls->fs;
  expdesc var;
  int k;
  buildglobal(ls, varname, &var); /* create global variable in 'var' */
  k = var.u.ind.keystr;           /* index of global name in 'k' */
  luaK_codecheckglobal(fs, &var, k, line);
}

/*
** Recursively traverse list of globals to be initalized. When
** going, generate table description for the global. In the end,
** after all indices have been generated, read list of initializing
** expressions. When returning, generate the assignment of the value on
** the stack to the corresponding table description. 'n' is the variable
** being handled, range [0, nvars - 1].
*/
static void initglobal(LexState *ls, int nvars, int firstidx, int n, int line) {
  if (n == nvars) { /* traversed all variables? */
    expdesc e;
    int nexps = explist(ls, &e); /* read list of expressions */
    adjust_assign(ls, nvars, nexps, &e);
  } else { /* handle variable 'n' */
    FuncState *fs = ls->fs;
    expdesc var;
    TString *varname = getlocalvardesc(fs, firstidx + n)->vd.name;
    buildglobal(ls, varname, &var); /* create global variable in 'var' */
    enterlevel(ls);                 /* control recursion depth */
    initglobal(ls, nvars, firstidx, n + 1, line);
    leavelevel(ls);
    checkglobal(ls, varname, line);
    storevartop(fs, &var);
  }
}

/*
** Handle 'from' table deconstruction for global variables.
** 'global a, b, c from tbl' is equivalent to 'global a, b, c = tbl.a, tbl.b,
*tbl.c'
** Recursively traverses the variable list, building global table descriptions,
** evaluates all field accesses at the base case, then assigns on the way back.
*/
static void globalfrom(LexState *ls, int nvars, int firstidx, int n, int line) {
  if (n == nvars) {
    /* Base case: parse table and access all fields */
    FuncState *fs = ls->fs;
    expdesc tbl;
    int tblreg;
    int base;
    int i;

    /* Reserve registers for field values first */
    base = fs->freereg;
    luaK_reserveregs(fs, nvars);

    /* Parse table expression into next register (after reserved slots) */
    expr(ls, &tbl);
    luaK_exp2nextreg(fs, &tbl);
    tblreg = fs->freereg - 1;

    /* Generate GETFIELD for each field directly into reserved registers */
    for (i = 0; i < nvars; i++) {
      TString *fieldname = getlocalvardesc(fs, firstidx + i)->vd.name;
      expdesc key, val;

      init_exp(&val, VNONRELOC, tblreg);
      codestring(&key, fieldname);
      luaK_indexed(fs, &val, &key);
      /* Discharge and set destination to reserved register */
      luaK_dischargevars(fs, &val);
      {
        Instruction *pc = &getinstruction(fs, &val);
        SETARG_A(*pc, base + i);
      }
    }

    /* Restore freereg: drop table register, keep field values */
    fs->freereg = base + nvars;
  } else {
    FuncState *fs = ls->fs;
    expdesc var;
    TString *varname = getlocalvardesc(fs, firstidx + n)->vd.name;

    /* Build global variable target */
    buildglobal(ls, varname, &var);

    /* Recurse to handle remaining variables and evaluate expressions */
    enterlevel(ls);
    globalfrom(ls, nvars, firstidx, n + 1, line);
    leavelevel(ls);

    /* Now store the value from stack to global */
    checkglobal(ls, varname, line);
    storevartop(fs, &var);
  }
}

static void globalnames(LexState *ls, lu_byte defkind) {
  FuncState *fs = ls->fs;
  int nvars = 0;
  int lastidx; /* index of last registered variable */
  do {         /* for each name */
    TString *vname = str_checkname(ls);
    lu_byte kind = getglobalattribute(ls, defkind);
    lastidx = new_varkind(ls, vname, kind);
    nvars++;
  } while (testnext(ls, ','));
  if (testnext(ls, TK_FROM)) { /* table deconstruction? */
    globalfrom(ls, nvars, lastidx - nvars + 1, 0, ls->linenumber);
  } else if (testnext(ls, '=')) /* initialization? */
    initglobal(ls, nvars, lastidx - nvars + 1, 0, ls->linenumber);
  fs->nactvar = cast_short(fs->nactvar + nvars); /* activate declaration */
}

static void globalstat(LexState *ls) {
  /* globalstat -> (GLOBAL) attrib '*'
     globalstat -> (GLOBAL) attrib NAME attrib {',' NAME attrib} */
  FuncState *fs = ls->fs;
  /* get prefixed attribute (if any); default is regular global variable */
  lu_byte defkind = getglobalattribute(ls, GDKREG);
  if (!testnext(ls, '*'))
    globalnames(ls, defkind);
  else {
    /* use NULL as name to represent '*' entries */
    new_varkind(ls, NULL, defkind);
    fs->nactvar++; /* activate declaration */
  }
}

static void globalfunc(LexState *ls, int line) {
  /* globalfunc -> (GLOBAL FUNCTION) NAME body */
  expdesc var, b;
  FuncState *fs = ls->fs;
  TString *fname = str_checkname(ls);
  new_varkind(ls, fname, GDKREG); /* declare global variable */
  fs->nactvar++;                  /* enter its scope */
  buildglobal(ls, fname, &var);
  body(ls, &b, 0, ls->linenumber); /* compile and return closure in 'b' */
  checkglobal(ls, fname, line);
  luaK_storevar(fs, &var, &b);
  luaK_fixline(fs, line); /* definition "happens" in the first line */
}

static void globalstatfunc(LexState *ls, int line) {
  /* stat -> GLOBAL globalfunc | GLOBAL globalstat */
  luaX_next(ls); /* skip 'global' */
  if (testnext(ls, TK_FUNCTION))
    globalfunc(ls, line);
  else
    globalstat(ls);
}

static int funcname(LexState *ls, expdesc *v) {
  /* funcname -> NAME {fieldsel} [':' NAME] */
  int ismethod = 0;
  singlevar(ls, v);
  while (ls->t.token == '.')
    fieldsel(ls, v);
  if (ls->t.token == ':') {
    ismethod = 1;
    fieldsel(ls, v);
  }
  return ismethod;
}

static void funcstat(LexState *ls, int line) {
  /* funcstat -> FUNCTION funcname body */
  int ismethod;
  expdesc v, b;
  luaX_next(ls); /* skip FUNCTION */
  ismethod = funcname(ls, &v);
  check_readonly(ls, &v);
  body(ls, &b, ismethod, line);
  luaK_storevar(ls->fs, &v, &b);
  luaK_fixline(ls->fs, line); /* definition "happens" in the first line */
}

static void exprstat(LexState *ls) {
  /* stat -> func | assignment */
  FuncState *fs = ls->fs;
  struct LHS_assign v;
  suffixedexp(ls, &v.v);
  if (ls->t.token == '=' || ls->t.token == ',' ||
      ls->t.token == TK_FROM) { /* stat -> assignment ? */
    /* Store LHS AST for assignment */
    if (AST_ACTIVE(ls) && ls->ast->curnode) {
      ls->ast->curnode->u.assign.lhs = v.v.ast;
    }
    v.prev = NULL;
    restassign(ls, &v, 1);
  } else { /* stat -> func */
    Instruction *inst;
    check_condition(ls, v.v.k == VCALL, "syntax error");
    inst = &getinstruction(fs, &v.v);
    SETARG_C(*inst, 1); /* call statement uses no results */
    /* Store call AST - change node type to CALLSTAT */
    if (AST_ACTIVE(ls) && ls->ast->curnode) {
      ls->ast->curnode->type = AST_CALLSTAT;
      ls->ast->curnode->u.call.func = v.v.ast ? v.v.ast->u.call.func : NULL;
      ls->ast->curnode->u.call.args = v.v.ast ? v.v.ast->u.call.args : NULL;
      ls->ast->curnode->u.call.method = NULL;
    }
  }
}

static void retstat(LexState *ls) {
  /* stat -> RETURN [explist] [';'] */
  FuncState *fs = ls->fs;
  expdesc e;
  int nret;                       /* number of values being returned */
  int first = luaY_nvarstack(fs); /* first slot to be returned */
  if (block_follow(ls, 1) || ls->t.token == ';')
    nret = 0; /* return no values */
  else {
    nret = explist(ls, &e); /* optional return values */
    if (hasmultret(e.k)) {
      luaK_setmultret(fs, &e);
      if (e.k == VCALL && nret == 1 && !fs->bl->insidetbc) { /* tail call? */
        SET_OPCODE(getinstruction(fs, &e), OP_TAILCALL);
        lua_assert(GETARG_A(getinstruction(fs, &e)) == luaY_nvarstack(fs));
      }
      nret = LUA_MULTRET; /* return all values */
    } else {
      if (nret == 1)                     /* only one single value? */
        first = luaK_exp2anyreg(fs, &e); /* can use original slot */
      else { /* values must go to the top of the stack */
        luaK_exp2nextreg(fs, &e);
        lua_assert(nret == fs->freereg - first);
      }
    }
  }
  luaK_ret(fs, first, nret);
  testnext(ls, ';'); /* skip optional semicolon */
}

static void statement(LexState *ls) {
  int line = ls->linenumber; /* may be needed for error messages */
  LusAstNode *node = NULL;   /* AST node for this statement */
  enterlevel(ls);
  switch (ls->t.token) {
  case ';': {      /* stat -> ';' (empty statement) */
    luaX_next(ls); /* skip ';' */
    break;
  }
  case TK_IF: { /* stat -> ifstat */
    if (AST_ACTIVE(ls)) {
      node = lusA_newnode(ls->ast, AST_IF, line);
      ls->ast->curnode = node;
    }
    ifstat(ls, line);
    if (AST_ACTIVE(ls))
      ls->ast->curnode = NULL;
    break;
  }
  case TK_WHILE: { /* stat -> whilestat */
    if (AST_ACTIVE(ls)) {
      node = lusA_newnode(ls->ast, AST_WHILE, line);
      ls->ast->curnode = node;
    }
    whilestat(ls, line);
    if (AST_ACTIVE(ls))
      ls->ast->curnode = NULL;
    break;
  }
  case TK_DO: { /* stat -> DO block END */
    if (AST_ACTIVE(ls))
      node = lusA_newnode(ls->ast, AST_DO, line);
    luaX_next(ls); /* skip DO */
    block(ls);
    check_match(ls, TK_END, TK_DO, line);
    break;
  }
  case TK_FOR: { /* stat -> forstat */
    /* forstat will determine if it's fornum or forgen */
    if (AST_ACTIVE(ls)) {
      node = lusA_newnode(ls->ast, AST_FORNUM, line);
      ls->ast->curnode = node;
    }
    forstat(ls, line);
    if (AST_ACTIVE(ls))
      ls->ast->curnode = NULL;
    break;
  }
  case TK_REPEAT: { /* stat -> repeatstat */
    if (AST_ACTIVE(ls)) {
      node = lusA_newnode(ls->ast, AST_REPEAT, line);
      ls->ast->curnode = node;
    }
    repeatstat(ls, line);
    if (AST_ACTIVE(ls))
      ls->ast->curnode = NULL;
    break;
  }
  case TK_FUNCTION: { /* stat -> funcstat */
    if (AST_ACTIVE(ls)) {
      node = lusA_newnode(ls->ast, AST_FUNCSTAT, line);
      ls->ast->curnode = node;
    }
    funcstat(ls, line);
    if (AST_ACTIVE(ls))
      ls->ast->curnode = NULL;
    break;
  }
  case TK_LOCAL: {                   /* stat -> localstat */
    luaX_next(ls);                   /* skip LOCAL */
    if (testnext(ls, TK_FUNCTION)) { /* local function? */
      if (AST_ACTIVE(ls)) {
        node = lusA_newnode(ls->ast, AST_LOCALFUNC, line);
        ls->ast->curnode = node;
      }
      localfunc(ls);
    } else {
      if (AST_ACTIVE(ls)) {
        node = lusA_newnode(ls->ast, AST_LOCAL, line);
        ls->ast->curnode = node;
      }
      localstat(ls);
    }
    if (AST_ACTIVE(ls))
      ls->ast->curnode = NULL;
    break;
  }
  case TK_GLOBAL: { /* stat -> globalstatfunc */
    if (AST_ACTIVE(ls))
      node = lusA_newnode(ls->ast, AST_GLOBAL, line);
    globalstatfunc(ls, line);
    break;
  }
  case TK_DBCOLON: { /* stat -> label */
    if (AST_ACTIVE(ls))
      node = lusA_newnode(ls->ast, AST_LABEL, line);
    luaX_next(ls); /* skip double colon */
    labelstat(ls, str_checkname(ls), line);
    break;
  }
  case TK_RETURN: { /* stat -> retstat */
    if (AST_ACTIVE(ls))
      node = lusA_newnode(ls->ast, AST_RETURN, line);
    luaX_next(ls); /* skip RETURN */
    retstat(ls);
    break;
  }
  case TK_BREAK: { /* stat -> breakstat */
    if (AST_ACTIVE(ls))
      node = lusA_newnode(ls->ast, AST_BREAK, line);
    breakstat(ls, line);
    break;
  }
  case TK_CATCH: { /* stat -> catchstat */
    if (AST_ACTIVE(ls))
      node = lusA_newnode(ls->ast, AST_CATCHSTAT, line);
    catchstat(ls, line);
    break;
  }
  case TK_GOTO: { /* stat -> 'goto' NAME */
    if (AST_ACTIVE(ls))
      node = lusA_newnode(ls->ast, AST_GOTO, line);
    luaX_next(ls); /* skip 'goto' */
    gotostat(ls, line);
    break;
  }
#if defined(LUA_COMPAT_GLOBAL)
  case TK_NAME: {
    /* compatibility code to parse global keyword when "global"
       is not reserved */
    if (ls->t.seminfo.ts == ls->glbn) { /* current = "global"? */
      int lk = luaX_lookahead(ls);
      if (lk == '<' || lk == TK_NAME || lk == '*' || lk == TK_FUNCTION) {
        /* 'global <attrib>' or 'global name' or 'global *' or
           'global function' */
        if (AST_ACTIVE(ls))
          node = lusA_newnode(ls->ast, AST_GLOBAL, line);
        globalstatfunc(ls, line);
        break;
      }
    } /* else... */
  }
#endif
  /* FALLTHROUGH */
  default: { /* stat -> func | assignment */
    if (AST_ACTIVE(ls)) {
      node = lusA_newnode(ls->ast, AST_ASSIGN, line);
      ls->ast->curnode = node;
    }
    exprstat(ls);
    if (AST_ACTIVE(ls))
      ls->ast->curnode = NULL;
    break;
  }
  }
  /* Add statement node to current block (or root if no block context) */
  if (node != NULL && AST_ACTIVE(ls)) {
    LusAstNode *parent = ls->ast->curblock ? ls->ast->curblock : ls->ast->root;
    if (parent != NULL) {
      lusA_addchild(parent, node);
    }
  }
  lua_assert(ls->fs->f->maxstacksize >= ls->fs->freereg &&
             ls->fs->freereg >= luaY_nvarstack(ls->fs));
  ls->fs->freereg = luaY_nvarstack(ls->fs); /* free registers */
  leavelevel(ls);
}

/* }====================================================================== */

/* }====================================================================== */

/*
** compiles the main function, which is a regular vararg function with an
** upvalue named LUA_ENV
*/
static void mainfunc(LexState *ls, FuncState *fs) {
  BlockCnt bl;
  Upvaldesc *env;
  open_func(ls, fs, &bl);
  setvararg(fs);          /* main function is always vararg */
  env = allocupvalue(fs); /* ...set environment upvalue */
  env->instack = 1;
  env->idx = 0;
  env->kind = VDKREG;
  env->name = ls->envn;
  luaC_objbarrier(ls->L, fs->f, env->name);
  /* Create root AST chunk node if AST generation is enabled */
  if (AST_ACTIVE(ls)) {
    ls->ast->root = lusA_newnode(ls->ast, AST_CHUNK, 1);
  }
  luaX_next(ls); /* read first token */
  statlist(ls);  /* parse main body */
  check(ls, TK_EOS);
  close_func(ls);
}

LClosure *luaY_parser(lua_State *L, ZIO *z, Mbuffer *buff, Dyndata *dyd,
                      const char *name, int firstchar, LusAst *ast) {
  LexState lexstate;
  FuncState funcstate;
  LClosure *cl = luaF_newLclosure(L, 1); /* create main closure */
  setclLvalue2s(L, L->top.p, cl); /* anchor it (to avoid being collected) */
  luaD_inctop(L);
  lexstate.h = luaH_new(L);             /* create table for scanner */
  sethvalue2s(L, L->top.p, lexstate.h); /* anchor it */
  luaD_inctop(L);
  funcstate.f = cl->p = luaF_newproto(L);
  luaC_objbarrier(L, cl, cl->p);
  funcstate.f->source = luaS_new(L, name); /* create and anchor TString */
  luaC_objbarrier(L, funcstate.f, funcstate.f->source);
  lexstate.buff = buff;
  lexstate.dyd = dyd;
  lexstate.ast = ast; /* store AST pointer for parser to use */
  dyd->actvar.n = dyd->gt.n = dyd->label.n = 0;
  luaX_setinput(L, &lexstate, z, funcstate.f->source, firstchar);
  mainfunc(&lexstate, &funcstate);
  lua_assert(!funcstate.prev && funcstate.nups == 1 && !lexstate.fs);
  /* all scopes should be correctly finished */
  lua_assert(dyd->actvar.n == 0 && dyd->gt.n == 0 && dyd->label.n == 0);
  L->top.p--; /* remove scanner's table */
  return cl;  /* closure is on the stack, too */
}
