/*
** lcode2.h - Lus AST Code Generator
** Code generator that produces bytecode from AST.
*/

#ifndef lcode2_h
#define lcode2_h

#include <stdio.h>

#include "lcode.h" /* for BinOpr, UnOpr enums */
#include "lobject.h"
#include "lparser.h" /* for FuncState, expdesc */
#include "lparser2.h"

/*
** {==================================================================
** CFG (Control Flow Graph) Structures
** ===================================================================
*/

/* CFG basic block - sequence of instructions with single entry/exit */
typedef struct lus_CFGBlock {
  int id;                        /* unique block ID */
  int startpc;                   /* start PC in bytecode */
  int endpc;                     /* end PC in bytecode */
  struct lus_CFGBlock *next;     /* next block in list */
  struct lus_CFGBlock *truejmp;  /* successor on true/fall-through */
  struct lus_CFGBlock *falsejmp; /* successor on false branch */
} lus_CFGBlock;

/* CFG for a function */
typedef struct lus_CFG {
  lus_CFGBlock *entry;  /* entry block */
  lus_CFGBlock *blocks; /* list of all blocks */
  int nblocks;          /* number of blocks */
  lusM_Arena *arena;    /* arena for CFG nodes */
} lus_CFG;

/* }================================================================== */

/*
** {==================================================================
** Code Generator State
** ===================================================================
*/

/* Code generator state - wraps FuncState with AST context */
typedef struct lus_CodeGenState {
  lua_State *L;      /* Lua state */
  FuncState *fs;     /* current function state */
  LexState *ls;      /* lexer state (for error messages) */
  lusM_Arena *arena; /* AST arena (for temp allocations) */
  lus_CFG *cfg;      /* optional CFG for analysis */
} lus_CodeGenState;

/* }================================================================== */

/*
** {==================================================================
** Code Generator API (all functions prefixed with lus_)
** ===================================================================
*/

/*
** Generate bytecode from AST.
** Takes a parsed AST block (from lus_parse) and produces a Proto.
** Returns the compiled Proto, or NULL on error.
*/
LUAI_FUNC Proto *lus_codegen(lua_State *L, lus_Node *ast, const char *source);

/*
** Generate bytecode for a single function body.
** Used internally for nested function compilation.
*/
LUAI_FUNC void lus_codegen_func(lus_CodeGenState *cs, lus_FunctionNode *fn);

/* }================================================================== */

/*
** {==================================================================
** CFG API
** ===================================================================
*/

/*
** Build CFG from compiled bytecode.
** Call after lus_codegen to analyze the generated code.
*/
LUAI_FUNC lus_CFG *lus_build_cfg(lua_State *L, Proto *p);

/*
** Free CFG and its arena.
*/
LUAI_FUNC void lus_free_cfg(lus_CFG *cfg);

/*
** Output CFG as Graphviz DOT format.
*/
LUAI_FUNC int lus_cfg_to_dot(FILE *out, lus_CFG *cfg, Proto *p);

/* }================================================================== */

#endif
