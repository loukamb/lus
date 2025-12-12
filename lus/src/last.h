/*
** last.h
** Abstract Syntax Tree for Lus
** See Copyright Notice in lua.h
*/

#ifndef last_h
#define last_h

#include "lobject.h"
#include "lua.h"

/*
** AST Node Types
*/
typedef enum {
  /* Statements */
  AST_CHUNK,      /* root: list of statements */
  AST_BLOCK,      /* block of statements */
  AST_LOCAL,      /* local variable declaration */
  AST_GLOBAL,     /* global variable declaration */
  AST_ASSIGN,     /* assignment statement */
  AST_IF,         /* if statement */
  AST_WHILE,      /* while loop */
  AST_REPEAT,     /* repeat-until loop */
  AST_FORNUM,     /* numeric for */
  AST_FORGEN,     /* generic for */
  AST_FUNCSTAT,   /* function statement (assignment) */
  AST_LOCALFUNC,  /* local function definition */
  AST_GLOBALFUNC, /* global function definition */
  AST_RETURN,     /* return statement */
  AST_CALLSTAT,   /* function call (statement) */
  AST_BREAK,      /* break statement */
  AST_GOTO,       /* goto statement */
  AST_LABEL,      /* label */
  AST_CATCHSTAT,  /* catch statement */
  AST_DO,         /* do block end */
  /* Expressions */
  AST_NIL,
  AST_TRUE,
  AST_FALSE,
  AST_NUMBER,     /* integer or float literal */
  AST_STRING,     /* string literal */
  AST_VARARG,     /* ... */
  AST_NAME,       /* variable reference */
  AST_INDEX,      /* table[key] */
  AST_FIELD,      /* table.field */
  AST_BINOP,      /* binary operation */
  AST_UNOP,       /* unary operation */
  AST_TABLE,      /* table constructor */
  AST_FUNCEXPR,   /* function expression */
  AST_CALLEXPR,   /* function call (expression) */
  AST_METHODCALL, /* method call (a:b()) */
  AST_ENUM,       /* enum expression */
  AST_OPTCHAIN,   /* optional chaining (?) */
  AST_FROM,       /* from deconstruction */
  AST_CATCHEXPR,  /* catch expression */
  /* Auxiliary */
  AST_PARAM,      /* function parameter */
  AST_NAMELIST,   /* list of names */
  AST_EXPLIST,    /* list of expressions */
  AST_ELSEIF,     /* elseif branch */
  AST_ELSE,       /* else branch */
  AST_TABLEFIELD, /* table field (key = value or [key] = value) */
} LusAstType;

/*
** Binary operators (matches parser OPR_* values)
*/
typedef enum {
  AST_OP_ADD,
  AST_OP_SUB,
  AST_OP_MUL,
  AST_OP_MOD,
  AST_OP_POW,
  AST_OP_DIV,
  AST_OP_IDIV,
  AST_OP_BAND,
  AST_OP_BOR,
  AST_OP_BXOR,
  AST_OP_SHL,
  AST_OP_SHR,
  AST_OP_CONCAT,
  AST_OP_NE,
  AST_OP_EQ,
  AST_OP_LT,
  AST_OP_LE,
  AST_OP_GT,
  AST_OP_GE,
  AST_OP_AND,
  AST_OP_OR,
} LusAstBinOp;

/*
** Unary operators
*/
typedef enum {
  AST_OP_MINUS,
  AST_OP_BNOT,
  AST_OP_NOT,
  AST_OP_LEN,
} LusAstUnOp;

/*
** AST Node Structure
** Uses a flexible structure with type-specific unions.
*/
typedef struct LusAstNode {
  LusAstType type;          /* node type */
  int line;                 /* source line number */
  struct LusAstNode *next;  /* for sibling lists */
  struct LusAstNode *child; /* first child node */
  union {
    /* AST_NUMBER */
    struct {
      int isint; /* 1 if integer, 0 if float */
      union {
        lua_Integer i;
        lua_Number n;
      } val;
    } num;
    /* AST_STRING, AST_NAME, AST_FIELD, AST_LABEL, AST_GOTO */
    TString *str;
    /* AST_BINOP */
    struct {
      LusAstBinOp op;
      struct LusAstNode *left;
      struct LusAstNode *right;
    } binop;
    /* AST_UNOP */
    struct {
      LusAstUnOp op;
      struct LusAstNode *operand;
    } unop;
    /* AST_FUNCEXPR, AST_LOCALFUNC, AST_GLOBALFUNC, AST_FUNCSTAT */
    struct {
      struct LusAstNode *params; /* parameter list */
      struct LusAstNode *body;   /* function body */
      int isvararg;              /* has ... parameter */
      int ismethod;              /* has implicit self parameter */
    } func;
    /* AST_IF */
    struct {
      struct LusAstNode *cond;     /* condition */
      struct LusAstNode *thenpart; /* then block */
      struct LusAstNode *elsepart; /* else/elseif or NULL */
    } ifstat;
    /* AST_WHILE, AST_REPEAT */
    struct {
      struct LusAstNode *cond;
      struct LusAstNode *body;
    } loop;
    /* AST_FORNUM */
    struct {
      struct LusAstNode *var;   /* control variable name */
      struct LusAstNode *init;  /* initial value */
      struct LusAstNode *limit; /* limit */
      struct LusAstNode *step;  /* step (or NULL for default 1) */
      struct LusAstNode *body;
    } fornum;
    /* AST_FORGEN */
    struct {
      struct LusAstNode *names;   /* name list */
      struct LusAstNode *explist; /* iterator expressions */
      struct LusAstNode *body;
    } forgen;
    /* AST_LOCAL, AST_GLOBAL */
    struct {
      struct LusAstNode *names;  /* name list */
      struct LusAstNode *values; /* expression list (or NULL) */
      int isfrom;                /* 1 if using 'from' syntax */
    } decl;
    /* AST_ASSIGN */
    struct {
      struct LusAstNode *lhs; /* left-hand side list */
      struct LusAstNode *rhs; /* right-hand side list */
      int isfrom;             /* 1 if using 'from' syntax */
    } assign;
    /* AST_INDEX */
    struct {
      struct LusAstNode *table;
      struct LusAstNode *key;
    } index;
    /* AST_CALLEXPR, AST_CALLSTAT, AST_METHODCALL */
    struct {
      struct LusAstNode *func;
      struct LusAstNode *args;
      TString *method; /* method name for AST_METHODCALL */
    } call;
    /* AST_TABLE */
    struct {
      struct LusAstNode *fields; /* list of AST_TABLEFIELD */
    } table;
    /* AST_TABLEFIELD */
    struct {
      struct LusAstNode *key; /* NULL for array part */
      struct LusAstNode *value;
    } field;
    /* AST_RETURN */
    struct {
      struct LusAstNode *values; /* expression list (or NULL) */
    } ret;
    /* AST_PARAM */
    struct {
      TString *name;
      int kind; /* VDKREG, RDKCONST, etc. */
    } param;
    /* AST_CATCHEXPR, AST_CATCHSTAT */
    struct {
      struct LusAstNode *expr; /* expression to catch */
    } catchnode;
    /* AST_OPTCHAIN */
    struct {
      struct LusAstNode *base;   /* base expression */
      struct LusAstNode *suffix; /* suffix operations */
    } optchain;
    /* AST_ENUM */
    struct {
      struct LusAstNode *names; /* name list */
    } enumdef;
  } u;
} LusAstNode;

/*
** AST Container (plain allocation, not GC-managed)
*/
typedef struct LusAst {
  lua_State *L;         /* lua state for allocations */
  LusAstNode *root;     /* root node */
  LusAstNode *curnode;  /* current statement node being built */
  LusAstNode *curblock; /* current block/parent for nested statements */
  size_t nodecount;     /* number of nodes allocated */
} LusAst;

/*
** AST API Functions
*/

/* Create a new AST container */
LUAI_FUNC LusAst *lusA_new(lua_State *L);

/* Free an AST and all its nodes */
LUAI_FUNC void lusA_free(lua_State *L, LusAst *ast);

/* Allocate a new node */
LUAI_FUNC LusAstNode *lusA_newnode(LusAst *ast, LusAstType type, int line);

/* Free a single node (internal use) */
LUAI_FUNC void lusA_freenode(lua_State *L, LusAstNode *node);

/* Add a child node to a parent */
LUAI_FUNC void lusA_addchild(LusAstNode *parent, LusAstNode *child);

/* Add a sibling node (for lists) */
LUAI_FUNC void lusA_addsibling(LusAstNode *node, LusAstNode *sibling);

/* Convert AST to Lua table (for debug.parse) */
LUAI_FUNC void lusA_totable(lua_State *L, LusAst *ast);

/* Convert AST to Graphviz DOT format */
LUAI_FUNC int lusA_tographviz(LusAst *ast, const char *filename);

/* Convert AST to JSON format */
LUAI_FUNC int lusA_tojson(LusAst *ast, const char *filename);

/* Get string name for node type */
LUAI_FUNC const char *lusA_typename(LusAstType type);

/*
** Macros for conditional AST building in parser
*/
#define AST_ACTIVE(ls) ((ls)->ast != NULL)

#define AST_NODE(ls, type, line)                                               \
  (AST_ACTIVE(ls) ? lusA_newnode((ls)->ast, (type), (line)) : NULL)

#define AST_SETCHILD(parent, child)                                            \
  do {                                                                         \
    if ((parent) != NULL)                                                      \
      lusA_addchild((parent), (child));                                        \
  } while (0)

#define AST_ADDSIBLING(node, sib)                                              \
  do {                                                                         \
    if ((node) != NULL)                                                        \
      lusA_addsibling((node), (sib));                                          \
  } while (0)

#endif
