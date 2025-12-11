/*
** lparser2.h - Lus AST Parser
** Parser that produces Abstract Syntax Trees and AST node definitions.
*/

#ifndef lparser2_h
#define lparser2_h

#include <stdio.h>

#include "llex.h"
#include "llimits.h"
#include "lmem.h"
#include "lobject.h"
#include "lzio.h"

/*
** {==================================================================
** AST Node Types
** ===================================================================
*/

typedef enum lus_NodeType {
  /* Expressions - Literals */
  LUS_NODE_NIL,
  LUS_NODE_TRUE,
  LUS_NODE_FALSE,
  LUS_NODE_INTEGER,
  LUS_NODE_FLOAT,
  LUS_NODE_STRING,
  LUS_NODE_VARARG,

  /* Expressions - Compound */
  LUS_NODE_FUNCTION,   /* function definition */
  LUS_NODE_TABLE,      /* table constructor */
  LUS_NODE_BINOP,      /* binary operation */
  LUS_NODE_UNOP,       /* unary operation */
  LUS_NODE_CALL,       /* function call */
  LUS_NODE_METHODCALL, /* method call (obj:method(args)) */
  LUS_NODE_INDEX,      /* table[key] */
  LUS_NODE_FIELD,      /* table.field */
  LUS_NODE_CONCAT,     /* string concatenation (..) */

  /* Expressions - Variables */
  LUS_NODE_VAR,    /* local variable */
  LUS_NODE_UPVAL,  /* upvalue */
  LUS_NODE_GLOBAL, /* global variable */

  /* Expressions - Lus extensions */
  LUS_NODE_CATCH,    /* catch expression */
  LUS_NODE_OPTCHAIN, /* optional chaining (?) */
  LUS_NODE_ENUM,     /* enum expression */

  /* Statements */
  LUS_NODE_BLOCK,      /* block of statements */
  LUS_NODE_ASSIGN,     /* assignment */
  LUS_NODE_LOCAL,      /* local declaration */
  LUS_NODE_GLOBALDECL, /* global declaration */
  LUS_NODE_IF,         /* if statement */
  LUS_NODE_WHILE,      /* while loop */
  LUS_NODE_REPEAT,     /* repeat-until loop */
  LUS_NODE_FOR,        /* numeric for */
  LUS_NODE_FORIN,      /* generic for-in */
  LUS_NODE_RETURN,     /* return statement */
  LUS_NODE_BREAK,      /* break statement */
  LUS_NODE_GOTO,       /* goto statement */
  LUS_NODE_LABEL,      /* label definition */
  LUS_NODE_FUNCSTAT,   /* function statement */
  LUS_NODE_CATCHSTAT,  /* catch statement */
  LUS_NODE_FROM,       /* from table destructuring */

  LUS_NODE_COUNT /* total number of node types */
} lus_NodeType;

/* }================================================================== */

/*
** {==================================================================
** AST Node Structures
** ===================================================================
*/

/* Forward declarations */
typedef struct lus_Node lus_Node;
typedef struct lus_NodeList lus_NodeList;

/* Base node structure - all nodes start with this */
struct lus_Node {
  lus_NodeType type;
  int line;       /* source line number */
  lus_Node *next; /* for linked lists of nodes */
};

/* List of nodes (for statements, parameters, etc.) */
struct lus_NodeList {
  lus_Node *head;
  lus_Node *tail;
  int count;
};

/*
** Literal nodes
*/
typedef struct {
  lus_Node base;
  lua_Integer value;
} lus_IntegerNode;

typedef struct {
  lus_Node base;
  lua_Number value;
} lus_FloatNode;

typedef struct {
  lus_Node base;
  TString *value;
} lus_StringNode;

/*
** Binary/unary operation nodes
*/
typedef struct {
  lus_Node base;
  int op; /* BinOpr from lcode.h */
  lus_Node *left;
  lus_Node *right;
} lus_BinOpNode;

typedef struct {
  lus_Node base;
  int op; /* UnOpr from lcode.h */
  lus_Node *operand;
} lus_UnOpNode;

/*
** Variable nodes
*/
typedef struct {
  lus_Node base;
  TString *name;
  int vidx; /* variable index (register or upvalue index) */
} lus_VarNode;

/*
** Table access nodes
*/
typedef struct {
  lus_Node base;
  lus_Node *table; /* expression yielding the table */
  lus_Node *key;   /* key expression */
} lus_IndexNode;

typedef struct {
  lus_Node base;
  lus_Node *table; /* expression yielding the table */
  TString *field;  /* field name */
} lus_FieldNode;

/*
** Function call nodes
*/
typedef struct {
  lus_Node base;
  lus_Node *func;    /* function expression */
  lus_NodeList args; /* argument list */
} lus_CallNode;

typedef struct {
  lus_Node base;
  lus_Node *object;  /* object expression */
  TString *method;   /* method name */
  lus_NodeList args; /* argument list */
} lus_MethodCallNode;

/*
** Function definition node
*/
typedef struct {
  lus_Node base;
  lus_NodeList params; /* parameter names (lus_VarNode) */
  lus_Node *body;      /* function body (block) */
  lu_byte isvararg;    /* has ... parameter */
  lu_byte ismethod;    /* is a method (has implicit self) */
} lus_FunctionNode;

/*
** Table constructor node
*/
typedef struct lus_TableField {
  lus_Node *key;   /* key expression (NULL for array part) */
  lus_Node *value; /* value expression */
  struct lus_TableField *next;
} lus_TableField;

typedef struct {
  lus_Node base;
  lus_TableField *fields; /* linked list of fields */
  int narray;             /* count of array-style entries */
  int nhash;              /* count of hash-style entries */
} lus_TableNode;

/*
** Control flow nodes
*/
typedef struct {
  lus_Node base;
  lus_NodeList stmts; /* list of statements */
  lusM_Arena *arena;  /* arena for this AST (only on root block) */
} lus_BlockNode;

typedef struct lus_IfBranch {
  lus_Node *cond;       /* condition (NULL for else) */
  lus_Node *body;       /* block */
  lus_NodeList assigns; /* if-assignment variables (Lus extension) */
  struct lus_IfBranch *next;
} lus_IfBranch;

typedef struct {
  lus_Node base;
  lus_IfBranch *branches; /* if/elseif/else chain */
} lus_IfNode;

typedef struct {
  lus_Node base;
  lus_Node *cond; /* condition */
  lus_Node *body; /* body block */
} lus_WhileNode;

typedef struct {
  lus_Node base;
  lus_Node *body; /* body block */
  lus_Node *cond; /* until condition */
} lus_RepeatNode;

typedef struct {
  lus_Node base;
  lus_VarNode *var; /* loop variable */
  lus_Node *init;   /* initial value */
  lus_Node *limit;  /* limit value */
  lus_Node *step;   /* step value (NULL = 1) */
  lus_Node *body;   /* body block */
} lus_ForNode;

typedef struct {
  lus_Node base;
  lus_NodeList vars;  /* loop variables */
  lus_NodeList exprs; /* iterator expressions */
  lus_Node *body;     /* body block */
} lus_ForInNode;

/*
** Assignment nodes
*/
typedef struct {
  lus_Node base;
  lus_NodeList vars;  /* left-hand side variables */
  lus_NodeList exprs; /* right-hand side expressions */
} lus_AssignNode;

typedef struct {
  lus_Node base;
  lus_NodeList vars;  /* variable declarations */
  lus_NodeList exprs; /* initializer expressions */
  lu_byte kind;       /* VDKREG, RDKCONST, RDKTOCLOSE */
} lus_LocalNode;

typedef struct {
  lus_Node base;
  lus_NodeList vars;  /* variable declarations */
  lus_NodeList exprs; /* initializer expressions */
  lu_byte kind;       /* GDKREG, GDKCONST */
} lus_GlobalDeclNode;

/*
** Other statement nodes
*/
typedef struct {
  lus_Node base;
  lus_NodeList exprs; /* return values */
} lus_ReturnNode;

typedef struct {
  lus_Node base;
  TString *label; /* target label name */
} lus_GotoNode;

typedef struct {
  lus_Node base;
  TString *name; /* label name */
} lus_LabelNode;

/*
** Lus extension nodes
*/
typedef struct {
  lus_Node base;
  lus_Node *expr; /* expression to catch */
} lus_CatchNode;

typedef struct {
  lus_Node base;
  lus_Node *expr;        /* expression being chained */
  lus_NodeList suffixes; /* chain of suffix operations */
} lus_OptChainNode;

typedef struct {
  lus_Node base;
  lus_NodeList names; /* enum value names */
} lus_EnumNode;

typedef struct {
  lus_Node base;
  lus_NodeList vars; /* variables to assign */
  lus_Node *table;   /* source table */
} lus_FromNode;

/* }================================================================== */

/*
** {==================================================================
** Parser State
** ===================================================================
*/

/* Parser context - tracks arena and error state */
typedef struct lus_ParseState {
  lua_State *L;        /* Lua state for error handling */
  lusM_Arena *arena;   /* arena for AST nodes */
  LexState *ls;        /* lexical state (token stream) */
  struct Dyndata *dyd; /* dynamic data (for labels, etc.) */
  int nerrors;         /* error count */
} lus_ParseState;

/* }================================================================== */

/*
** {==================================================================
** AST Helper Functions
** ===================================================================
*/

/* Create a new node of given size from arena */
#define lus_newnode(a, T, type) ((T *)lus_newnode_(a, sizeof(T), type))

LUAI_FUNC lus_Node *lus_newnode_(lusM_Arena *a, size_t size, lus_NodeType type);

/* Node list operations */
LUAI_FUNC void lus_nodelist_init(lus_NodeList *list);
LUAI_FUNC void lus_nodelist_append(lus_NodeList *list, lus_Node *node);

/* Get node type name for debugging */
LUAI_FUNC const char *lus_nodetype_name(lus_NodeType type);

/* }================================================================== */

/*
** {==================================================================
** Parser API (all functions prefixed with lus_)
** ===================================================================
*/

/*
** Parse source code into an AST.
** Returns the root node (a block) or NULL on error.
** The arena is attached to the root block and must be freed
** by the caller using lus_freeparse().
*/
LUAI_FUNC lus_Node *lus_parse(lua_State *L, ZIO *z, Mbuffer *buff,
                              struct Dyndata *dyd, const char *name,
                              int firstchar);

/*
** Parse a string into an AST.
** Convenience wrapper around lus_parse().
*/
LUAI_FUNC lus_Node *lus_parsestring(lua_State *L, const char *source,
                                    size_t len, const char *name);

/*
** Free an AST and its associated arena.
** The 'root' must be a node returned by lus_parse() or lus_parsestring().
*/
LUAI_FUNC void lus_freeparse(lua_State *L, lus_Node *root);

/* }================================================================== */

/*
** {==================================================================
** AST Visualization (Graphviz DOT output)
** ===================================================================
*/

/*
** Output AST as Graphviz DOT format to the given file.
** Returns 0 on success, non-zero on error.
*/
LUAI_FUNC int lus_ast_to_dot(FILE *out, lus_Node *root);

/* }================================================================== */

/*
** {==================================================================
** AST to Lua Table Conversion (for debug.parse)
** ===================================================================
*/

/*
** Convert AST to a Lua table and push it onto the stack.
** Used by debug.parse() to expose AST structure to Lua code.
*/
LUAI_FUNC void lus_ast_to_table(lua_State *L, lus_Node *root);

/* }================================================================== */

#endif
