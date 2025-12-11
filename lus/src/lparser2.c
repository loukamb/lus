/*
** lparser2.c - Lus AST Parser Implementation
** Parser that produces Abstract Syntax Trees.
*/

#define lparser2_c
#define LUA_CORE

#include "lprefix.h"

#include <stdio.h>
#include <string.h>

#include "lua.h"

#include "ldo.h"
#include "lgc.h"
#include "lparser2.h"
#include "lstring.h"
#include "ltable.h"

/*
** {==================================================================
** Node Type Names (for debugging)
** ===================================================================
*/

static const char *const nodetype_names[] = {
    /* Expressions - Literals */
    "nil", "true", "false", "integer", "float", "string", "vararg",
    /* Expressions - Compound */
    "function", "table", "binop", "unop", "call", "methodcall", "index",
    "field", "concat",
    /* Expressions - Variables */
    "var", "upval", "global",
    /* Expressions - Lus extensions */
    "catch", "optchain", "enum",
    /* Statements */
    "block", "assign", "local", "globaldecl", "if", "while", "repeat", "for",
    "forin", "return", "break", "goto", "label", "funcstat", "catchstat",
    "from"};

/* }================================================================== */

/*
** {==================================================================
** AST Helper Functions
** ===================================================================
*/

/*
** Create a new node of the given size and type from the arena.
*/
lus_Node *lus_newnode_(lusM_Arena *a, size_t size, lus_NodeType type) {
  lus_Node *node = cast(lus_Node *, lusM_arenaalloc(a, size));
  memset(node, 0, size);
  node->type = type;
  node->line = 0;
  node->next = NULL;
  return node;
}

/*
** Initialize a node list to empty.
*/
void lus_nodelist_init(lus_NodeList *list) {
  list->head = NULL;
  list->tail = NULL;
  list->count = 0;
}

/*
** Append a node to the end of a node list.
*/
void lus_nodelist_append(lus_NodeList *list, lus_Node *node) {
  node->next = NULL;
  if (list->tail == NULL) {
    list->head = node;
    list->tail = node;
  } else {
    list->tail->next = node;
    list->tail = node;
  }
  list->count++;
}

/*
** Get the name of a node type for debugging.
*/
const char *lus_nodetype_name(lus_NodeType type) {
  if (type >= 0 && type < LUS_NODE_COUNT)
    return nodetype_names[type];
  else
    return "unknown";
}

/* }================================================================== */

/*
** {==================================================================
** Parser State Management
** ===================================================================
*/

/*
** Initialize parser state.
*/
static void init_parsestate(lus_ParseState *ps, lua_State *L, LexState *ls,
                            struct Dyndata *dyd) {
  ps->L = L;
  ps->arena = lusM_newarena(L, 0);
  ps->ls = ls;
  ps->dyd = dyd;
  ps->nerrors = 0;
}

/*
** Free parser state (but not arena - caller owns that).
*/
static void close_parsestate(lus_ParseState *ps) {
  (void)ps; /* nothing to do currently */
}

/* }================================================================== */

/*
** {==================================================================
** Lexer Helpers
** ===================================================================
*/

#define check(ps, c) ((ps)->ls->t.token == (c))

static void next(lus_ParseState *ps) { luaX_next(ps->ls); }

static int testnext(lus_ParseState *ps, int c) {
  if (ps->ls->t.token == c) {
    next(ps);
    return 1;
  }
  return 0;
}

static void checknext(lus_ParseState *ps, int c) {
  if (ps->ls->t.token != c)
    luaX_syntaxerror(ps->ls, luaX_token2str(ps->ls, c));
  next(ps);
}

static TString *str_checkname(lus_ParseState *ps) {
  TString *ts;
  if (ps->ls->t.token != TK_NAME)
    luaX_syntaxerror(ps->ls, "name expected");
  ts = ps->ls->t.seminfo.ts;
  next(ps);
  return ts;
}

/* }================================================================== */

/*
** {==================================================================
** Expression Parsing (Forward Declarations)
** ===================================================================
*/

static lus_Node *parse_expr(lus_ParseState *ps);
static lus_Node *parse_subexpr(lus_ParseState *ps, int limit);
static lus_Node *parse_block(lus_ParseState *ps);

/* }================================================================== */

/*
** {==================================================================
** Simple Expression Parsing
** ===================================================================
*/

static lus_Node *parse_simpleexp(lus_ParseState *ps) {
  lusM_Arena *a = ps->arena;
  LexState *ls = ps->ls;
  int line = ls->linenumber;

  switch (ls->t.token) {
  case TK_NIL: {
    lus_Node *node = lus_newnode(a, lus_Node, LUS_NODE_NIL);
    node->line = line;
    next(ps);
    return node;
  }
  case TK_TRUE: {
    lus_Node *node = lus_newnode(a, lus_Node, LUS_NODE_TRUE);
    node->line = line;
    next(ps);
    return node;
  }
  case TK_FALSE: {
    lus_Node *node = lus_newnode(a, lus_Node, LUS_NODE_FALSE);
    node->line = line;
    next(ps);
    return node;
  }
  case TK_INT: {
    lus_IntegerNode *node = lus_newnode(a, lus_IntegerNode, LUS_NODE_INTEGER);
    node->base.line = line;
    node->value = ls->t.seminfo.i;
    next(ps);
    return &node->base;
  }
  case TK_FLT: {
    lus_FloatNode *node = lus_newnode(a, lus_FloatNode, LUS_NODE_FLOAT);
    node->base.line = line;
    node->value = ls->t.seminfo.r;
    next(ps);
    return &node->base;
  }
  case TK_STRING: {
    lus_StringNode *node = lus_newnode(a, lus_StringNode, LUS_NODE_STRING);
    node->base.line = line;
    node->value = ls->t.seminfo.ts;
    next(ps);
    return &node->base;
  }
  case TK_DOTS: { /* vararg */
    lus_Node *node = lus_newnode(a, lus_Node, LUS_NODE_VARARG);
    node->line = line;
    next(ps);
    return node;
  }
  case TK_NAME: {
    lus_VarNode *node = lus_newnode(a, lus_VarNode, LUS_NODE_VAR);
    node->base.line = line;
    node->name = str_checkname(ps);
    node->vidx = -1; /* unresolved */
    return &node->base;
  }
  case '(': {
    next(ps);
    lus_Node *expr = parse_expr(ps);
    checknext(ps, ')');
    return expr;
  }
  case '{': {
    /* table constructor */
    lus_TableNode *node = lus_newnode(a, lus_TableNode, LUS_NODE_TABLE);
    node->base.line = line;
    node->fields = NULL;
    node->narray = 0;
    node->nhash = 0;
    next(ps); /* skip '{' */
    if (!check(ps, '}')) {
      lus_TableField *tail = NULL;
      do {
        if (check(ps, '}'))
          break;
        lus_TableField *field =
            cast(lus_TableField *, lusM_arenaalloc(a, sizeof(lus_TableField)));
        field->next = NULL;
        if (check(ps, '[')) {
          /* [expr] = expr */
          next(ps);
          field->key = parse_expr(ps);
          checknext(ps, ']');
          checknext(ps, '=');
          field->value = parse_expr(ps);
          node->nhash++;
        } else if (check(ps, TK_NAME) && ls->lookahead.token == '=') {
          /* name = expr */
          lus_StringNode *key = lus_newnode(a, lus_StringNode, LUS_NODE_STRING);
          key->base.line = ls->linenumber;
          key->value = str_checkname(ps);
          field->key = &key->base;
          checknext(ps, '=');
          field->value = parse_expr(ps);
          node->nhash++;
        } else {
          /* expr (array part) */
          field->key = NULL;
          field->value = parse_expr(ps);
          node->narray++;
        }
        if (tail == NULL) {
          node->fields = field;
        } else {
          tail->next = field;
        }
        tail = field;
      } while (testnext(ps, ',') || testnext(ps, ';'));
    }
    checknext(ps, '}');
    return &node->base;
  }
  case TK_FUNCTION: {
    /* anonymous function */
    next(ps); /* skip 'function' */
    lus_FunctionNode *node =
        lus_newnode(a, lus_FunctionNode, LUS_NODE_FUNCTION);
    node->base.line = line;
    node->isvararg = 0;
    node->ismethod = 0;
    lus_nodelist_init(&node->params);
    checknext(ps, '(');
    /* parse parameters */
    if (!check(ps, ')')) {
      do {
        if (check(ps, TK_DOTS)) {
          node->isvararg = 1;
          next(ps);
          break;
        }
        lus_VarNode *param = lus_newnode(a, lus_VarNode, LUS_NODE_VAR);
        param->base.line = ls->linenumber;
        param->name = str_checkname(ps);
        param->vidx = -1;
        lus_nodelist_append(&node->params, &param->base);
      } while (testnext(ps, ','));
    }
    checknext(ps, ')');
    node->body = parse_block(ps);
    checknext(ps, TK_END);
    return &node->base;
  }
  case TK_CATCH: {
    /* catch expression */
    next(ps); /* skip 'catch' */
    lus_CatchNode *node = lus_newnode(a, lus_CatchNode, LUS_NODE_CATCH);
    node->base.line = line;
    node->expr = parse_subexpr(ps, 0);
    return &node->base;
  }
  case TK_ENUM: {
    /* enum expression */
    next(ps); /* skip 'enum' */
    lus_EnumNode *node = lus_newnode(a, lus_EnumNode, LUS_NODE_ENUM);
    node->base.line = line;
    lus_nodelist_init(&node->names);
    /* parse enum names */
    do {
      lus_VarNode *name = lus_newnode(a, lus_VarNode, LUS_NODE_VAR);
      name->base.line = ls->linenumber;
      name->name = str_checkname(ps);
      name->vidx = -1;
      lus_nodelist_append(&node->names, &name->base);
    } while (testnext(ps, ','));
    checknext(ps, TK_END);
    return &node->base;
  }
  default: {
    luaX_syntaxerror(ls, "unexpected symbol");
    return NULL;
  }
  }
}

/* }================================================================== */

/*
** {==================================================================
** Suffix Expression Parsing
** ===================================================================
*/

static lus_Node *parse_suffixedexp(lus_ParseState *ps) {
  lusM_Arena *a = ps->arena;
  LexState *ls = ps->ls;
  lus_Node *expr = parse_simpleexp(ps);

  for (;;) {
    switch (ls->t.token) {
    case '.': {
      next(ps);
      lus_FieldNode *node = lus_newnode(a, lus_FieldNode, LUS_NODE_FIELD);
      node->base.line = ls->linenumber;
      node->table = expr;
      node->field = str_checkname(ps);
      expr = &node->base;
      break;
    }
    case '[': {
      next(ps);
      lus_IndexNode *node = lus_newnode(a, lus_IndexNode, LUS_NODE_INDEX);
      node->base.line = ls->linenumber;
      node->table = expr;
      node->key = parse_expr(ps);
      checknext(ps, ']');
      expr = &node->base;
      break;
    }
    case ':': {
      next(ps);
      lus_MethodCallNode *node =
          lus_newnode(a, lus_MethodCallNode, LUS_NODE_METHODCALL);
      node->base.line = ls->linenumber;
      node->object = expr;
      node->method = str_checkname(ps);
      lus_nodelist_init(&node->args);
      /* TODO: parse arguments */
      if (ls->t.token != '(')
        luaX_syntaxerror(ls, "'(' expected after method name");
      next(ps);
      /* parse args */
      if (ls->t.token != ')') {
        do {
          lus_Node *arg = parse_expr(ps);
          lus_nodelist_append(&node->args, arg);
        } while (testnext(ps, ','));
      }
      checknext(ps, ')');
      expr = &node->base;
      break;
    }
    case '(':
    case TK_STRING:
    case '{': {
      lus_CallNode *node = lus_newnode(a, lus_CallNode, LUS_NODE_CALL);
      node->base.line = ls->linenumber;
      node->func = expr;
      lus_nodelist_init(&node->args);
      if (ls->t.token == '(') {
        next(ps);
        if (ls->t.token != ')') {
          do {
            lus_Node *arg = parse_expr(ps);
            lus_nodelist_append(&node->args, arg);
          } while (testnext(ps, ','));
        }
        checknext(ps, ')');
      } else if (ls->t.token == TK_STRING) {
        lus_StringNode *str = lus_newnode(a, lus_StringNode, LUS_NODE_STRING);
        str->base.line = ls->linenumber;
        str->value = ls->t.seminfo.ts;
        next(ps);
        lus_nodelist_append(&node->args, &str->base);
      } else {
        /* table constructor as single argument - TODO */
        luaX_syntaxerror(ls, "table constructor not yet implemented");
      }
      expr = &node->base;
      break;
    }
    case '?': {
      /* TODO: optional chaining */
      luaX_syntaxerror(ls, "optional chaining not yet implemented");
      return NULL;
    }
    default:
      return expr;
    }
  }
}

/* }================================================================== */

/*
** {==================================================================
** Binary/Unary Expression Parsing
** ===================================================================
*/

/* Operator priority table */
static const struct {
  lu_byte left;
  lu_byte right;
} priority[] = {
    {10, 10}, {10, 10},         /* + - */
    {11, 11}, {11, 11},         /* * % */
    {14, 13},                   /* ^ (right associative) */
    {11, 11}, {11, 11},         /* / // */
    {6, 6},   {4, 4},   {5, 5}, /* & | ~ */
    {7, 7},   {7, 7},           /* << >> */
    {9, 8},                     /* .. (right associative) */
    {3, 3},   {3, 3},   {3, 3}, /* == < <= */
    {3, 3},   {3, 3},   {3, 3}, /* ~= > >= */
    {2, 2},   {1, 1}            /* and or */
};

#define UNARY_PRIORITY 12

static int getbinopr(int op) {
  switch (op) {
  case '+':
    return 0;
  case '-':
    return 1;
  case '*':
    return 2;
  case '%':
    return 3;
  case '^':
    return 4;
  case '/':
    return 5;
  case TK_IDIV:
    return 6;
  case '&':
    return 7;
  case '|':
    return 8;
  case '~':
    return 9;
  case TK_SHL:
    return 10;
  case TK_SHR:
    return 11;
  case TK_CONCAT:
    return 12;
  case TK_EQ:
    return 13;
  case '<':
    return 14;
  case TK_LE:
    return 15;
  case TK_NE:
    return 16;
  case '>':
    return 17;
  case TK_GE:
    return 18;
  case TK_AND:
    return 19;
  case TK_OR:
    return 20;
  default:
    return -1;
  }
}

static int getunopr(int op) {
  switch (op) {
  case '-':
    return 0;
  case '~':
    return 1;
  case TK_NOT:
    return 2;
  case '#':
    return 3;
  default:
    return -1;
  }
}

static lus_Node *parse_subexpr(lus_ParseState *ps, int limit) {
  lusM_Arena *a = ps->arena;
  LexState *ls = ps->ls;
  lus_Node *expr;
  int uop = getunopr(ls->t.token);

  if (uop >= 0) {
    int line = ls->linenumber;
    next(ps);
    lus_UnOpNode *node = lus_newnode(a, lus_UnOpNode, LUS_NODE_UNOP);
    node->base.line = line;
    node->op = uop;
    node->operand = parse_subexpr(ps, UNARY_PRIORITY);
    expr = &node->base;
  } else {
    expr = parse_suffixedexp(ps);
  }

  /* binary operators */
  int op = getbinopr(ls->t.token);
  while (op >= 0 && priority[op].left > limit) {
    int line = ls->linenumber;
    next(ps);
    lus_BinOpNode *node = lus_newnode(a, lus_BinOpNode, LUS_NODE_BINOP);
    node->base.line = line;
    node->op = op;
    node->left = expr;
    node->right = parse_subexpr(ps, priority[op].right);
    expr = &node->base;
    op = getbinopr(ls->t.token);
  }

  return expr;
}

static lus_Node *parse_expr(lus_ParseState *ps) { return parse_subexpr(ps, 0); }

/* }================================================================== */

/*
** {==================================================================
** Statement Parsing
** ===================================================================
*/

static lus_Node *parse_statement(lus_ParseState *ps);

static lus_Node *parse_block(lus_ParseState *ps) {
  lusM_Arena *a = ps->arena;
  lus_BlockNode *block = lus_newnode(a, lus_BlockNode, LUS_NODE_BLOCK);
  block->base.line = ps->ls->linenumber;
  block->arena = NULL; /* only set on root */
  lus_nodelist_init(&block->stmts);

  while (!check(ps, TK_END) && !check(ps, TK_ELSE) && !check(ps, TK_ELSEIF) &&
         !check(ps, TK_UNTIL) && !check(ps, TK_EOS)) {
    lus_Node *stmt = parse_statement(ps);
    if (stmt != NULL)
      lus_nodelist_append(&block->stmts, stmt);
  }

  return &block->base;
}

static lus_Node *parse_retstat(lus_ParseState *ps) {
  lusM_Arena *a = ps->arena;
  LexState *ls = ps->ls;
  lus_ReturnNode *node = lus_newnode(a, lus_ReturnNode, LUS_NODE_RETURN);
  node->base.line = ls->linenumber;
  lus_nodelist_init(&node->exprs);

  next(ps); /* skip 'return' */

  /* check for empty return */
  if (!check(ps, TK_END) && !check(ps, TK_ELSE) && !check(ps, TK_ELSEIF) &&
      !check(ps, TK_UNTIL) && !check(ps, TK_EOS) && !check(ps, ';')) {
    do {
      lus_Node *expr = parse_expr(ps);
      lus_nodelist_append(&node->exprs, expr);
    } while (testnext(ps, ','));
  }
  testnext(ps, ';');

  return &node->base;
}

static lus_Node *parse_statement(lus_ParseState *ps) {
  lusM_Arena *a = ps->arena;
  LexState *ls = ps->ls;
  int line = ls->linenumber;

  switch (ls->t.token) {
  case ';': {
    next(ps);
    return NULL; /* empty statement */
  }
  case TK_RETURN: {
    return parse_retstat(ps);
  }
  case TK_BREAK: {
    lus_Node *node = lus_newnode(a, lus_Node, LUS_NODE_BREAK);
    node->line = line;
    next(ps);
    return node;
  }
  case TK_GOTO: {
    lus_GotoNode *node = lus_newnode(a, lus_GotoNode, LUS_NODE_GOTO);
    node->base.line = line;
    next(ps);
    node->label = str_checkname(ps);
    return &node->base;
  }
  case TK_DBCOLON: { /* label */
    lus_LabelNode *node = lus_newnode(a, lus_LabelNode, LUS_NODE_LABEL);
    node->base.line = line;
    next(ps);
    node->name = str_checkname(ps);
    checknext(ps, TK_DBCOLON);
    return &node->base;
  }
  case TK_IF: {
    /* if cond then block {elseif cond then block} [else block] end */
    lus_IfNode *node = lus_newnode(a, lus_IfNode, LUS_NODE_IF);
    node->base.line = line;
    node->branches = NULL;
    lus_IfBranch *tail = NULL;
    do {
      lus_IfBranch *br =
          cast(lus_IfBranch *, lusM_arenaalloc(a, sizeof(lus_IfBranch)));
      br->next = NULL;
      lus_nodelist_init(&br->assigns);
      next(ps); /* skip 'if' or 'elseif' */
      br->cond = parse_expr(ps);
      checknext(ps, TK_THEN);
      br->body = parse_block(ps);
      if (tail == NULL)
        node->branches = br;
      else
        tail->next = br;
      tail = br;
    } while (check(ps, TK_ELSEIF));
    if (testnext(ps, TK_ELSE)) {
      lus_IfBranch *br =
          cast(lus_IfBranch *, lusM_arenaalloc(a, sizeof(lus_IfBranch)));
      br->next = NULL;
      br->cond = NULL; /* else has no condition */
      lus_nodelist_init(&br->assigns);
      br->body = parse_block(ps);
      tail->next = br;
    }
    checknext(ps, TK_END);
    return &node->base;
  }
  case TK_WHILE: {
    /* while cond do block end */
    lus_WhileNode *node = lus_newnode(a, lus_WhileNode, LUS_NODE_WHILE);
    node->base.line = line;
    next(ps); /* skip 'while' */
    node->cond = parse_expr(ps);
    checknext(ps, TK_DO);
    node->body = parse_block(ps);
    checknext(ps, TK_END);
    return &node->base;
  }
  case TK_REPEAT: {
    /* repeat block until cond */
    lus_RepeatNode *node = lus_newnode(a, lus_RepeatNode, LUS_NODE_REPEAT);
    node->base.line = line;
    next(ps); /* skip 'repeat' */
    node->body = parse_block(ps);
    checknext(ps, TK_UNTIL);
    node->cond = parse_expr(ps);
    return &node->base;
  }
  case TK_FOR: {
    /* for name = e1,e2[,e3] do block end  OR  for namelist in explist do block
     * end */
    next(ps); /* skip 'for' */
    TString *firstname = str_checkname(ps);
    if (check(ps, '=')) {
      /* numeric for */
      lus_ForNode *node = lus_newnode(a, lus_ForNode, LUS_NODE_FOR);
      node->base.line = line;
      lus_VarNode *var = lus_newnode(a, lus_VarNode, LUS_NODE_VAR);
      var->base.line = line;
      var->name = firstname;
      var->vidx = -1;
      node->var = var;
      next(ps); /* skip '=' */
      node->init = parse_expr(ps);
      checknext(ps, ',');
      node->limit = parse_expr(ps);
      if (testnext(ps, ',')) {
        node->step = parse_expr(ps);
      } else {
        node->step = NULL;
      }
      checknext(ps, TK_DO);
      node->body = parse_block(ps);
      checknext(ps, TK_END);
      return &node->base;
    } else {
      /* generic for-in */
      lus_ForInNode *node = lus_newnode(a, lus_ForInNode, LUS_NODE_FORIN);
      node->base.line = line;
      lus_nodelist_init(&node->vars);
      lus_nodelist_init(&node->exprs);
      /* first name already read */
      lus_VarNode *var = lus_newnode(a, lus_VarNode, LUS_NODE_VAR);
      var->base.line = line;
      var->name = firstname;
      var->vidx = -1;
      lus_nodelist_append(&node->vars, &var->base);
      while (testnext(ps, ',')) {
        lus_VarNode *v = lus_newnode(a, lus_VarNode, LUS_NODE_VAR);
        v->base.line = ls->linenumber;
        v->name = str_checkname(ps);
        v->vidx = -1;
        lus_nodelist_append(&node->vars, &v->base);
      }
      checknext(ps, TK_IN);
      do {
        lus_Node *e = parse_expr(ps);
        lus_nodelist_append(&node->exprs, e);
      } while (testnext(ps, ','));
      checknext(ps, TK_DO);
      node->body = parse_block(ps);
      checknext(ps, TK_END);
      return &node->base;
    }
  }
  case TK_DO: {
    /* do block end */
    next(ps); /* skip 'do' */
    lus_Node *block = parse_block(ps);
    checknext(ps, TK_END);
    return block;
  }
  case TK_LOCAL: {
    /* local namelist [= explist] | local function name body */
    next(ps); /* skip 'local' */
    if (testnext(ps, TK_FUNCTION)) {
      /* local function name body */
      lus_LocalNode *node = lus_newnode(a, lus_LocalNode, LUS_NODE_LOCAL);
      node->base.line = line;
      node->kind = 0; /* VDKREG */
      lus_nodelist_init(&node->vars);
      lus_nodelist_init(&node->exprs);
      lus_VarNode *var = lus_newnode(a, lus_VarNode, LUS_NODE_VAR);
      var->base.line = ls->linenumber;
      var->name = str_checkname(ps);
      var->vidx = -1;
      lus_nodelist_append(&node->vars, &var->base);
      /* parse function body */
      lus_FunctionNode *fn =
          lus_newnode(a, lus_FunctionNode, LUS_NODE_FUNCTION);
      fn->base.line = ls->linenumber;
      fn->isvararg = 0;
      fn->ismethod = 0;
      lus_nodelist_init(&fn->params);
      checknext(ps, '(');
      if (!check(ps, ')')) {
        do {
          if (check(ps, TK_DOTS)) {
            fn->isvararg = 1;
            next(ps);
            break;
          }
          lus_VarNode *p = lus_newnode(a, lus_VarNode, LUS_NODE_VAR);
          p->base.line = ls->linenumber;
          p->name = str_checkname(ps);
          p->vidx = -1;
          lus_nodelist_append(&fn->params, &p->base);
        } while (testnext(ps, ','));
      }
      checknext(ps, ')');
      fn->body = parse_block(ps);
      checknext(ps, TK_END);
      lus_nodelist_append(&node->exprs, &fn->base);
      return &node->base;
    } else {
      /* local namelist [= explist] */
      lus_LocalNode *node = lus_newnode(a, lus_LocalNode, LUS_NODE_LOCAL);
      node->base.line = line;
      node->kind = 0; /* VDKREG */
      lus_nodelist_init(&node->vars);
      lus_nodelist_init(&node->exprs);
      do {
        lus_VarNode *var = lus_newnode(a, lus_VarNode, LUS_NODE_VAR);
        var->base.line = ls->linenumber;
        var->name = str_checkname(ps);
        var->vidx = -1;
        lus_nodelist_append(&node->vars, &var->base);
      } while (testnext(ps, ','));
      if (testnext(ps, '=')) {
        do {
          lus_Node *e = parse_expr(ps);
          lus_nodelist_append(&node->exprs, e);
        } while (testnext(ps, ','));
      }
      return &node->base;
    }
  }
  case TK_GLOBAL: {
    /* global namelist [= explist] | global function name body */
    next(ps); /* skip 'global' */
    if (testnext(ps, TK_FUNCTION)) {
      /* global function name body */
      lus_GlobalDeclNode *node =
          lus_newnode(a, lus_GlobalDeclNode, LUS_NODE_GLOBALDECL);
      node->base.line = line;
      node->kind = 0;
      lus_nodelist_init(&node->vars);
      lus_nodelist_init(&node->exprs);
      lus_VarNode *var = lus_newnode(a, lus_VarNode, LUS_NODE_VAR);
      var->base.line = ls->linenumber;
      var->name = str_checkname(ps);
      var->vidx = -1;
      lus_nodelist_append(&node->vars, &var->base);
      /* parse function body */
      lus_FunctionNode *fn =
          lus_newnode(a, lus_FunctionNode, LUS_NODE_FUNCTION);
      fn->base.line = ls->linenumber;
      fn->isvararg = 0;
      fn->ismethod = 0;
      lus_nodelist_init(&fn->params);
      checknext(ps, '(');
      if (!check(ps, ')')) {
        do {
          if (check(ps, TK_DOTS)) {
            fn->isvararg = 1;
            next(ps);
            break;
          }
          lus_VarNode *p = lus_newnode(a, lus_VarNode, LUS_NODE_VAR);
          p->base.line = ls->linenumber;
          p->name = str_checkname(ps);
          p->vidx = -1;
          lus_nodelist_append(&fn->params, &p->base);
        } while (testnext(ps, ','));
      }
      checknext(ps, ')');
      fn->body = parse_block(ps);
      checknext(ps, TK_END);
      lus_nodelist_append(&node->exprs, &fn->base);
      return &node->base;
    } else {
      /* global namelist [= explist] */
      lus_GlobalDeclNode *node =
          lus_newnode(a, lus_GlobalDeclNode, LUS_NODE_GLOBALDECL);
      node->base.line = line;
      node->kind = 0;
      lus_nodelist_init(&node->vars);
      lus_nodelist_init(&node->exprs);
      do {
        lus_VarNode *var = lus_newnode(a, lus_VarNode, LUS_NODE_VAR);
        var->base.line = ls->linenumber;
        var->name = str_checkname(ps);
        var->vidx = -1;
        lus_nodelist_append(&node->vars, &var->base);
      } while (testnext(ps, ','));
      if (testnext(ps, '=')) {
        do {
          lus_Node *e = parse_expr(ps);
          lus_nodelist_append(&node->exprs, e);
        } while (testnext(ps, ','));
      }
      return &node->base;
    }
  }
  case TK_FUNCTION: {
    /* function funcname body */
    next(ps); /* skip 'function' */
    /* For now, treat as expression statement - TODO: handle dotted names */
    lus_AssignNode *node = lus_newnode(a, lus_AssignNode, LUS_NODE_ASSIGN);
    node->base.line = line;
    lus_nodelist_init(&node->vars);
    lus_nodelist_init(&node->exprs);
    /* parse function name (may include dots and colon) */
    lus_VarNode *var = lus_newnode(a, lus_VarNode, LUS_NODE_VAR);
    var->base.line = ls->linenumber;
    var->name = str_checkname(ps);
    var->vidx = -1;
    lus_Node *target = &var->base;
    int ismethod = 0;
    while (check(ps, '.') || check(ps, ':')) {
      if (check(ps, ':')) {
        ismethod = 1;
        next(ps);
        lus_FieldNode *f = lus_newnode(a, lus_FieldNode, LUS_NODE_FIELD);
        f->base.line = ls->linenumber;
        f->table = target;
        f->field = str_checkname(ps);
        target = &f->base;
        break; /* colon must be last */
      }
      next(ps); /* skip '.' */
      lus_FieldNode *f = lus_newnode(a, lus_FieldNode, LUS_NODE_FIELD);
      f->base.line = ls->linenumber;
      f->table = target;
      f->field = str_checkname(ps);
      target = &f->base;
    }
    lus_nodelist_append(&node->vars, target);
    /* parse function body */
    lus_FunctionNode *fn = lus_newnode(a, lus_FunctionNode, LUS_NODE_FUNCTION);
    fn->base.line = ls->linenumber;
    fn->isvararg = 0;
    fn->ismethod = cast(lu_byte, ismethod);
    lus_nodelist_init(&fn->params);
    checknext(ps, '(');
    if (!check(ps, ')')) {
      do {
        if (check(ps, TK_DOTS)) {
          fn->isvararg = 1;
          next(ps);
          break;
        }
        lus_VarNode *p = lus_newnode(a, lus_VarNode, LUS_NODE_VAR);
        p->base.line = ls->linenumber;
        p->name = str_checkname(ps);
        p->vidx = -1;
        lus_nodelist_append(&fn->params, &p->base);
      } while (testnext(ps, ','));
    }
    checknext(ps, ')');
    fn->body = parse_block(ps);
    checknext(ps, TK_END);
    lus_nodelist_append(&node->exprs, &fn->base);
    return &node->base;
  }
  case TK_CATCH: {
    /* catch statement - execute block and catch errors */
    luaX_syntaxerror(ls, "catch statement not yet implemented");
    return NULL;
  }
  default: {
    /* expression statement (function call or assignment) */
    lus_Node *expr = parse_suffixedexp(ps);
    if (check(ps, '=') || check(ps, ',')) {
      /* assignment: varlist = explist */
      lus_AssignNode *node = lus_newnode(a, lus_AssignNode, LUS_NODE_ASSIGN);
      node->base.line = line;
      lus_nodelist_init(&node->vars);
      lus_nodelist_init(&node->exprs);
      lus_nodelist_append(&node->vars, expr);
      while (testnext(ps, ',')) {
        lus_Node *v = parse_suffixedexp(ps);
        lus_nodelist_append(&node->vars, v);
      }
      checknext(ps, '=');
      do {
        lus_Node *e = parse_expr(ps);
        lus_nodelist_append(&node->exprs, e);
      } while (testnext(ps, ','));
      return &node->base;
    } else if (check(ps, TK_FROM)) {
      /* from table deconstruction */
      lus_FromNode *node = lus_newnode(a, lus_FromNode, LUS_NODE_FROM);
      node->base.line = line;
      lus_nodelist_init(&node->vars);
      lus_nodelist_append(&node->vars, expr);
      while (testnext(ps, ',')) {
        lus_Node *v = parse_suffixedexp(ps);
        lus_nodelist_append(&node->vars, v);
      }
      next(ps); /* skip 'from' */
      node->table = parse_expr(ps);
      return &node->base;
    } else {
      /* function call as statement */
      if (expr->type != LUS_NODE_CALL && expr->type != LUS_NODE_METHODCALL)
        luaX_syntaxerror(ls, "syntax error");
      return expr;
    }
  }
  }
}

/* }================================================================== */

/*
** {==================================================================
** Main Parser API
** ===================================================================
*/

lus_Node *lus_parse(lua_State *L, ZIO *z, Mbuffer *buff, struct Dyndata *dyd,
                    const char *name, int firstchar) {
  LexState lexstate;
  lus_ParseState ps;
  (void)buff; /* unused for now */

  /* create scanner hash table and anchor it on stack (like luaY_parser) */
  lexstate.h = luaH_new(L);
  sethvalue2s(L, L->top.p, lexstate.h);
  luaD_inctop(L);

  /* initialize lexer */
  luaX_setinput(L, &lexstate, z, luaS_new(L, name), firstchar);

  /* initialize parser state */
  init_parsestate(&ps, L, &lexstate, dyd);

  /* skip first token */
  next(&ps);

  /* parse main block */
  lus_BlockNode *root = (lus_BlockNode *)parse_block(&ps);
  root->arena = ps.arena; /* attach arena to root for cleanup */

  /* check for end of input */
  if (ps.ls->t.token != TK_EOS)
    luaX_syntaxerror(ps.ls, "<eof> expected");

  close_parsestate(&ps);

  /* pop scanner table */
  L->top.p--;

  return &root->base;
}

lus_Node *lus_parsestring(lua_State *L, const char *source, size_t len,
                          const char *name) {
  /* TODO: implement string parsing with a string reader */
  (void)L;
  (void)source;
  (void)len;
  (void)name;
  return NULL;
}

void lus_freeparse(lua_State *L, lus_Node *root) {
  if (root != NULL && root->type == LUS_NODE_BLOCK) {
    lus_BlockNode *block = (lus_BlockNode *)root;
    if (block->arena != NULL)
      lusM_freearena(block->arena);
  }
  (void)L;
}

/* }================================================================== */

/*
** {==================================================================
** AST Visualization (DOT output)
** ===================================================================
*/

static int dot_node_id = 0;

static int emit_dot_node(FILE *out, lus_Node *node) {
  int id = dot_node_id++;
  fprintf(out, "  n%d [label=\"%s", id, lus_nodetype_name(node->type));

  switch (node->type) {
  case LUS_NODE_INTEGER:
    fprintf(out, "\\n%lld", (long long)((lus_IntegerNode *)node)->value);
    break;
  case LUS_NODE_FLOAT:
    fprintf(out, "\\n%g", ((lus_FloatNode *)node)->value);
    break;
  case LUS_NODE_STRING:
    fprintf(out, "\\n\\\"%s\\\"", getstr(((lus_StringNode *)node)->value));
    break;
  case LUS_NODE_VAR:
  case LUS_NODE_UPVAL:
  case LUS_NODE_GLOBAL:
    fprintf(out, "\\n%s", getstr(((lus_VarNode *)node)->name));
    break;
  default:
    break;
  }

  fprintf(out, "\"];\n");
  return id;
}

static void emit_dot_edge(FILE *out, int from, int to, const char *label) {
  if (label)
    fprintf(out, "  n%d -> n%d [label=\"%s\"];\n", from, to, label);
  else
    fprintf(out, "  n%d -> n%d;\n", from, to);
}

static void emit_dot_tree(FILE *out, lus_Node *node, int parent,
                          const char *edge_label);

static void emit_dot_list(FILE *out, lus_NodeList *list, int parent,
                          const char *label) {
  lus_Node *n;
  for (n = list->head; n != NULL; n = n->next) {
    emit_dot_tree(out, n, parent, label);
  }
}

static void emit_dot_tree(FILE *out, lus_Node *node, int parent,
                          const char *edge_label) {
  if (node == NULL)
    return;

  int id = emit_dot_node(out, node);
  if (parent >= 0)
    emit_dot_edge(out, parent, id, edge_label);

  switch (node->type) {
  case LUS_NODE_BINOP: {
    lus_BinOpNode *n = (lus_BinOpNode *)node;
    emit_dot_tree(out, n->left, id, "L");
    emit_dot_tree(out, n->right, id, "R");
    break;
  }
  case LUS_NODE_UNOP: {
    lus_UnOpNode *n = (lus_UnOpNode *)node;
    emit_dot_tree(out, n->operand, id, NULL);
    break;
  }
  case LUS_NODE_CALL: {
    lus_CallNode *n = (lus_CallNode *)node;
    emit_dot_tree(out, n->func, id, "fn");
    emit_dot_list(out, &n->args, id, "arg");
    break;
  }
  case LUS_NODE_METHODCALL: {
    lus_MethodCallNode *n = (lus_MethodCallNode *)node;
    emit_dot_tree(out, n->object, id, "obj");
    emit_dot_list(out, &n->args, id, "arg");
    break;
  }
  case LUS_NODE_INDEX: {
    lus_IndexNode *n = (lus_IndexNode *)node;
    emit_dot_tree(out, n->table, id, "tbl");
    emit_dot_tree(out, n->key, id, "key");
    break;
  }
  case LUS_NODE_FIELD: {
    lus_FieldNode *n = (lus_FieldNode *)node;
    emit_dot_tree(out, n->table, id, "tbl");
    break;
  }
  case LUS_NODE_BLOCK: {
    lus_BlockNode *n = (lus_BlockNode *)node;
    emit_dot_list(out, &n->stmts, id, NULL);
    break;
  }
  case LUS_NODE_RETURN: {
    lus_ReturnNode *n = (lus_ReturnNode *)node;
    emit_dot_list(out, &n->exprs, id, NULL);
    break;
  }
  default:
    break;
  }
}

int lus_ast_to_dot(FILE *out, lus_Node *root) {
  if (out == NULL || root == NULL)
    return -1;

  dot_node_id = 0;
  fprintf(out, "digraph AST {\n");
  fprintf(out, "  node [shape=box];\n");
  emit_dot_tree(out, root, -1, NULL);
  fprintf(out, "}\n");

  return 0;
}

/* }================================================================== */

/*
** {==================================================================
** AST to Lua Table Conversion
** ===================================================================
*/

static void node_to_table(lua_State *L, lus_Node *node);

static void list_to_table(lua_State *L, lus_NodeList *list) {
  lua_newtable(L);
  int i = 1;
  for (lus_Node *n = list->head; n != NULL; n = n->next) {
    node_to_table(L, n);
    lua_rawseti(L, -2, i++);
  }
}

static void node_to_table(lua_State *L, lus_Node *node) {
  if (node == NULL) {
    lua_pushnil(L);
    return;
  }

  lua_newtable(L);

  /* type field */
  lua_pushstring(L, lus_nodetype_name(node->type));
  lua_setfield(L, -2, "type");

  /* line field */
  lua_pushinteger(L, node->line);
  lua_setfield(L, -2, "line");

  /* type-specific fields */
  switch (node->type) {
  case LUS_NODE_INTEGER:
    lua_pushinteger(L, ((lus_IntegerNode *)node)->value);
    lua_setfield(L, -2, "value");
    break;
  case LUS_NODE_FLOAT:
    lua_pushnumber(L, ((lus_FloatNode *)node)->value);
    lua_setfield(L, -2, "value");
    break;
  case LUS_NODE_STRING:
    lua_pushstring(L, getstr(((lus_StringNode *)node)->value));
    lua_setfield(L, -2, "value");
    break;
  case LUS_NODE_VAR:
  case LUS_NODE_UPVAL:
  case LUS_NODE_GLOBAL:
    lua_pushstring(L, getstr(((lus_VarNode *)node)->name));
    lua_setfield(L, -2, "name");
    break;
  case LUS_NODE_BINOP: {
    lus_BinOpNode *n = (lus_BinOpNode *)node;
    lua_pushinteger(L, n->op);
    lua_setfield(L, -2, "op");
    node_to_table(L, n->left);
    lua_setfield(L, -2, "left");
    node_to_table(L, n->right);
    lua_setfield(L, -2, "right");
    break;
  }
  case LUS_NODE_UNOP: {
    lus_UnOpNode *n = (lus_UnOpNode *)node;
    lua_pushinteger(L, n->op);
    lua_setfield(L, -2, "op");
    node_to_table(L, n->operand);
    lua_setfield(L, -2, "operand");
    break;
  }
  case LUS_NODE_CALL: {
    lus_CallNode *n = (lus_CallNode *)node;
    node_to_table(L, n->func);
    lua_setfield(L, -2, "func");
    list_to_table(L, &n->args);
    lua_setfield(L, -2, "args");
    break;
  }
  case LUS_NODE_BLOCK: {
    lus_BlockNode *n = (lus_BlockNode *)node;
    list_to_table(L, &n->stmts);
    lua_setfield(L, -2, "body");
    break;
  }
  case LUS_NODE_RETURN: {
    lus_ReturnNode *n = (lus_ReturnNode *)node;
    list_to_table(L, &n->exprs);
    lua_setfield(L, -2, "values");
    break;
  }
  default:
    break;
  }
}

void lus_ast_to_table(lua_State *L, lus_Node *root) { node_to_table(L, root); }

/* }================================================================== */
