/*
** last.c
** Abstract Syntax Tree for Lus
** See Copyright Notice in lua.h
*/

#define last_c
#define LUA_CORE

#include "lprefix.h"

#include <stdio.h>
#include <string.h>

#include "lua.h"

#include "last.h"
#include "lmem.h"
#include "lobject.h"

/*
** Node type names for debugging and Graphviz output
*/
static const char *const ast_typenames[] = {
    "chunk",      "block",  "local",      "global",   "assign",     "if",
    "while",      "repeat", "fornum",     "forgen",   "funcstat",   "localfunc",
    "globalfunc", "return", "callstat",   "break",    "goto",       "label",
    "catchstat",  "do",     "nil",        "true",     "false",      "number",
    "string",     "vararg", "name",       "index",    "field",      "binop",
    "unop",       "table",  "funcexpr",   "callexpr", "methodcall", "enum",
    "optchain",   "from",   "catchexpr",  "param",    "namelist",   "explist",
    "elseif",     "else",   "tablefield", NULL};

const char *lusA_typename(LusAstType type) {
  if (type >= 0 &&
      type < (int)(sizeof(ast_typenames) / sizeof(ast_typenames[0]) - 1))
    return ast_typenames[type];
  return "unknown";
}

/*
** Create a new AST container (uses plain memory, not GC-managed)
*/
LusAst *lusA_new(lua_State *L) {
  LusAst *ast = luaM_new(L, LusAst);
  memset(ast, 0, sizeof(LusAst));
  ast->L = L;
  ast->root = NULL;
  ast->nodecount = 0;
  return ast;
}

/*
** Free a single node (does not free children)
*/
void lusA_freenode(lua_State *L, LusAstNode *node) {
  if (node == NULL)
    return;
  luaM_free(L, node);
}

/*
** Recursively free a node and all its descendants
*/
static void freenodetree(lua_State *L, LusAstNode *node) {
  if (node == NULL)
    return;

  /* Check for already-freed node (double-free protection) */
  if ((int)node->type < 0) {
    /* This node was already freed - skip it */
    return;
  }

  /* Mark as freed to prevent double-free */
  node->type = (LusAstType)(-1);

  /* Free siblings first */
  freenodetree(L, node->next);

  /* Free children */
  freenodetree(L, node->child);

  /* Free type-specific data */
  switch (node->type) {
  case AST_BINOP:
    freenodetree(L, node->u.binop.left);
    freenodetree(L, node->u.binop.right);
    break;
  case AST_UNOP:
    freenodetree(L, node->u.unop.operand);
    break;
  case AST_FUNCEXPR:
  case AST_LOCALFUNC:
  case AST_GLOBALFUNC:
  case AST_FUNCSTAT:
    freenodetree(L, node->u.func.params);
    /* body is in children, not u.func.body */
    break;
  case AST_IF:
    freenodetree(L, node->u.ifstat.cond);
    /* then/else are in children */
    break;
  case AST_WHILE:
  case AST_REPEAT:
    freenodetree(L, node->u.loop.cond);
    /* body is in children */
    break;
  case AST_FORNUM:
    freenodetree(L, node->u.fornum.var);
    freenodetree(L, node->u.fornum.init);
    freenodetree(L, node->u.fornum.limit);
    freenodetree(L, node->u.fornum.step);
    /* body is in children */
    break;
  case AST_FORGEN:
    freenodetree(L, node->u.forgen.names);
    freenodetree(L, node->u.forgen.explist);
    /* body is in children */
    break;
  case AST_LOCAL:
  case AST_GLOBAL:
    freenodetree(L, node->u.decl.names);
    freenodetree(L, node->u.decl.values);
    break;
  case AST_ASSIGN:
    freenodetree(L, node->u.assign.lhs);
    freenodetree(L, node->u.assign.rhs);
    break;
  case AST_FIELD:
  case AST_INDEX:
    freenodetree(L, node->u.index.table);
    freenodetree(L, node->u.index.key);
    break;
  case AST_CALLEXPR:
  case AST_CALLSTAT:
  case AST_METHODCALL:
    freenodetree(L, node->u.call.func);
    freenodetree(L, node->u.call.args);
    break;
  case AST_TABLE:
    freenodetree(L, node->u.table.fields);
    break;
  case AST_TABLEFIELD:
    freenodetree(L, node->u.field.key);
    freenodetree(L, node->u.field.value);
    break;
  case AST_RETURN:
    freenodetree(L, node->u.ret.values);
    break;
  case AST_CATCHEXPR:
  case AST_CATCHSTAT:
    freenodetree(L, node->u.catchnode.expr);
    break;
  case AST_OPTCHAIN:
    freenodetree(L, node->u.optchain.base);
    freenodetree(L, node->u.optchain.suffix);
    break;
  case AST_ENUM:
    freenodetree(L, node->u.enumdef.names);
    break;
  default:
    /* No nested nodes to free */
    break;
  }

  luaM_free(L, node);
}

/*
** Free an AST and all its nodes
*/
void lusA_free(lua_State *L, LusAst *ast) {
  if (ast == NULL)
    return;
  freenodetree(L, ast->root);
  luaM_free(L, ast);
}

/*
** Allocate a new node
*/
LusAstNode *lusA_newnode(LusAst *ast, LusAstType type, int line) {
  LusAstNode *node = luaM_new(ast->L, LusAstNode);
  memset(node, 0, sizeof(LusAstNode));
  node->type = type;
  node->line = line;
  node->next = NULL;
  node->child = NULL;
  ast->nodecount++;
  return node;
}

/*
** Add a child node to a parent (appends to child list)
*/
void lusA_addchild(LusAstNode *parent, LusAstNode *child) {
  if (parent == NULL || child == NULL)
    return;
  if (parent->child == NULL) {
    parent->child = child;
  } else {
    LusAstNode *last = parent->child;
    while (last->next != NULL)
      last = last->next;
    last->next = child;
  }
}

/*
** Add a sibling node (for lists)
*/
void lusA_addsibling(LusAstNode *node, LusAstNode *sibling) {
  if (node == NULL || sibling == NULL)
    return;
  while (node->next != NULL)
    node = node->next;
  node->next = sibling;
}

/*
** Binary operator names for Graphviz
*/
static const char *binop_names[] = {
    "+",  "-",  "*",  "%",  "^", "/",  "//", "&",  "|",   "~", "<<",
    ">>", "..", "~=", "==", "<", "<=", ">",  ">=", "and", "or"};

/*
** Unary operator names for Graphviz
*/
static const char *unop_names[] = {"-", "~", "not", "#"};

/*
** Convert a single node to a Lua table (recursive)
*/
static void nodetotable(lua_State *L, LusAstNode *node) {
  if (node == NULL) {
    lua_pushnil(L);
    return;
  }

  lua_newtable(L);

  /* type */
  lua_pushstring(L, lusA_typename(node->type));
  lua_setfield(L, -2, "type");

  /* line */
  lua_pushinteger(L, node->line);
  lua_setfield(L, -2, "line");

  /* Type-specific fields */
  switch (node->type) {
  case AST_NUMBER:
    if (node->u.num.isint) {
      lua_pushinteger(L, node->u.num.val.i);
    } else {
      lua_pushnumber(L, node->u.num.val.n);
    }
    lua_setfield(L, -2, "value");
    break;

  case AST_STRING:
  case AST_NAME:
  case AST_LABEL:
  case AST_GOTO:
    if (node->u.str != NULL) {
      lua_pushstring(L, getstr(node->u.str));
      lua_setfield(L, -2, "value");
    }
    break;

  case AST_FIELD:
  case AST_INDEX:
    nodetotable(L, node->u.index.table);
    lua_setfield(L, -2, "table");
    nodetotable(L, node->u.index.key);
    lua_setfield(L, -2, "key");
    break;

  case AST_BINOP:
    lua_pushstring(L, binop_names[node->u.binop.op]);
    lua_setfield(L, -2, "op");
    nodetotable(L, node->u.binop.left);
    lua_setfield(L, -2, "left");
    nodetotable(L, node->u.binop.right);
    lua_setfield(L, -2, "right");
    break;

  case AST_UNOP:
    lua_pushstring(L, unop_names[node->u.unop.op]);
    lua_setfield(L, -2, "op");
    nodetotable(L, node->u.unop.operand);
    lua_setfield(L, -2, "operand");
    break;

  case AST_FUNCEXPR:
  case AST_LOCALFUNC:
  case AST_GLOBALFUNC:
  case AST_FUNCSTAT:
    nodetotable(L, node->u.func.params);
    lua_setfield(L, -2, "params");
    nodetotable(L, node->u.func.body);
    lua_setfield(L, -2, "body");
    lua_pushboolean(L, node->u.func.isvararg);
    lua_setfield(L, -2, "isvararg");
    lua_pushboolean(L, node->u.func.ismethod);
    lua_setfield(L, -2, "ismethod");
    break;

  case AST_IF:
    nodetotable(L, node->u.ifstat.cond);
    lua_setfield(L, -2, "cond");
    /* then/elseif/else statements are in children array */
    break;

  case AST_WHILE:
  case AST_REPEAT:
    nodetotable(L, node->u.loop.cond);
    lua_setfield(L, -2, "cond");
    /* body statements are in children array, not u.loop.body */
    break;

  case AST_FORNUM:
    nodetotable(L, node->u.fornum.var);
    lua_setfield(L, -2, "var");
    nodetotable(L, node->u.fornum.init);
    lua_setfield(L, -2, "init");
    nodetotable(L, node->u.fornum.limit);
    lua_setfield(L, -2, "limit");
    nodetotable(L, node->u.fornum.step);
    lua_setfield(L, -2, "step");
    /* body statements are in children array */
    break;

  case AST_FORGEN:
    nodetotable(L, node->u.forgen.names);
    lua_setfield(L, -2, "names");
    nodetotable(L, node->u.forgen.explist);
    lua_setfield(L, -2, "explist");
    /* body statements are in children array */
    break;

  case AST_LOCAL:
  case AST_GLOBAL:
    nodetotable(L, node->u.decl.names);
    lua_setfield(L, -2, "names");
    nodetotable(L, node->u.decl.values);
    lua_setfield(L, -2, "values");
    lua_pushboolean(L, node->u.decl.isfrom);
    lua_setfield(L, -2, "isfrom");
    break;

  case AST_ASSIGN:
    nodetotable(L, node->u.assign.lhs);
    lua_setfield(L, -2, "lhs");
    nodetotable(L, node->u.assign.rhs);
    lua_setfield(L, -2, "rhs");
    lua_pushboolean(L, node->u.assign.isfrom);
    lua_setfield(L, -2, "isfrom");
    break;

  case AST_CALLEXPR:
  case AST_CALLSTAT:
  case AST_METHODCALL:
    nodetotable(L, node->u.call.func);
    lua_setfield(L, -2, "func");
    nodetotable(L, node->u.call.args);
    lua_setfield(L, -2, "args");
    if (node->type == AST_METHODCALL && node->u.call.method != NULL) {
      lua_pushstring(L, getstr(node->u.call.method));
      lua_setfield(L, -2, "method");
    }
    break;

  case AST_TABLE:
    nodetotable(L, node->u.table.fields);
    lua_setfield(L, -2, "fields");
    break;

  case AST_TABLEFIELD:
    nodetotable(L, node->u.field.key);
    lua_setfield(L, -2, "key");
    nodetotable(L, node->u.field.value);
    lua_setfield(L, -2, "value");
    break;

  case AST_RETURN:
    nodetotable(L, node->u.ret.values);
    lua_setfield(L, -2, "values");
    break;

  case AST_PARAM:
    if (node->u.param.name != NULL) {
      lua_pushstring(L, getstr(node->u.param.name));
      lua_setfield(L, -2, "name");
    }
    lua_pushinteger(L, node->u.param.kind);
    lua_setfield(L, -2, "kind");
    break;

  case AST_CATCHEXPR:
  case AST_CATCHSTAT:
    nodetotable(L, node->u.catchnode.expr);
    lua_setfield(L, -2, "expr");
    break;

  case AST_OPTCHAIN:
    nodetotable(L, node->u.optchain.base);
    lua_setfield(L, -2, "base");
    nodetotable(L, node->u.optchain.suffix);
    lua_setfield(L, -2, "suffix");
    break;

  case AST_ENUM:
    nodetotable(L, node->u.enumdef.names);
    lua_setfield(L, -2, "names");
    break;

  default:
    break;
  }

  /* children array (for generic child lists) */
  if (node->child != NULL) {
    int i = 1;
    LusAstNode *c = node->child;
    lua_newtable(L);
    while (c != NULL) {
      nodetotable(L, c);
      lua_rawseti(L, -2, i++);
      c = c->next;
    }
    lua_setfield(L, -2, "children");
  }
}

/*
** Convert AST to Lua table (for debug.parse)
** NOTE: We must stop GC while iterating, because AST nodes contain
** raw TString* pointers that are GC-managed. If GC runs during
** nodetotable, those strings can be collected, causing use-after-free.
*/
void lusA_totable(lua_State *L, LusAst *ast) {
  if (ast == NULL || ast->root == NULL) {
    lua_pushnil(L);
    return;
  }
  lua_gc(L, LUA_GCSTOP); /* prevent GC during AST traversal */
  nodetotable(L, ast->root);
  lua_gc(L, LUA_GCRESTART); /* resume GC */
}

/*
** Graphviz output helpers
*/
static int graphviz_nodeid = 0;

static int emit_node(FILE *f, LusAstNode *node) {
  int myid = graphviz_nodeid++;
  const char *label;
  char buf[256];

  if (node == NULL)
    return -1;

  /* Build label based on type */
  switch (node->type) {
  case AST_NUMBER:
    if (node->u.num.isint)
      snprintf(buf, sizeof(buf), "number\\n%lld", (long long)node->u.num.val.i);
    else
      snprintf(buf, sizeof(buf), "number\\n%.14g", node->u.num.val.n);
    label = buf;
    break;
  case AST_STRING:
  case AST_NAME:
    snprintf(buf, sizeof(buf), "%s\\n\\\"%s\\\"", lusA_typename(node->type),
             node->u.str ? getstr(node->u.str) : "");
    label = buf;
    break;
  case AST_BINOP:
    snprintf(buf, sizeof(buf), "binop\\n%s", binop_names[node->u.binop.op]);
    label = buf;
    break;
  case AST_UNOP:
    snprintf(buf, sizeof(buf), "unop\\n%s", unop_names[node->u.unop.op]);
    label = buf;
    break;
  default:
    label = lusA_typename(node->type);
    break;
  }

  fprintf(f, "  n%d [label=\"%s\"];\n", myid, label);

  /* Emit edges for type-specific children */
  switch (node->type) {
  case AST_BINOP: {
    int left = emit_node(f, node->u.binop.left);
    int right = emit_node(f, node->u.binop.right);
    if (left >= 0)
      fprintf(f, "  n%d -> n%d [label=\"L\"];\n", myid, left);
    if (right >= 0)
      fprintf(f, "  n%d -> n%d [label=\"R\"];\n", myid, right);
    break;
  }
  case AST_UNOP: {
    int op = emit_node(f, node->u.unop.operand);
    if (op >= 0)
      fprintf(f, "  n%d -> n%d;\n", myid, op);
    break;
  }
  case AST_IF: {
    int cond = emit_node(f, node->u.ifstat.cond);
    int then = emit_node(f, node->u.ifstat.thenpart);
    int elsepart = emit_node(f, node->u.ifstat.elsepart);
    if (cond >= 0)
      fprintf(f, "  n%d -> n%d [label=\"cond\"];\n", myid, cond);
    if (then >= 0)
      fprintf(f, "  n%d -> n%d [label=\"then\"];\n", myid, then);
    if (elsepart >= 0)
      fprintf(f, "  n%d -> n%d [label=\"else\"];\n", myid, elsepart);
    break;
  }
  case AST_WHILE:
  case AST_REPEAT: {
    int cond = emit_node(f, node->u.loop.cond);
    int body = emit_node(f, node->u.loop.body);
    if (cond >= 0)
      fprintf(f, "  n%d -> n%d [label=\"cond\"];\n", myid, cond);
    if (body >= 0)
      fprintf(f, "  n%d -> n%d [label=\"body\"];\n", myid, body);
    break;
  }
  default: {
    /* Emit generic children */
    LusAstNode *c = node->child;
    while (c != NULL) {
      int cid = emit_node(f, c);
      if (cid >= 0)
        fprintf(f, "  n%d -> n%d;\n", myid, cid);
      c = c->next;
    }
    break;
  }
  }

  return myid;
}

/*
** Convert AST to Graphviz DOT format
*/
int lusA_tographviz(LusAst *ast, const char *filename) {
  FILE *f;

  if (ast == NULL || ast->root == NULL || filename == NULL)
    return 0;

  f = fopen(filename, "w");
  if (f == NULL)
    return 0;

  graphviz_nodeid = 0;

  fprintf(f, "digraph AST {\n");
  fprintf(f, "  node [shape=box, fontname=\"Courier\"];\n");
  fprintf(f, "  edge [fontname=\"Courier\", fontsize=10];\n");

  emit_node(f, ast->root);

  fprintf(f, "}\n");
  fclose(f);

  return 1;
}

/*
** JSON output helpers
*/
static void json_escape_string(FILE *f, const char *s) {
  fputc('"', f);
  while (*s) {
    switch (*s) {
    case '"':
      fputs("\\\"", f);
      break;
    case '\\':
      fputs("\\\\", f);
      break;
    case '\n':
      fputs("\\n", f);
      break;
    case '\r':
      fputs("\\r", f);
      break;
    case '\t':
      fputs("\\t", f);
      break;
    default:
      if ((unsigned char)*s < 32)
        fprintf(f, "\\u%04x", (unsigned char)*s);
      else
        fputc(*s, f);
    }
    s++;
  }
  fputc('"', f);
}

static void emit_json_node(FILE *f, LusAstNode *node, int indent);

static void emit_json_children(FILE *f, LusAstNode *first, int indent) {
  LusAstNode *c = first;
  int first_item = 1;
  fprintf(f, "[\n");
  while (c != NULL) {
    if (!first_item)
      fprintf(f, ",\n");
    first_item = 0;
    emit_json_node(f, c, indent + 2);
    c = c->next;
  }
  fprintf(f, "\n%*s]", indent, "");
}

static void emit_json_node(FILE *f, LusAstNode *node, int indent) {
  if (node == NULL) {
    fprintf(f, "%*snull", indent, "");
    return;
  }

  fprintf(f, "%*s{\n", indent, "");
  fprintf(f, "%*s\"type\": \"%s\",\n", indent + 2, "",
          ast_typenames[node->type]);
  fprintf(f, "%*s\"line\": %d", indent + 2, "", node->line);

  /* Type-specific fields */
  switch (node->type) {
  case AST_NUMBER:
    if (node->u.num.isint)
      fprintf(f, ",\n%*s\"value\": " LUA_INTEGER_FMT, indent + 2, "",
              node->u.num.val.i);
    else
      fprintf(f, ",\n%*s\"value\": %g", indent + 2, "", node->u.num.val.n);
    break;

  case AST_STRING:
  case AST_NAME:
  case AST_GOTO:
  case AST_LABEL:
    if (node->u.str != NULL) {
      fprintf(f, ",\n%*s\"value\": ", indent + 2, "");
      json_escape_string(f, getstr(node->u.str));
    }
    break;

  case AST_BINOP:
    fprintf(f, ",\n%*s\"op\": \"%s\"", indent + 2, "",
            binop_names[node->u.binop.op]);
    if (node->u.binop.left) {
      fprintf(f, ",\n%*s\"left\": ", indent + 2, "");
      emit_json_node(f, node->u.binop.left, 0);
    }
    if (node->u.binop.right) {
      fprintf(f, ",\n%*s\"right\": ", indent + 2, "");
      emit_json_node(f, node->u.binop.right, 0);
    }
    break;

  case AST_UNOP:
    fprintf(f, ",\n%*s\"op\": \"%s\"", indent + 2, "",
            unop_names[node->u.unop.op]);
    if (node->u.unop.operand) {
      fprintf(f, ",\n%*s\"operand\": ", indent + 2, "");
      emit_json_node(f, node->u.unop.operand, 0);
    }
    break;

  case AST_FUNCEXPR:
  case AST_LOCALFUNC:
  case AST_GLOBALFUNC:
  case AST_FUNCSTAT:
    if (node->u.func.params) {
      fprintf(f, ",\n%*s\"params\": ", indent + 2, "");
      emit_json_children(f, node->u.func.params, indent + 2);
    }
    break;

  case AST_IF:
    if (node->u.ifstat.cond) {
      fprintf(f, ",\n%*s\"cond\": ", indent + 2, "");
      emit_json_node(f, node->u.ifstat.cond, 0);
    }
    break;

  case AST_WHILE:
  case AST_REPEAT:
    if (node->u.loop.cond) {
      fprintf(f, ",\n%*s\"cond\": ", indent + 2, "");
      emit_json_node(f, node->u.loop.cond, 0);
    }
    break;

  case AST_FORNUM:
    if (node->u.fornum.var) {
      fprintf(f, ",\n%*s\"var\": ", indent + 2, "");
      emit_json_node(f, node->u.fornum.var, 0);
    }
    if (node->u.fornum.init) {
      fprintf(f, ",\n%*s\"init\": ", indent + 2, "");
      emit_json_node(f, node->u.fornum.init, 0);
    }
    if (node->u.fornum.limit) {
      fprintf(f, ",\n%*s\"limit\": ", indent + 2, "");
      emit_json_node(f, node->u.fornum.limit, 0);
    }
    if (node->u.fornum.step) {
      fprintf(f, ",\n%*s\"step\": ", indent + 2, "");
      emit_json_node(f, node->u.fornum.step, 0);
    }
    break;

  case AST_FORGEN:
    if (node->u.forgen.names) {
      fprintf(f, ",\n%*s\"names\": ", indent + 2, "");
      emit_json_children(f, node->u.forgen.names, indent + 2);
    }
    if (node->u.forgen.explist) {
      fprintf(f, ",\n%*s\"explist\": ", indent + 2, "");
      emit_json_children(f, node->u.forgen.explist, indent + 2);
    }
    break;

  case AST_LOCAL:
  case AST_GLOBAL:
    if (node->u.decl.names) {
      fprintf(f, ",\n%*s\"names\": ", indent + 2, "");
      emit_json_children(f, node->u.decl.names, indent + 2);
    }
    if (node->u.decl.values) {
      fprintf(f, ",\n%*s\"values\": ", indent + 2, "");
      emit_json_children(f, node->u.decl.values, indent + 2);
    }
    break;

  case AST_ASSIGN:
    if (node->u.assign.lhs) {
      fprintf(f, ",\n%*s\"lhs\": ", indent + 2, "");
      emit_json_children(f, node->u.assign.lhs, indent + 2);
    }
    if (node->u.assign.rhs) {
      fprintf(f, ",\n%*s\"rhs\": ", indent + 2, "");
      emit_json_children(f, node->u.assign.rhs, indent + 2);
    }
    break;

  case AST_FIELD:
  case AST_INDEX:
    if (node->u.index.table) {
      fprintf(f, ",\n%*s\"table\": ", indent + 2, "");
      emit_json_node(f, node->u.index.table, 0);
    }
    if (node->u.index.key) {
      fprintf(f, ",\n%*s\"key\": ", indent + 2, "");
      emit_json_node(f, node->u.index.key, 0);
    }
    break;

  case AST_CALLEXPR:
  case AST_CALLSTAT:
  case AST_METHODCALL:
    if (node->u.call.func) {
      fprintf(f, ",\n%*s\"func\": ", indent + 2, "");
      emit_json_node(f, node->u.call.func, 0);
    }
    if (node->u.call.args) {
      fprintf(f, ",\n%*s\"args\": ", indent + 2, "");
      emit_json_children(f, node->u.call.args, indent + 2);
    }
    if (node->type == AST_METHODCALL && node->u.call.method) {
      fprintf(f, ",\n%*s\"method\": ", indent + 2, "");
      json_escape_string(f, getstr(node->u.call.method));
    }
    break;

  case AST_TABLE:
    if (node->u.table.fields) {
      fprintf(f, ",\n%*s\"fields\": ", indent + 2, "");
      emit_json_children(f, node->u.table.fields, indent + 2);
    }
    break;

  case AST_TABLEFIELD:
    if (node->u.field.key) {
      fprintf(f, ",\n%*s\"key\": ", indent + 2, "");
      emit_json_node(f, node->u.field.key, 0);
    }
    if (node->u.field.value) {
      fprintf(f, ",\n%*s\"value\": ", indent + 2, "");
      emit_json_node(f, node->u.field.value, 0);
    }
    break;

  case AST_RETURN:
    if (node->u.ret.values) {
      fprintf(f, ",\n%*s\"values\": ", indent + 2, "");
      emit_json_children(f, node->u.ret.values, indent + 2);
    }
    break;

  case AST_CATCHEXPR:
  case AST_CATCHSTAT:
    if (node->u.catchnode.expr) {
      fprintf(f, ",\n%*s\"expr\": ", indent + 2, "");
      emit_json_node(f, node->u.catchnode.expr, 0);
    }
    break;

  case AST_OPTCHAIN:
    if (node->u.optchain.base) {
      fprintf(f, ",\n%*s\"base\": ", indent + 2, "");
      emit_json_node(f, node->u.optchain.base, 0);
    }
    if (node->u.optchain.suffix) {
      fprintf(f, ",\n%*s\"suffix\": ", indent + 2, "");
      emit_json_node(f, node->u.optchain.suffix, 0);
    }
    break;

  case AST_ENUM:
    if (node->u.enumdef.names) {
      fprintf(f, ",\n%*s\"names\": ", indent + 2, "");
      emit_json_children(f, node->u.enumdef.names, indent + 2);
    }
    break;

  default:
    break;
  }

  /* Children array */
  if (node->child != NULL) {
    fprintf(f, ",\n%*s\"children\": ", indent + 2, "");
    emit_json_children(f, node->child, indent + 2);
  }

  fprintf(f, "\n%*s}", indent, "");
}

/*
** Convert AST to JSON format
*/
int lusA_tojson(LusAst *ast, const char *filename) {
  FILE *f;

  if (ast == NULL || ast->root == NULL || filename == NULL)
    return 0;

  f = fopen(filename, "w");
  if (f == NULL)
    return 0;

  emit_json_node(f, ast->root, 0);
  fprintf(f, "\n");
  fclose(f);

  return 1;
}
