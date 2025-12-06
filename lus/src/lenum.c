/*
** $Id: lenum.c $
** Enum handling
** See Copyright Notice in lua.h
*/

#define lenum_c
#define LUA_CORE

#include "lprefix.h"

#include <string.h>

#include "lua.h"

#include "lenum.h"
#include "lgc.h"
#include "lmem.h"
#include "lobject.h"
#include "lstate.h"
#include "lstring.h"


/*
** Create a new EnumRoot with 'size' names.
** The names array is uninitialized and should be filled by the caller.
*/
EnumRoot *luaE_newroot(lua_State *L, int size) {
  size_t sz = sizeenumroot(size);
  GCObject *o = luaC_newobjdt(L, LUA_VENUMROOT, sz, 0);
  EnumRoot *root = gco2enumroot(o);
  root->size = size;
  root->gclist = NULL;
  /* Initialize names to NULL (caller will fill them in) */
  for (int i = 0; i < size; i++)
    root->names[i] = NULL;
  return root;
}


/*
** Create a new Enum value with the given root and 1-based index.
*/
Enum *luaE_new(lua_State *L, EnumRoot *root, int idx) {
  GCObject *o = luaC_newobj(L, LUA_VENUM, sizeenum);
  Enum *e = gco2enum(o);
  e->root = root;
  e->idx = idx;
  return e;
}


/*
** Look up an enum value by name string.
** Returns the 1-based index if found, 0 if not found.
*/
int luaE_findname(EnumRoot *root, TString *name) {
  int i;
  for (i = 0; i < root->size; i++) {
    if (root->names[i] == name) /* short strings are interned */
      return i + 1;             /* 1-based index */
  }
  return 0; /* not found */
}


/*
** Get an enum value from the root by string key.
** Creates and returns a new Enum value, or returns NULL if key not found.
*/
Enum *luaE_getbyname(lua_State *L, EnumRoot *root, TString *key) {
  int idx = luaE_findname(root, key);
  if (idx == 0)
    return NULL; /* not found */
  return luaE_new(L, root, idx);
}


/*
** Get an enum value from the root by integer index.
** Creates and returns a new Enum value, or returns NULL if index out of bounds.
*/
Enum *luaE_getbyidx(lua_State *L, EnumRoot *root, int idx) {
  if (idx < 1 || idx > root->size)
    return NULL; /* out of bounds */
  return luaE_new(L, root, idx);
}


/*
** Free an EnumRoot.
*/
void luaE_freeroot(lua_State *L, EnumRoot *root) {
  luaM_freemem(L, root, sizeenumroot(root->size));
}


/*
** Free an Enum value.
*/
void luaE_free(lua_State *L, Enum *e) {
  luaM_freemem(L, e, sizeenum);
}


/*
** Get the size (in bytes) of an EnumRoot.
*/
lu_mem luaE_rootsize(EnumRoot *root) {
  return cast(lu_mem, sizeenumroot(root->size));
}
