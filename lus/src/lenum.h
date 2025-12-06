/*
** $Id: lenum.h $
** Enum handling
** See Copyright Notice in lua.h
*/

#ifndef lenum_h
#define lenum_h

#include "lobject.h"


/*
** Create a new EnumRoot with 'size' names.
** The names array should be populated after creation.
*/
LUAI_FUNC EnumRoot *luaE_newroot(lua_State *L, int size);

/*
** Create a new Enum value with the given root and 1-based index.
*/
LUAI_FUNC Enum *luaE_new(lua_State *L, EnumRoot *root, int idx);

/*
** Look up an enum value by name string.
** Returns the 1-based index if found, 0 if not found.
*/
LUAI_FUNC int luaE_findname(EnumRoot *root, TString *name);

/*
** Get an enum value from the root by string key.
** Creates and returns a new Enum value, or returns NULL if key not found.
*/
LUAI_FUNC Enum *luaE_getbyname(lua_State *L, EnumRoot *root, TString *key);

/*
** Get an enum value from the root by integer index.
** Creates and returns a new Enum value, or returns NULL if index out of bounds.
*/
LUAI_FUNC Enum *luaE_getbyidx(lua_State *L, EnumRoot *root, int idx);

/*
** Free an EnumRoot.
*/
LUAI_FUNC void luaE_freeroot(lua_State *L, EnumRoot *root);

/*
** Free an Enum value.
*/
LUAI_FUNC void luaE_free(lua_State *L, Enum *e);

/*
** Get the size (in bytes) of an EnumRoot.
*/
LUAI_FUNC lu_mem luaE_rootsize(EnumRoot *root);


#endif
