#include <stdio.h>
#include <assert.h>
#include "lua.h"
#include "lualib.h"
#include "lauxlib.h"

int main(void) {
    printf("Running H2: test_stack\n");

    lua_State *L = luaL_newstate();
    assert(L != NULL);

    // Push integers
    lua_pushinteger(L, 10);
    lua_pushinteger(L, 20);
    assert(lua_gettop(L) == 2);

    // Check values
    assert(lua_tointeger(L, 1) == 10);
    assert(lua_tointeger(L, 2) == 20);

    // Pop
    lua_pop(L, 1);
    assert(lua_gettop(L) == 1);
    assert(lua_tointeger(L, 1) == 10);

    // Push string
    lua_pushstring(L, "Lus");
    assert(lua_gettop(L) == 2);
    assert(lua_isstring(L, -1));
    
    printf("stack test passed\n");

    lua_close(L);
    return 0;
}
