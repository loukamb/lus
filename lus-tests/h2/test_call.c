#include <stdio.h>
#include <assert.h>
#include "lua.h"
#include "lualib.h"
#include "lauxlib.h"

int main(void) {
    printf("Running H2: test_call\n");

    lua_State *L = luaL_newstate();
    luaL_openlibs(L);

    // Load a simple Lus script that defines a function
    const char *code = "function add(a, b) return a + b end";
    if (luaL_dostring(L, code) != LUA_OK) {
        fprintf(stderr, "Failed to load code: %s\n", lua_tostring(L, -1));
        return 1;
    }

    // Call the function
    lua_getglobal(L, "add");
    lua_pushinteger(L, 10);
    lua_pushinteger(L, 32);

    if (lua_pcall(L, 2, 1, 0) != LUA_OK) {
        fprintf(stderr, "Failed to call function: %s\n", lua_tostring(L, -1));
        return 1;
    }

    // Check result
    if (!lua_isinteger(L, -1)) {
        fprintf(stderr, "Return value is not integer\n");
        return 1;
    }

    int result = lua_tointeger(L, -1);
    assert(result == 42);

    printf("call test passed\n");

    lua_close(L);
    return 0;
}
