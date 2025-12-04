/*
** Lus WebAssembly wrapper
** Provides a simple API for executing Lus code in the browser
*/

#include <stdlib.h>
#include <string.h>

#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

/* Output buffer for capturing print results */
#define OUTPUT_BUFFER_SIZE 65536

typedef struct {
  lua_State *L;
  char output[OUTPUT_BUFFER_SIZE];
  size_t output_len;
} LusState;

/* Custom print function that captures output */
static int wasm_print(lua_State *L) {
  LusState *state = (LusState *)lua_touserdata(L, lua_upvalueindex(1));
  int n = lua_gettop(L);
  int i;

  for (i = 1; i <= n; i++) {
    size_t len;
    const char *s = luaL_tolstring(L, i, &len);

    if (i > 1) {
      if (state->output_len < OUTPUT_BUFFER_SIZE - 1) {
        state->output[state->output_len++] = '\t';
      }
    }

    if (state->output_len + len < OUTPUT_BUFFER_SIZE - 1) {
      memcpy(state->output + state->output_len, s, len);
      state->output_len += len;
    }

    lua_pop(L, 1); /* pop result of luaL_tolstring */
  }

  if (state->output_len < OUTPUT_BUFFER_SIZE - 1) {
    state->output[state->output_len++] = '\n';
  }

  state->output[state->output_len] = '\0';
  return 0;
}

/* Create a new Lus state */
LusState *lus_create(void) {
  LusState *state = (LusState *)malloc(sizeof(LusState));
  if (!state) return NULL;

  state->L = luaL_newstate();
  if (!state->L) {
    free(state);
    return NULL;
  }

  state->output[0] = '\0';
  state->output_len = 0;

  /* Open safe libraries (no io, os.execute, loadfile, etc.) */
  luaL_requiref(state->L, LUA_GNAME, luaopen_base, 1);
  lua_pop(state->L, 1);
  luaL_requiref(state->L, LUA_TABLIBNAME, luaopen_table, 1);
  lua_pop(state->L, 1);
  luaL_requiref(state->L, LUA_STRLIBNAME, luaopen_string, 1);
  lua_pop(state->L, 1);
  luaL_requiref(state->L, LUA_MATHLIBNAME, luaopen_math, 1);
  lua_pop(state->L, 1);
  luaL_requiref(state->L, LUA_UTF8LIBNAME, luaopen_utf8, 1);
  lua_pop(state->L, 1);
  luaL_requiref(state->L, LUA_COLIBNAME, luaopen_coroutine, 1);
  lua_pop(state->L, 1);

  /* Remove dangerous functions from base */
  lua_pushnil(state->L);
  lua_setglobal(state->L, "dofile");
  lua_pushnil(state->L);
  lua_setglobal(state->L, "loadfile");

  /* Override print to capture output */
  lua_pushlightuserdata(state->L, state);
  lua_pushcclosure(state->L, wasm_print, 1);
  lua_setglobal(state->L, "print");

  return state;
}

/* Execute Lus code and return output (or error message) */
const char *lus_execute(LusState *state, const char *code) {
  if (!state || !state->L) return "Error: Invalid state";

  /* Clear output buffer */
  state->output[0] = '\0';
  state->output_len = 0;

  /* Try to load as expression first (prepend "return ") */
  size_t code_len = strlen(code);
  char *expr_code = (char *)malloc(code_len + 8);
  if (expr_code) {
    strcpy(expr_code, "return ");
    strcat(expr_code, code);

    int status = luaL_loadstring(state->L, expr_code);
    free(expr_code);

    if (status == LUA_OK) {
      /* Expression loaded, execute it */
      status = lua_pcall(state->L, 0, LUA_MULTRET, 0);
      if (status == LUA_OK) {
        /* Print any return values */
        int nresults = lua_gettop(state->L);
        if (nresults > 0) {
          lua_getglobal(state->L, "print");
          lua_insert(state->L, 1);
          lua_pcall(state->L, nresults, 0, 0);
        }
        return state->output;
      }
      /* Execution failed, get error */
      goto handle_error;
    }

    /* Not a valid expression, clear stack and try as statement */
    lua_settop(state->L, 0);
  }

  /* Load as statement */
  int status = luaL_loadstring(state->L, code);
  if (status != LUA_OK) {
    goto handle_error;
  }

  /* Execute */
  status = lua_pcall(state->L, 0, LUA_MULTRET, 0);
  if (status != LUA_OK) {
    goto handle_error;
  }

  return state->output;

handle_error:
  {
    const char *err = lua_tostring(state->L, -1);
    if (err && state->output_len + strlen(err) < OUTPUT_BUFFER_SIZE - 1) {
      strcpy(state->output + state->output_len, err);
    }
    lua_pop(state->L, 1);
    return state->output;
  }
}

/* Destroy a Lus state */
void lus_destroy(LusState *state) {
  if (state) {
    if (state->L) {
      lua_close(state->L);
    }
    free(state);
  }
}

