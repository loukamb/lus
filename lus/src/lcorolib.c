/*
** $Id: lcorolib.c $
** Coroutine Library
** See Copyright Notice in lua.h
*/

#define lcorolib_c
#define LUA_LIB

#include "lprefix.h"

#include <stdlib.h>

#include "lua.h"

#include "events/lev.h"
#include "lauxlib.h"
#include "llimits.h"
#include "lualib.h"

static lua_State *getco(lua_State *L) {
  lua_State *co = lua_tothread(L, 1);
  luaL_argexpected(L, co, 1, "thread");
  return co;
}

/*
** Resumes a coroutine. Returns the number of results for non-error
** cases or -1 for errors.
*/
static int auxresume(lua_State *L, lua_State *co, int narg) {
  int status, nres;
  if (l_unlikely(!lua_checkstack(co, narg))) {
    lua_pushliteral(L, "too many arguments to resume");
    return -1; /* error flag */
  }
  lua_xmove(L, co, narg);
  status = lua_resume(co, L, narg, &nres);
  if (l_likely(status == LUA_OK || status == LUA_YIELD)) {
    if (l_unlikely(!lua_checkstack(L, nres + 1))) {
      lua_pop(co, nres); /* remove results anyway */
      lua_pushliteral(L, "too many results to resume");
      return -1; /* error flag */
    }
    lua_xmove(co, L, nres); /* move yielded values */
    return nres;
  } else {
    lua_xmove(co, L, 1); /* move error message */
    return -1;           /* error flag */
  }
}

static int luaB_coresume(lua_State *L) {
  lua_State *co = getco(L);
  int nargs = lua_gettop(L) - 1;
  int r;

  /* Check if this is a detached coroutine */
  if (is_detached(co)) {
    /* Move arguments to coroutine */
    if (nargs > 0) {
      if (l_unlikely(!lua_checkstack(co, nargs))) {
        lua_pushboolean(L, 0);
        lua_pushliteral(L, "too many arguments to resume");
        return 2;
      }
      lua_xmove(L, co, nargs);
    }
    /* Use event-driven resume */
    r = detached_resume(L, co, nargs);
    /* detached_resume either returns results or errors via lua_error */
    lua_pushboolean(L, 1);
    lua_insert(L, -(r + 1));
    return r + 1;
  }

  /* Normal resume for non-detached coroutines */
  r = auxresume(L, co, nargs);
  if (l_unlikely(r < 0)) {
    lua_pushboolean(L, 0);
    lua_insert(L, -2);
    return 2; /* return false + error message */
  } else {
    lua_pushboolean(L, 1);
    lua_insert(L, -(r + 1));
    return r + 1; /* return true + 'resume' returns */
  }
}

static int luaB_auxwrap(lua_State *L) {
  lua_State *co = lua_tothread(L, lua_upvalueindex(1));
  int r = auxresume(L, co, lua_gettop(L));
  if (l_unlikely(r < 0)) { /* error? */
    int stat = lua_status(co);
    if (stat != LUA_OK && stat != LUA_YIELD) { /* error in the coroutine? */
      stat = lua_closethread(co, L);           /* close its tbc variables */
      lua_assert(stat != LUA_OK);
      lua_xmove(co, L, 1); /* move error message to the caller */
    }
    if (stat != LUA_ERRMEM &&             /* not a memory error and ... */
        lua_type(L, -1) == LUA_TSTRING) { /* ... error object is a string? */
      luaL_where(L, 1);                   /* add extra info, if available */
      lua_insert(L, -2);
      lua_concat(L, 2);
    }
    return lua_error(L); /* propagate error */
  }
  return r;
}

static int luaB_cocreate(lua_State *L) {
  lua_State *NL;
  luaL_checktype(L, 1, LUA_TFUNCTION);
  NL = lua_newthread(L);
  lua_pushvalue(L, 1); /* move function to top */
  lua_xmove(L, NL, 1); /* move function from L to NL */
  return 1;
}

static int luaB_cowrap(lua_State *L) {
  luaB_cocreate(L);
  lua_pushcclosure(L, luaB_auxwrap, 1);
  return 1;
}

static int luaB_yield(lua_State *L) { return lua_yield(L, lua_gettop(L)); }

#define COS_RUN 0
#define COS_DEAD 1
#define COS_YIELD 2
#define COS_NORM 3

static const char *const statname[] = {"running", "dead", "suspended",
                                       "normal"};

static int auxstatus(lua_State *L, lua_State *co) {
  if (L == co)
    return COS_RUN;
  else {
    switch (lua_status(co)) {
    case LUA_YIELD:
      return COS_YIELD;
    case LUA_OK: {
      lua_Debug ar;
      if (lua_getstack(co, 0, &ar)) /* does it have frames? */
        return COS_NORM;            /* it is running */
      else if (lua_gettop(co) == 0)
        return COS_DEAD;
      else
        return COS_YIELD; /* initial state */
    }
    default: /* some error occurred */
      return COS_DEAD;
    }
  }
}

static int luaB_costatus(lua_State *L) {
  lua_State *co = getco(L);
  lua_pushstring(L, statname[auxstatus(L, co)]);
  return 1;
}

static lua_State *getoptco(lua_State *L) {
  return (lua_isnone(L, 1) ? L : getco(L));
}

static int luaB_yieldable(lua_State *L) {
  lua_State *co = getoptco(L);
  lua_pushboolean(L, lua_isyieldable(co));
  return 1;
}

static int luaB_corunning(lua_State *L) {
  int ismain = lua_pushthread(L);
  lua_pushboolean(L, ismain);
  return 2;
}

static int luaB_close(lua_State *L) {
  lua_State *co = getoptco(L);
  int status = auxstatus(L, co);
  switch (status) {
  case COS_DEAD:
  case COS_YIELD: {
    status = lua_closethread(co, L);
    if (status == LUA_OK) {
      lua_pushboolean(L, 1);
      return 1;
    } else {
      lua_pushboolean(L, 0);
      lua_xmove(co, L, 1); /* move error message */
      return 2;
    }
  }
  case COS_NORM:
    return luaL_error(L, "cannot close a %s coroutine", statname[status]);
  case COS_RUN:
    lua_geti(L, LUA_REGISTRYINDEX, LUA_RIDX_MAINTHREAD); /* get main */
    if (lua_tothread(L, -1) == co)
      return luaL_error(L, "cannot close main thread");
    lua_closethread(co, L);             /* close itself */
    /* previous call does not return */ /* FALLTHROUGH */
  default:
    lua_assert(0);
    return 0;
  }
}

/*
** coroutine.detach(co)
** Start a coroutine for event-driven execution.
** Runs immediately until async I/O or completion, then returns.
** Returns: status_e enum value, followed by results if applicable.
*/
static int luaB_detach(lua_State *L) {
  lua_State *co = getco(L);
  int status = auxstatus(L, co);
  int nres;

  if (status == COS_DEAD) {
    return luaL_error(L, "cannot detach a dead coroutine");
  }
  if (status == COS_RUN) {
    return luaL_error(L, "cannot detach a running coroutine");
  }

  mark_detached(co);

  /* Start the coroutine immediately */
  int lua_status = lua_resume(co, L, 0, &nres);

  if (lua_status == LUA_YIELD) {
    int reason = get_yield_reason(co);
    if (reason == YIELD_IO || reason == YIELD_SLEEP ||
        reason == YIELD_THREADPOOL) {
      /* Async I/O or thread pool - add to pending list and return
       * status_e.pending */
      int fd = get_yield_fd(co);
      int events = get_yield_events(co);
      lua_Number deadline = get_yield_deadline(co);
      scheduler_add_pending(L, co, fd, events, deadline);

      /* Get status_e.pending enum value */
      lua_getfield(L, LUA_REGISTRYINDEX, "coroutine.status_e");
      lua_getfield(L, -1, "pending");
      lua_remove(L, -2); /* Remove the enum table */
      return 1;
    } else {
      /* Normal yield - return status_e.yielded + values */
      lua_getfield(L, LUA_REGISTRYINDEX, "coroutine.status_e");
      lua_getfield(L, -1, "yielded");
      lua_remove(L, -2);
      lua_xmove(co, L, nres);
      return 1 + nres;
    }
  } else if (lua_status == LUA_OK) {
    /* Completed synchronously - return status_e.completed + results */
    unmark_detached(co);
    lua_getfield(L, LUA_REGISTRYINDEX, "coroutine.status_e");
    lua_getfield(L, -1, "completed");
    lua_remove(L, -2);
    lua_xmove(co, L, nres);
    return 1 + nres;
  } else {
    /* Error - return status_e.error + error message */
    unmark_detached(co);
    lua_getfield(L, LUA_REGISTRYINDEX, "coroutine.status_e");
    lua_getfield(L, -1, "error");
    lua_remove(L, -2);
    lua_xmove(co, L, 1); /* Move error message */
    return 2;
  }
}

/*
** coroutine.sleep(seconds)
** Yield for a specified duration. Only works in detached coroutines.
*/
static int luaB_sleep(lua_State *L) {
  lua_Number seconds = luaL_checknumber(L, 1);

  if (!is_detached(L)) {
    return luaL_error(L, "coroutine.sleep only works in detached coroutines");
  }

  if (seconds < 0) {
    return luaL_error(L, "sleep duration must be non-negative");
  }

  /* Set up sleep yield */
  lua_Number deadline = eventloop_now() + seconds;
  set_yield_reason(L, YIELD_SLEEP);
  set_yield_deadline(L, deadline);

  return lua_yield(L, 0);
}

/*
** coroutine.poll([timeout])
** Process pending I/O and resume ready coroutines.
** timeout: optional, seconds to wait. nil = non-blocking, -1 = block
*indefinitely
** Throws any errors from detached coroutines.
*/
static int luaB_poll(lua_State *L) {
  int timeout_ms = 0;

  if (!lua_isnoneornil(L, 1)) {
    lua_Number timeout = luaL_checknumber(L, 1);
    if (timeout < 0) {
      timeout_ms = -1; /* Block indefinitely */
    } else {
      timeout_ms = (int)(timeout * 1000);
    }
  }

  return scheduler_poll(L, timeout_ms);
}

/*
** coroutine.pending()
** Returns the number of coroutines waiting on I/O.
*/
static int luaB_pending(lua_State *L) {
  lua_pushinteger(L, scheduler_pending_count(L));
  return 1;
}

static const luaL_Reg co_funcs[] = {{"create", luaB_cocreate},
                                    {"resume", luaB_coresume},
                                    {"running", luaB_corunning},
                                    {"status", luaB_costatus},
                                    {"wrap", luaB_cowrap},
                                    {"yield", luaB_yield},
                                    {"isyieldable", luaB_yieldable},
                                    {"close", luaB_close},
                                    {"detach", luaB_detach},
                                    {"sleep", luaB_sleep},
                                    {"poll", luaB_poll},
                                    {"pending", luaB_pending},
                                    {NULL, NULL}};

LUAMOD_API int luaopen_coroutine(lua_State *L) {
  luaL_newlib(L, co_funcs);

  /* Create status_e enum: pending, completed, yielded, error */
  lua_pushstring(L, "pending");
  lua_pushinteger(L, 1);
  lua_pushstring(L, "completed");
  lua_pushinteger(L, 2);
  lua_pushstring(L, "yielded");
  lua_pushinteger(L, 3);
  lua_pushstring(L, "error");
  lua_pushinteger(L, 4);
  lua_pushenum(L, 4);

  /* Store in registry for detach() to access */
  lua_pushvalue(L, -1);
  lua_setfield(L, LUA_REGISTRYINDEX, "coroutine.status_e");

  /* Also add to coroutine table */
  lua_setfield(L, -2, "status_e");

  return 1;
}
