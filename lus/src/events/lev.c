/*
** lev.c
** Event loop scheduler for detached coroutines
**
** =======================================================
** HOW TO MAKE LUS CODE ASYNC-AWARE IN DETACHED COROUTINES
** =======================================================
**
** To make a C function async-aware for detached coroutines:
**
** 1. Check if running in detached coroutine:
**      if (is_detached(L)) { ... }
**
** 2. For I/O operations (sockets), yield with YIELD_IO:
**      set_yield_reason(L, YIELD_IO);
**      set_yield_fd(L, fd);
**      set_yield_events(L, EVLOOP_READ or EVLOOP_WRITE);
**      return lua_yield(L, 0);
**
** 3. For blocking work (file I/O, computation), use thread pool:
**      a. Create a ThreadPoolTask with work function
**      b. Submit to thread pool: threadpool_submit(pool, task)
**      c. Yield: set_yield_reason(L, YIELD_THREADPOOL)
**               set_yield_task(L, task)
**               return lua_yield(L, 0)
**      d. When scheduler_poll() detects completion, coroutine resumes
**
** 4. For sleeps, yield with YIELD_SLEEP:
**      set_yield_reason(L, YIELD_SLEEP);
**      set_yield_deadline(L, eventloop_now() + seconds);
**      return lua_yield(L, 0);
**
** KEY CONSTRAINTS:
** - Worker threads must NEVER touch lua_State (GIL constraint)
** - Only perform kernel-delegated work in threads (I/O, syscalls)
** - Lus execution and allocations remain single-threaded
** - Results from threads must be copied safely to main thread
*/

#define lev_c
#define LUA_LIB

#include "../lprefix.h"

#include <stdlib.h>
#include <string.h>

#include "../lauxlib.h"
#include "../lua.h"
#include "lev.h"
#include "lev_threadpool.h"

#if defined(LUS_PLATFORM_WINDOWS)
#include <windows.h>
#else
#include <sys/time.h>
#include <time.h>
#endif

/*
** {======================================================
** Time Utilities
** =======================================================
*/

lua_Number eventloop_now(void) {
#if defined(LUS_PLATFORM_WINDOWS)
  LARGE_INTEGER freq, count;
  QueryPerformanceFrequency(&freq);
  QueryPerformanceCounter(&count);
  return (lua_Number)count.QuadPart / (lua_Number)freq.QuadPart;
#else
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return (lua_Number)ts.tv_sec + (lua_Number)ts.tv_nsec / 1e9;
#endif
}

/* }====================================================== */

/*
** {======================================================
** Scheduler State
** =======================================================
*/

#define SCHEDULER_KEY "lus.scheduler"
#define PENDING_KEY                                                            \
  "lus.pending_coros" /* Table to anchor pending coroutines                    \
                       */

struct Scheduler {
  EventBackend *backend;
  const BackendOps *ops;
  ThreadPool *threadpool;    /* Thread pool for async work */
  PendingCoroutine *pending; /* Linked list of pending coroutines */
  int pending_count;         /* Number of pending coroutines */
  char *pending_error;       /* Error to throw on next poll() */
};

Scheduler *scheduler_get(lua_State *L) {
  lua_getfield(L, LUA_REGISTRYINDEX, SCHEDULER_KEY);
  Scheduler *sched = (Scheduler *)lua_touserdata(L, -1);
  lua_pop(L, 1);
  return sched;
}

void scheduler_init(lua_State *L) {
  /* Check if already initialized */
  lua_getfield(L, LUA_REGISTRYINDEX, SCHEDULER_KEY);
  if (!lua_isnil(L, -1)) {
    lua_pop(L, 1);
    return;
  }
  lua_pop(L, 1);

  /* Create scheduler */
  Scheduler *sched = (Scheduler *)lua_newuserdata(L, sizeof(Scheduler));
  sched->ops = eventloop_get_backend();
  sched->backend = sched->ops->create();
  sched->threadpool = threadpool_create(4); /* 4 worker threads */
  sched->pending = NULL;
  sched->pending_count = 0;
  sched->pending_error = NULL;

  if (!sched->backend) {
    if (sched->threadpool)
      threadpool_destroy(sched->threadpool);
    luaL_error(L, "failed to create event loop backend");
  }

  lua_setfield(L, LUA_REGISTRYINDEX, SCHEDULER_KEY);

  /* Create table to anchor pending coroutines (prevent GC) */
  lua_newtable(L);
  lua_setfield(L, LUA_REGISTRYINDEX, PENDING_KEY);
}

void scheduler_cleanup(lua_State *L) {
  Scheduler *sched = scheduler_get(L);
  if (sched) {
    /* Free pending list */
    PendingCoroutine *p = sched->pending;
    while (p) {
      PendingCoroutine *next = p->next;
      free(p);
      p = next;
    }
    sched->pending = NULL;
    sched->pending_count = 0;

    /* Free error message */
    if (sched->pending_error) {
      free(sched->pending_error);
      sched->pending_error = NULL;
    }

    if (sched->backend) {
      sched->ops->destroy(sched->backend);
      sched->backend = NULL;
    }

    if (sched->threadpool) {
      threadpool_destroy(sched->threadpool);
      sched->threadpool = NULL;
    }
  }
  lua_pushnil(L);
  lua_setfield(L, LUA_REGISTRYINDEX, SCHEDULER_KEY);
  lua_pushnil(L);
  lua_setfield(L, LUA_REGISTRYINDEX, PENDING_KEY);
}

ThreadPool *scheduler_get_threadpool(lua_State *L) {
  Scheduler *sched = scheduler_get(L);
  if (!sched) {
    scheduler_init(L);
    sched = scheduler_get(L);
  }
  return sched ? sched->threadpool : NULL;
}

/*
** Add a coroutine to the pending list.
** The coroutine is anchored in a registry table to prevent GC.
*/
void scheduler_add_pending(lua_State *L, lua_State *co, int fd, int events,
                           lua_Number deadline) {
  Scheduler *sched = scheduler_get(L);
  if (!sched) {
    scheduler_init(L);
    sched = scheduler_get(L);
  }

  /* Create pending entry */
  PendingCoroutine *p = (PendingCoroutine *)malloc(sizeof(PendingCoroutine));
  p->co = co;
  p->fd = fd;
  p->events = events;
  p->deadline = deadline;
  p->next = sched->pending;
  sched->pending = p;
  sched->pending_count++;

  /* Register fd with backend (if not a pure timer) */
  if (fd >= 0) {
    sched->ops->add(sched->backend, fd, events, (void *)co);
  }

  /* Anchor the coroutine in registry to prevent GC */
  lua_getfield(L, LUA_REGISTRYINDEX, PENDING_KEY);
  lua_pushlightuserdata(L, (void *)co);
  lua_pushthread(co);
  lua_xmove(co, L, 1); /* Move thread from co's stack to L */
  lua_rawset(L, -3);
  lua_pop(L, 1); /* Pop pending table */
}

/*
** Poll for I/O events and resume ready coroutines.
** Throws any errors from failed coroutines.
** Returns 0 (no Lua values pushed).
*/
int scheduler_poll(lua_State *L, int timeout_ms) {
  Scheduler *sched = scheduler_get(L);
  if (!sched) {
    return 0; /* No scheduler, nothing to do */
  }

  /* Check for pending error to throw */
  if (sched->pending_error) {
    char *err = sched->pending_error;
    sched->pending_error = NULL;
    lua_pushstring(L, err);
    free(err);
    return lua_error(L);
  }

  if (sched->pending_count == 0) {
    return 0; /* Nothing pending */
  }

  /* Calculate sleep time for timers */
  int effective_timeout = timeout_ms;
  lua_Number now = eventloop_now();
  PendingCoroutine *p;

  for (p = sched->pending; p != NULL; p = p->next) {
    if (p->deadline > 0) {
      lua_Number wait = p->deadline - now;
      if (wait <= 0) {
        effective_timeout = 0; /* Timer already expired */
        break;
      }
      int wait_ms = (int)(wait * 1000);
      if (effective_timeout < 0 || wait_ms < effective_timeout) {
        effective_timeout = wait_ms;
      }
    }
  }

  /* Wait for events */
  EventResult results[16];
  int ready = sched->ops->wait(sched->backend, results, 16, effective_timeout);

  /* Process ready events and expired timers */
  now = eventloop_now();
  PendingCoroutine **pp = &sched->pending;

  while (*pp != NULL) {
    PendingCoroutine *cur = *pp;
    int should_resume = 0;

    /* Check if timer expired */
    if (cur->deadline > 0 && now >= cur->deadline) {
      should_resume = 1;
    }

    /* Check if fd is ready */
    if (cur->fd >= 0 && ready > 0) {
      for (int i = 0; i < ready; i++) {
        if (results[i].fd == cur->fd) {
          should_resume = 1;
          break;
        }
      }
    }

    /* Check if thread pool task is done (only for YIELD_THREADPOOL) */
    if (get_yield_reason(cur->co) == YIELD_THREADPOOL) {
      ThreadPoolTask *task = get_yield_task(cur->co);
      if (task && task->done) {
        should_resume = 1;
      }
    }

    if (should_resume) {
      lua_State *co = cur->co;

      /* Remove from pending list */
      *pp = cur->next;
      sched->pending_count--;

      /* Remove fd from backend */
      if (cur->fd >= 0) {
        sched->ops->remove(sched->backend, cur->fd);
      }

      /* Unanchor from registry */
      lua_getfield(L, LUA_REGISTRYINDEX, PENDING_KEY);
      lua_pushlightuserdata(L, (void *)co);
      lua_pushnil(L);
      lua_rawset(L, -3);
      lua_pop(L, 1);

      free(cur);

      /* Clear yield state and resume */
      set_yield_reason(co, YIELD_NORMAL);
      set_yield_task(co, NULL); /* Clear any stale task pointer */

      /* Check coroutine is still resumable */
      int co_status = lua_status(co);
      if (co_status != LUA_YIELD && co_status != LUA_OK) {
        /* Coroutine is dead or errored, skip */
        continue;
      }

      /* Check if it has any frames (not dead) */
      lua_Debug ar;
      if (co_status == LUA_OK && !lua_getstack(co, 0, &ar) &&
          lua_gettop(co) == 0) {
        /* Dead coroutine */
        unmark_detached(co);
        continue;
      }

      int nres;
      int status = lua_resume(co, L, 0, &nres);

      if (status == LUA_YIELD) {
        int reason = get_yield_reason(co);
        if (reason == YIELD_IO || reason == YIELD_SLEEP ||
            reason == YIELD_THREADPOOL) {
          /* Re-add to pending list */
          scheduler_add_pending(L, co, get_yield_fd(co), get_yield_events(co),
                                get_yield_deadline(co));
        }
        /* Otherwise it's a normal yield - coroutine is suspended */
      } else if (status == LUA_OK) {
        /* Completed - unmark detached */
        unmark_detached(co);
      } else {
        /* Error - store for throwing on next poll */
        unmark_detached(co);
        const char *msg = lua_tostring(co, -1);
        if (msg) {
          sched->pending_error = strdup(msg);
        } else {
          sched->pending_error = strdup("unknown error in detached coroutine");
        }
        lua_pop(co, 1);
      }
    } else {
      pp = &(*pp)->next;
    }
  }

  /* Check for pending error to throw */
  if (sched->pending_error) {
    char *err = sched->pending_error;
    sched->pending_error = NULL;
    lua_pushstring(L, err);
    free(err);
    return lua_error(L);
  }

  return 0;
}

/*
** Return the number of pending coroutines.
*/
int scheduler_pending_count(lua_State *L) {
  Scheduler *sched = scheduler_get(L);
  return sched ? sched->pending_count : 0;
}

/* }====================================================== */

/*
** {======================================================
** Detached Coroutine State
** =======================================================
** We store per-coroutine state in the registry keyed by the coroutine thread.
*/

typedef struct {
  int detached;
  int yield_reason;
  int yield_fd;
  int yield_events;
  lua_Number yield_deadline;
  ThreadPoolTask *yield_task; /* Thread pool task (for YIELD_THREADPOOL) */
} CoroutineState;

#define COROSTATE_KEY "lus.corostate"

static CoroutineState *get_corostate(lua_State *co, int create) {
  lua_getfield(co, LUA_REGISTRYINDEX, COROSTATE_KEY);
  if (lua_isnil(co, -1)) {
    lua_pop(co, 1);
    if (create) {
      lua_newtable(co);
      lua_setfield(co, LUA_REGISTRYINDEX, COROSTATE_KEY);
      lua_getfield(co, LUA_REGISTRYINDEX, COROSTATE_KEY);
    } else {
      return NULL;
    }
  }

  /* Get state for this specific coroutine */
  lua_pushlightuserdata(co, (void *)co);
  lua_rawget(co, -2);

  if (lua_isnil(co, -1)) {
    lua_pop(co, 1);
    if (create) {
      CoroutineState *cs =
          (CoroutineState *)lua_newuserdata(co, sizeof(CoroutineState));
      memset(cs, 0, sizeof(CoroutineState));
      lua_pushlightuserdata(co, (void *)co);
      lua_pushvalue(co, -2);
      lua_rawset(co, -4);
      lua_pop(co, 2); /* Pop userdata and COROSTATE_KEY table */
      return cs;
    } else {
      lua_pop(co, 1); /* Remove COROSTATE_KEY table */
      return NULL;
    }
  }

  CoroutineState *cs = (CoroutineState *)lua_touserdata(co, -1);
  lua_pop(co, 2); /* Pop state and COROSTATE_KEY table */
  return cs;
}

int is_detached(lua_State *co) {
  CoroutineState *cs = get_corostate(co, 0);
  return cs && cs->detached;
}

void mark_detached(lua_State *co) {
  CoroutineState *cs = get_corostate(co, 1);
  if (cs)
    cs->detached = 1;
}

void unmark_detached(lua_State *co) {
  CoroutineState *cs = get_corostate(co, 0);
  if (cs)
    cs->detached = 0;
}

void set_yield_reason(lua_State *co, int reason) {
  CoroutineState *cs = get_corostate(co, 1);
  if (cs)
    cs->yield_reason = reason;
}

int get_yield_reason(lua_State *co) {
  CoroutineState *cs = get_corostate(co, 0);
  return cs ? cs->yield_reason : YIELD_NORMAL;
}

void set_yield_fd(lua_State *co, int fd) {
  CoroutineState *cs = get_corostate(co, 1);
  if (cs)
    cs->yield_fd = fd;
}

int get_yield_fd(lua_State *co) {
  CoroutineState *cs = get_corostate(co, 0);
  return cs ? cs->yield_fd : -1;
}

void set_yield_events(lua_State *co, int events) {
  CoroutineState *cs = get_corostate(co, 1);
  if (cs)
    cs->yield_events = events;
}

int get_yield_events(lua_State *co) {
  CoroutineState *cs = get_corostate(co, 0);
  return cs ? cs->yield_events : 0;
}

void set_yield_deadline(lua_State *co, lua_Number deadline) {
  CoroutineState *cs = get_corostate(co, 1);
  if (cs)
    cs->yield_deadline = deadline;
}

lua_Number get_yield_deadline(lua_State *co) {
  CoroutineState *cs = get_corostate(co, 0);
  return cs ? cs->yield_deadline : 0;
}

void set_yield_task(lua_State *co, ThreadPoolTask *task) {
  CoroutineState *cs = get_corostate(co, 1);
  if (cs)
    cs->yield_task = task;
}

ThreadPoolTask *get_yield_task(lua_State *co) {
  CoroutineState *cs = get_corostate(co, 0);
  return cs ? cs->yield_task : NULL;
}

/* }====================================================== */

/*
** {======================================================
** Event-Driven Resume
** =======================================================
*/

int detached_resume(lua_State *L, lua_State *co, int nargs) {
  Scheduler *sched = scheduler_get(L);
  if (!sched || !sched->backend) {
    scheduler_init(L);
    sched = scheduler_get(L);
  }

  int resume_nargs = nargs;

  for (;;) {
    int nres;
    int status = lua_resume(co, L, resume_nargs, &nres);

    if (status == LUA_YIELD) {
      int reason = get_yield_reason(co);

      if (reason == YIELD_IO) {
        /* Wait for I/O event */
        int fd = get_yield_fd(co);
        int events = get_yield_events(co);

        /* Register fd with backend */
        sched->ops->add(sched->backend, fd, events, NULL);

        /* Wait for event */
        EventResult result;
        int ready = sched->ops->wait(sched->backend, &result, 1, -1);

        /* Remove fd from backend */
        sched->ops->remove(sched->backend, fd);

        if (ready < 0) {
          /* Error during wait */
          lua_pushstring(L, "I/O wait error");
          return lua_error(L);
        }

        /* Clear yield reason and continue */
        set_yield_reason(co, YIELD_NORMAL);
        resume_nargs = 0;
        continue;
      } else if (reason == YIELD_SLEEP) {
        /* Wait for timer */
        lua_Number deadline = get_yield_deadline(co);
        lua_Number now = eventloop_now();

        if (deadline > now) {
          int wait_ms = (int)((deadline - now) * 1000);
          if (wait_ms > 0) {
            /* Just wait (no fd) */
            EventResult result;
            sched->ops->wait(sched->backend, &result, 0, wait_ms);
          }
        }

        /* Clear yield reason and continue */
        set_yield_reason(co, YIELD_NORMAL);
        resume_nargs = 0;
        continue;
      } else {
        /* Normal yield - return to caller */
        /* Transfer results from co to L */
        lua_xmove(co, L, nres);
        return nres;
      }
    } else if (status == LUA_OK) {
      /* Coroutine completed - transfer results */
      lua_xmove(co, L, nres);
      unmark_detached(co);
      return nres;
    } else {
      /* Error - propagate to caller */
      lua_xmove(co, L, 1); /* Move error message */
      unmark_detached(co);
      return lua_error(L);
    }
  }
}

/* }====================================================== */
