/*
** WASM stubs for event loop functions.
** In WASM/browser context, async I/O with thread pools is not supported.
** These stubs ensure is_detached() returns 0, so all I/O stays synchronous.
*/

#include "lua.h"

/* Forward declare opaque types */
typedef struct ThreadPool ThreadPool;
typedef struct ThreadPoolTask ThreadPoolTask;

/* Stub: coroutines are never "detached" in WASM */
int is_detached(lua_State *L) {
  (void)L;
  return 0;
}

/* Scheduler stubs - no-op in WASM */
void detached_resume(lua_State *co, lua_State *from, int nargs) {
  (void)co;
  (void)from;
  (void)nargs;
}

void mark_detached(lua_State *co) { (void)co; }

void unmark_detached(lua_State *co) { (void)co; }

int get_yield_reason(lua_State *co) {
  (void)co;
  return 0;
}

int get_yield_fd(lua_State *co) {
  (void)co;
  return -1;
}

int get_yield_events(lua_State *co) {
  (void)co;
  return 0;
}

lua_Number get_yield_deadline(lua_State *co) {
  (void)co;
  return 0;
}

void set_yield_reason(lua_State *co, int reason) {
  (void)co;
  (void)reason;
}

void set_yield_fd(lua_State *co, int fd) {
  (void)co;
  (void)fd;
}

void set_yield_events(lua_State *co, int events) {
  (void)co;
  (void)events;
}

void set_yield_deadline(lua_State *co, lua_Number deadline) {
  (void)co;
  (void)deadline;
}

void set_yield_task(lua_State *co, ThreadPoolTask *task) {
  (void)co;
  (void)task;
}

ThreadPoolTask *get_yield_task(lua_State *co) {
  (void)co;
  return NULL;
}

void scheduler_add_pending(lua_State *L, lua_State *co, int fd, int events,
                           lua_Number deadline) {
  (void)L;
  (void)co;
  (void)fd;
  (void)events;
  (void)deadline;
}

int scheduler_poll(lua_State *L, lua_Number timeout) {
  (void)L;
  (void)timeout;
  return 0;
}

int scheduler_pending_count(lua_State *L) {
  (void)L;
  return 0;
}

lua_Number eventloop_now(void) { return 0; }

/* Thread pool stubs */
ThreadPool *scheduler_get_threadpool(lua_State *L) {
  (void)L;
  return NULL; /* No thread pool in WASM */
}

/* Network stubs */
int luaopen_network(lua_State *L) {
  (void)L;
  return 0; /* Network not available in WASM */
}
