/*
** lworkerlib.c
** Worker library for Lus - M:N thread pool with message passing
*/

#define lworkerlib_c
#define LUA_LIB

#include "lprefix.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lauxlib.h"
#include "lmem.h"
#include "lpledge.h"
#include "lua.h"
#include "lualib.h"
#include "lworkerlib.h"

/* Metatable name for worker userdata */
#define WORKER_METATABLE "worker.state"

/* Global worker pool (single instance) */
static WorkerPool g_pool = {0};

/* Global worker setup callback */
static lus_WorkerSetup g_worker_setup = NULL;

/*
** {======================================================
** Platform-specific helpers
** =======================================================
*/

#if defined(LUS_PLATFORM_WINDOWS)
static int get_cpu_count(void) {
  SYSTEM_INFO si;
  GetSystemInfo(&si);
  return (int)si.dwNumberOfProcessors;
}
#else
#include <unistd.h>
#if defined(_SC_NPROCESSORS_ONLN)
static int get_cpu_count(void) {
  long n = sysconf(_SC_NPROCESSORS_ONLN);
  return (n > 0) ? (int)n : 1;
}
#else
static int get_cpu_count(void) { return 4; /* fallback */ }
#endif
#endif

/* }====================================================== */

/*
** {======================================================
** Message Queue Operations
** =======================================================
*/

static void msgqueue_init(MessageQueue *q) {
  q->head = NULL;
  q->tail = NULL;
  q->count = 0;
}

static void msgqueue_push(MessageQueue *q, char *data, size_t size) {
  MessageNode *node = (MessageNode *)malloc(sizeof(MessageNode));
  node->data = data;
  node->size = size;
  node->next = NULL;
  if (q->tail) {
    q->tail->next = node;
  } else {
    q->head = node;
  }
  q->tail = node;
  q->count++;
}

static int msgqueue_pop(MessageQueue *q, char **data, size_t *size) {
  MessageNode *node = q->head;
  if (!node)
    return 0;
  q->head = node->next;
  if (!q->head)
    q->tail = NULL;
  q->count--;
  *data = node->data;
  *size = node->size;
  free(node);
  return 1;
}

static void msgqueue_clear(MessageQueue *q) {
  char *data;
  size_t size;
  while (msgqueue_pop(q, &data, &size)) {
    free(data);
  }
}

/* }====================================================== */

/*
** {======================================================
** Value Serialization
** =======================================================
*/

/* Serialization format tags */
#define SER_NIL 0
#define SER_BOOL 1
#define SER_INT 2
#define SER_NUM 3
#define SER_STRING 4
#define SER_TABLE 5

/* Buffer for serialization */
typedef struct {
  char *data;
  size_t size;
  size_t cap;
} SerBuffer;

static void serbuf_init(SerBuffer *b) {
  b->data = NULL;
  b->size = 0;
  b->cap = 0;
}

static void serbuf_ensure(lua_State *L, SerBuffer *b, size_t need) {
  if (b->size + need > b->cap) {
    size_t newcap = (b->cap == 0) ? 256 : b->cap * 2;
    while (newcap < b->size + need)
      newcap *= 2;
    b->data = luaM_reallocvchar(L, b->data, b->cap, newcap);
    b->cap = newcap;
  }
}

static void serbuf_write(lua_State *L, SerBuffer *b, const void *p, size_t n) {
  serbuf_ensure(L, b, n);
  memcpy(b->data + b->size, p, n);
  b->size += n;
}

static void serbuf_write_byte(lua_State *L, SerBuffer *b, unsigned char c) {
  serbuf_write(L, b, &c, 1);
}

/* Forward declaration for recursive serialization */
static int serialize_value(lua_State *L, int idx, SerBuffer *b, int depth);

static int serialize_table(lua_State *L, int idx, SerBuffer *b, int depth) {
  if (depth > 100) {
    return luaL_error(L, "table nesting too deep for serialization");
  }
  idx = lua_absindex(L, idx);
  serbuf_write_byte(L, b, SER_TABLE);

  /* Count entries first */
  lua_Integer count = 0;
  lua_pushnil(L);
  while (lua_next(L, idx)) {
    count++;
    lua_pop(L, 1);
  }
  serbuf_write(L, b, &count, sizeof(count));

  /* Serialize each key-value pair */
  lua_pushnil(L);
  while (lua_next(L, idx)) {
    /* key at -2, value at -1 */
    if (!serialize_value(L, -2, b, depth + 1))
      return 0;
    if (!serialize_value(L, -1, b, depth + 1))
      return 0;
    lua_pop(L, 1);
  }
  return 1;
}

static int serialize_value(lua_State *L, int idx, SerBuffer *b, int depth) {
  int t = lua_type(L, idx);
  switch (t) {
  case LUA_TNIL:
    serbuf_write_byte(L, b, SER_NIL);
    break;
  case LUA_TBOOLEAN: {
    serbuf_write_byte(L, b, SER_BOOL);
    unsigned char v = lua_toboolean(L, idx) ? 1 : 0;
    serbuf_write_byte(L, b, v);
    break;
  }
  case LUA_TNUMBER:
    if (lua_isinteger(L, idx)) {
      serbuf_write_byte(L, b, SER_INT);
      lua_Integer i = lua_tointeger(L, idx);
      serbuf_write(L, b, &i, sizeof(i));
    } else {
      serbuf_write_byte(L, b, SER_NUM);
      lua_Number n = lua_tonumber(L, idx);
      serbuf_write(L, b, &n, sizeof(n));
    }
    break;
  case LUA_TSTRING: {
    size_t len;
    const char *s = lua_tolstring(L, idx, &len);
    serbuf_write_byte(L, b, SER_STRING);
    serbuf_write(L, b, &len, sizeof(len));
    serbuf_write(L, b, s, len);
    break;
  }
  case LUA_TTABLE:
    return serialize_table(L, idx, b, depth);
  default:
    return luaL_error(L, "cannot serialize %s to worker", lua_typename(L, t));
  }
  return 1;
}

/* Deserialization */
typedef struct {
  const char *data;
  size_t size;
  size_t pos;
} DeserBuffer;

static int deser_read(DeserBuffer *b, void *out, size_t n) {
  if (b->pos + n > b->size)
    return 0;
  memcpy(out, b->data + b->pos, n);
  b->pos += n;
  return 1;
}

static int deser_read_byte(DeserBuffer *b, unsigned char *out) {
  return deser_read(b, out, 1);
}

/* Forward declaration */
static int deserialize_value(lua_State *L, DeserBuffer *b);

static int deserialize_table(lua_State *L, DeserBuffer *b) {
  lua_Integer count;
  if (!deser_read(b, &count, sizeof(count)))
    return 0;
  lua_createtable(L, 0, (int)count);
  for (lua_Integer i = 0; i < count; i++) {
    if (!deserialize_value(L, b))
      return 0; /* key */
    if (!deserialize_value(L, b))
      return 0; /* value */
    lua_settable(L, -3);
  }
  return 1;
}

static int deserialize_value(lua_State *L, DeserBuffer *b) {
  unsigned char tag;
  if (!deser_read_byte(b, &tag))
    return 0;

  switch (tag) {
  case SER_NIL:
    lua_pushnil(L);
    break;
  case SER_BOOL: {
    unsigned char v;
    if (!deser_read_byte(b, &v))
      return 0;
    lua_pushboolean(L, v);
    break;
  }
  case SER_INT: {
    lua_Integer i;
    if (!deser_read(b, &i, sizeof(i)))
      return 0;
    lua_pushinteger(L, i);
    break;
  }
  case SER_NUM: {
    lua_Number n;
    if (!deser_read(b, &n, sizeof(n)))
      return 0;
    lua_pushnumber(L, n);
    break;
  }
  case SER_STRING: {
    size_t len;
    if (!deser_read(b, &len, sizeof(len)))
      return 0;
    if (b->pos + len > b->size)
      return 0;
    lua_pushlstring(L, b->data + b->pos, len);
    b->pos += len;
    break;
  }
  case SER_TABLE:
    return deserialize_table(L, b);
  default:
    return 0;
  }
  return 1;
}

/* }====================================================== */

/*
** {======================================================
** Worker State Management
** =======================================================
*/

static WorkerState *worker_new(lua_State *parent, const char *path) {
  WorkerState *w = (WorkerState *)malloc(sizeof(WorkerState));
  if (!w)
    return NULL;

  memset(w, 0, sizeof(WorkerState));
  w->parent = parent;
  w->script_path = strdup(path);
  w->status = LUS_WORKER_RUNNING;
  w->refcount = 1;

  lus_mutex_init(&w->mutex);
  lus_cond_init(&w->outbox_cond);
  lus_cond_init(&w->inbox_cond);
  msgqueue_init(&w->outbox);
  msgqueue_init(&w->inbox);

  return w;
}

static void worker_incref(WorkerState *w) {
  lus_mutex_lock(&w->mutex);
  w->refcount++;
  lus_mutex_unlock(&w->mutex);
}

static void worker_decref(WorkerState *w) {
  int should_free = 0;
  lus_mutex_lock(&w->mutex);
  w->refcount--;
  if (w->refcount <= 0)
    should_free = 1;
  lus_mutex_unlock(&w->mutex);

  if (should_free) {
    if (w->L)
      lua_close(w->L);
    free(w->script_path);
    free(w->error_msg);
    msgqueue_clear(&w->outbox);
    msgqueue_clear(&w->inbox);
    lus_cond_destroy(&w->outbox_cond);
    lus_cond_destroy(&w->inbox_cond);
    lus_mutex_destroy(&w->mutex);
    free(w);
  }
}

/* }====================================================== */

/*
** {======================================================
** Thread Pool
** =======================================================
*/

static void pool_enqueue(WorkerState *w) {
  lus_mutex_lock(&g_pool.queue_mutex);
  w->next = NULL;
  if (g_pool.runnable_tail) {
    g_pool.runnable_tail->next = w;
  } else {
    g_pool.runnable_head = w;
  }
  g_pool.runnable_tail = w;
  lus_cond_signal(&g_pool.queue_cond);
  lus_mutex_unlock(&g_pool.queue_mutex);
}

static WorkerState *pool_dequeue(void) {
  lus_mutex_lock(&g_pool.queue_mutex);
  while (!g_pool.runnable_head && !g_pool.shutdown) {
    lus_cond_wait(&g_pool.queue_cond, &g_pool.queue_mutex);
  }
  if (g_pool.shutdown) {
    lus_mutex_unlock(&g_pool.queue_mutex);
    return NULL;
  }
  WorkerState *w = g_pool.runnable_head;
  g_pool.runnable_head = w->next;
  if (!g_pool.runnable_head)
    g_pool.runnable_tail = NULL;
  w->next = NULL;
  lus_mutex_unlock(&g_pool.queue_mutex);
  return w;
}

/* Worker thread: get global worker object to call worker.message */
static int worker_lib_message(lua_State *L) {
  /* Get worker state from registry */
  lua_getfield(L, LUA_REGISTRYINDEX, "_WORKER_STATE");
  WorkerState *w = (WorkerState *)lua_touserdata(L, -1);
  lua_pop(L, 1);
  if (!w)
    return luaL_error(L, "worker.message called outside worker context");

  /* Serialize the value */
  SerBuffer buf;
  serbuf_init(&buf);
  if (!serialize_value(L, 1, &buf, 0)) {
    free(buf.data);
    return lua_error(L);
  }

  /* Push to outbox */
  lus_mutex_lock(&w->mutex);
  msgqueue_push(&w->outbox, buf.data, buf.size);
  lus_cond_signal(&w->outbox_cond);
  lus_mutex_unlock(&w->mutex);

  return 0;
}

/* Worker thread: peek message from inbox */
static int worker_lib_peek(lua_State *L) {
  lua_getfield(L, LUA_REGISTRYINDEX, "_WORKER_STATE");
  WorkerState *w = (WorkerState *)lua_touserdata(L, -1);
  lua_pop(L, 1);
  if (!w)
    return luaL_error(L, "worker.peek called outside worker context");

  char *data;
  size_t size;

  lus_mutex_lock(&w->mutex);
  while (w->inbox.count == 0) {
    /* Block until message arrives */
    lus_cond_wait(&w->inbox_cond, &w->mutex);
  }
  msgqueue_pop(&w->inbox, &data, &size);
  lus_mutex_unlock(&w->mutex);

  /* Deserialize */
  DeserBuffer db = {data, size, 0};
  if (!deserialize_value(L, &db)) {
    free(data);
    return luaL_error(L, "failed to deserialize message");
  }
  free(data);
  return 1;
}

/* Run worker script in its own Lua state */
static void worker_run(WorkerState *w) {
  lua_State *L = luaL_newstate();
  if (!L) {
    lus_mutex_lock(&w->mutex);
    w->status = LUS_WORKER_ERROR;
    w->error_msg = strdup("failed to create Lua state");
    lus_cond_signal(&w->outbox_cond); /* wake blocked receive */
    lus_mutex_unlock(&w->mutex);
    return;
  }
  w->L = L;

  /* Call the setup callback if registered */
  if (g_worker_setup) {
    g_worker_setup(w->parent, L);
  }

  /* Store worker state pointer in registry */
  lua_pushlightuserdata(L, w);
  lua_setfield(L, LUA_REGISTRYINDEX, "_WORKER_STATE");

  /* Register worker.message and worker.peek in the worker table */
  lua_getglobal(L, "worker");
  if (!lua_istable(L, -1)) {
    lua_pop(L, 1);
    lua_newtable(L);
    lua_setglobal(L, "worker");
    lua_getglobal(L, "worker");
  }
  lua_pushcfunction(L, worker_lib_message);
  lua_setfield(L, -2, "message");
  lua_pushcfunction(L, worker_lib_peek);
  lua_setfield(L, -2, "peek");
  lua_pop(L, 1);

  /* Deserialize and push initial arguments */
  /* TODO: handle initial varargs from inbox */

  /* Load and run the script */
  if (luaL_loadfile(L, w->script_path) != LUA_OK) {
    lus_mutex_lock(&w->mutex);
    w->status = LUS_WORKER_ERROR;
    const char *err = lua_tostring(L, -1);
    w->error_msg = strdup(err ? err : "unknown load error");
    lus_cond_signal(&w->outbox_cond); /* wake blocked receive */
    lus_mutex_unlock(&w->mutex);
    return;
  }

  if (lua_pcall(L, 0, 0, 0) != LUA_OK) {
    lus_mutex_lock(&w->mutex);
    w->status = LUS_WORKER_ERROR;
    const char *err = lua_tostring(L, -1);
    w->error_msg = strdup(err ? err : "unknown runtime error");
    lus_cond_signal(&w->outbox_cond); /* wake blocked receive */
    lus_mutex_unlock(&w->mutex);
    return;
  }

  lus_mutex_lock(&w->mutex);
  w->status = LUS_WORKER_DEAD;
  lus_cond_signal(&w->outbox_cond); /* wake blocked receive */
  lus_mutex_unlock(&w->mutex);
}

/* Pool thread function */
#if defined(LUS_PLATFORM_WINDOWS)
static DWORD WINAPI pool_thread_func(LPVOID arg) {
#else
static void *pool_thread_func(void *arg) {
#endif
  (void)arg;
  while (1) {
    WorkerState *w = pool_dequeue();
    if (!w)
      break; /* shutdown */
    worker_run(w);
    worker_decref(w);
  }
#if defined(LUS_PLATFORM_WINDOWS)
  return 0;
#else
  return NULL;
#endif
}

LUA_API void lus_worker_pool_init(lua_State *L) {
  (void)L;
  if (g_pool.initialized)
    return;

  g_pool.nthreads = get_cpu_count();
  if (g_pool.nthreads < 1)
    g_pool.nthreads = 1;
  if (g_pool.nthreads > 32)
    g_pool.nthreads = 32;

  g_pool.threads =
      (lus_thread_t *)malloc(sizeof(lus_thread_t) * g_pool.nthreads);
  lus_mutex_init(&g_pool.queue_mutex);
  lus_cond_init(&g_pool.queue_cond);
  g_pool.runnable_head = NULL;
  g_pool.runnable_tail = NULL;
  g_pool.shutdown = 0;

  for (int i = 0; i < g_pool.nthreads; i++) {
#if defined(LUS_PLATFORM_WINDOWS)
    g_pool.threads[i] = CreateThread(NULL, 0, pool_thread_func, NULL, 0, NULL);
#else
    pthread_create(&g_pool.threads[i], NULL, pool_thread_func, NULL);
#endif
  }

  g_pool.initialized = 1;
}

LUA_API void lus_worker_pool_shutdown(void) {
  if (!g_pool.initialized)
    return;

  lus_mutex_lock(&g_pool.queue_mutex);
  g_pool.shutdown = 1;
  lus_cond_broadcast(&g_pool.queue_cond);
  lus_mutex_unlock(&g_pool.queue_mutex);

  for (int i = 0; i < g_pool.nthreads; i++) {
#if defined(LUS_PLATFORM_WINDOWS)
    WaitForSingleObject(g_pool.threads[i], INFINITE);
    CloseHandle(g_pool.threads[i]);
#else
    pthread_join(g_pool.threads[i], NULL);
#endif
  }

  free(g_pool.threads);
  lus_cond_destroy(&g_pool.queue_cond);
  lus_mutex_destroy(&g_pool.queue_mutex);
  g_pool.initialized = 0;
}

LUA_API void lus_onworker(lua_State *L, lus_WorkerSetup fn) {
  (void)L;
  g_worker_setup = fn;
}

/* }====================================================== */

/*
** {======================================================
** Lua Library Functions
** =======================================================
*/

static WorkerState *check_worker(lua_State *L, int idx) {
  return *(WorkerState **)luaL_checkudata(L, idx, WORKER_METATABLE);
}

/* worker.create(path, ...) */
static int lib_create(lua_State *L) {
  const char *path = luaL_checkstring(L, 1);

  /* Check permissions */
  lus_checkfsperm(L, "fs:read", path);
  if (!lus_haspledge(L, "load", NULL)) {
    return luaL_error(L, "permission denied: 'load' pledge required");
  }

  /* Initialize pool if needed */
  lus_worker_pool_init(L);

  /* Create worker state */
  WorkerState *w = worker_new(L, path);
  if (!w)
    return luaL_error(L, "failed to allocate worker");

  /* Serialize initial arguments (varargs after path) */
  int nargs = lua_gettop(L) - 1;
  for (int i = 2; i <= nargs + 1; i++) {
    SerBuffer buf;
    serbuf_init(&buf);
    if (!serialize_value(L, i, &buf, 0)) {
      worker_decref(w);
      free(buf.data);
      return lua_error(L);
    }
    lus_mutex_lock(&w->mutex);
    msgqueue_push(&w->inbox, buf.data, buf.size);
    lus_mutex_unlock(&w->mutex);
  }
  w->nargs = nargs;

  /* Create userdata */
  WorkerState **ud =
      (WorkerState **)lua_newuserdatauv(L, sizeof(WorkerState *), 0);
  *ud = w;
  worker_incref(w); /* userdata holds a reference */
  luaL_setmetatable(L, WORKER_METATABLE);

  /* Enqueue to pool */
  pool_enqueue(w);

  return 1;
}

/* worker.status(w) */
static int lib_status(lua_State *L) {
  WorkerState *w = check_worker(L, 1);
  lus_mutex_lock(&w->mutex);
  int status = w->status;
  lus_mutex_unlock(&w->mutex);

  switch (status) {
  case LUS_WORKER_RUNNING:
  case LUS_WORKER_BLOCKED:
    lua_pushliteral(L, "running");
    break;
  default:
    lua_pushliteral(L, "dead");
    break;
  }
  return 1;
}

/* worker.receive(w1, w2, ...) - select-style */
static int lib_receive(lua_State *L) {
  int nworkers = lua_gettop(L);
  if (nworkers == 0)
    return luaL_error(L, "expected at least one worker");

  WorkerState **workers =
      (WorkerState **)malloc(sizeof(WorkerState *) * nworkers);
  for (int i = 0; i < nworkers; i++) {
    workers[i] = check_worker(L, i + 1);
  }

  /* Try to get message from any worker */
  while (1) {
    int all_dead = 1;

    /* Check each worker */
    for (int i = 0; i < nworkers; i++) {
      WorkerState *w = workers[i];
      lus_mutex_lock(&w->mutex);

      if (w->status == LUS_WORKER_ERROR && w->error_msg) {
        char *errmsg = strdup(w->error_msg);
        lus_mutex_unlock(&w->mutex);
        free(workers);
        return luaL_error(L, "%s", errmsg);
      }

      if (w->status != LUS_WORKER_DEAD && w->status != LUS_WORKER_ERROR) {
        all_dead = 0;
      }

      char *data;
      size_t size;
      if (msgqueue_pop(&w->outbox, &data, &size)) {
        lus_mutex_unlock(&w->mutex);
        /* Deserialize and return */
        DeserBuffer db = {data, size, 0};
        /* Push nils for workers before this one */
        for (int j = 0; j < i; j++)
          lua_pushnil(L);
        if (!deserialize_value(L, &db)) {
          free(data);
          free(workers);
          return luaL_error(L, "failed to deserialize message");
        }
        free(data);
        /* Push nils for remaining workers */
        for (int j = i + 1; j < nworkers; j++)
          lua_pushnil(L);
        free(workers);
        return nworkers;
      }
      lus_mutex_unlock(&w->mutex);
    }

    /* If all dead and no messages, return all nils */
    if (all_dead) {
      for (int i = 0; i < nworkers; i++)
        lua_pushnil(L);
      free(workers);
      return nworkers;
    }

    /* Wait on first worker's condition (simplified) */
    /* TODO: proper multi-worker select */
    WorkerState *w = workers[0];
    lus_mutex_lock(&w->mutex);
    lus_cond_wait(&w->outbox_cond, &w->mutex);
    lus_mutex_unlock(&w->mutex);
  }
}

/* worker.send(w, value) */
static int lib_send(lua_State *L) {
  WorkerState *w = check_worker(L, 1);

  /* Serialize the value */
  SerBuffer buf;
  serbuf_init(&buf);
  if (!serialize_value(L, 2, &buf, 0)) {
    free(buf.data);
    return lua_error(L);
  }

  /* Push to inbox */
  lus_mutex_lock(&w->mutex);
  msgqueue_push(&w->inbox, buf.data, buf.size);
  lus_cond_signal(&w->inbox_cond);
  lus_mutex_unlock(&w->mutex);

  return 0;
}

/* GC metamethod */
static int worker_gc(lua_State *L) {
  WorkerState *w = check_worker(L, 1);
  worker_decref(w);
  return 0;
}

/* __tostring metamethod */
static int worker_tostring(lua_State *L) {
  WorkerState *w = check_worker(L, 1);
  lua_pushfstring(L, "worker: %p", (void *)w);
  return 1;
}

static const luaL_Reg worker_methods[] = {{"create", lib_create},
                                          {"status", lib_status},
                                          {"receive", lib_receive},
                                          {"send", lib_send},
                                          {NULL, NULL}};

static const luaL_Reg worker_meta[] = {
    {"__gc", worker_gc}, {"__tostring", worker_tostring}, {NULL, NULL}};

LUAMOD_API int luaopen_worker(lua_State *L) {
  /* Create metatable */
  luaL_newmetatable(L, WORKER_METATABLE);
  luaL_setfuncs(L, worker_meta, 0);
  lua_pop(L, 1);

  /* Create library table */
  luaL_newlib(L, worker_methods);

  /* Set as global */
  lua_pushvalue(L, -1);
  lua_setglobal(L, "worker");

  return 1;
}

/* }====================================================== */

/*
** {======================================================
** C API Implementation
** =======================================================
*/

LUA_API WorkerState *lus_worker_create(lua_State *L, const char *path) {
  lus_worker_pool_init(L);
  WorkerState *w = worker_new(L, path);
  if (w)
    pool_enqueue(w);
  return w;
}

LUA_API int lus_worker_send(lua_State *L, WorkerState *w, int idx) {
  SerBuffer buf;
  serbuf_init(&buf);
  if (!serialize_value(L, idx, &buf, 0)) {
    free(buf.data);
    return 0;
  }
  lus_mutex_lock(&w->mutex);
  msgqueue_push(&w->inbox, buf.data, buf.size);
  lus_cond_signal(&w->inbox_cond);
  lus_mutex_unlock(&w->mutex);
  return 1;
}

LUA_API int lus_worker_receive(lua_State *L, WorkerState *w) {
  char *data;
  size_t size;
  lus_mutex_lock(&w->mutex);
  int got = msgqueue_pop(&w->outbox, &data, &size);
  lus_mutex_unlock(&w->mutex);
  if (!got)
    return 0;

  DeserBuffer db = {data, size, 0};
  int ok = deserialize_value(L, &db);
  free(data);
  return ok;
}

LUA_API int lus_worker_status(WorkerState *w) {
  lus_mutex_lock(&w->mutex);
  int status = w->status;
  lus_mutex_unlock(&w->mutex);
  return status;
}

/* }====================================================== */
