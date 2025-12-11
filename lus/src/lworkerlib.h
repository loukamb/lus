/*
** lworkerlib.h
** Worker library for Lus - M:N thread pool with message passing
*/

#ifndef lworkerlib_h
#define lworkerlib_h

#include "lua.h"

/* Platform-specific threading primitives */
#if defined(LUS_PLATFORM_WINDOWS)
#include <windows.h>
typedef HANDLE lus_thread_t;
typedef CRITICAL_SECTION lus_mutex_t;
typedef CONDITION_VARIABLE lus_cond_t;
#define lus_mutex_init(m) InitializeCriticalSection(m)
#define lus_mutex_destroy(m) DeleteCriticalSection(m)
#define lus_mutex_lock(m) EnterCriticalSection(m)
#define lus_mutex_unlock(m) LeaveCriticalSection(m)
#define lus_cond_init(c) InitializeConditionVariable(c)
#define lus_cond_destroy(c) ((void)0)
#define lus_cond_wait(c, m) SleepConditionVariableCS(c, m, INFINITE)
#define lus_cond_signal(c) WakeConditionVariable(c)
#define lus_cond_broadcast(c) WakeAllConditionVariable(c)
#else
#include <pthread.h>
typedef pthread_t lus_thread_t;
typedef pthread_mutex_t lus_mutex_t;
typedef pthread_cond_t lus_cond_t;
#define lus_mutex_init(m) pthread_mutex_init(m, NULL)
#define lus_mutex_destroy(m) pthread_mutex_destroy(m)
#define lus_mutex_lock(m) pthread_mutex_lock(m)
#define lus_mutex_unlock(m) pthread_mutex_unlock(m)
#define lus_cond_init(c) pthread_cond_init(c, NULL)
#define lus_cond_destroy(c) pthread_cond_destroy(c)
#define lus_cond_wait(c, m) pthread_cond_wait(c, m)
#define lus_cond_signal(c) pthread_cond_signal(c)
#define lus_cond_broadcast(c) pthread_cond_broadcast(c)
#endif

/* Worker status constants */
#define LUS_WORKER_RUNNING 0
#define LUS_WORKER_BLOCKED 1
#define LUS_WORKER_DEAD 2
#define LUS_WORKER_ERROR 3

/* Forward declarations */
typedef struct WorkerPool WorkerPool;
typedef struct WorkerState WorkerState;
typedef struct MessageNode MessageNode;

/*
** Message queue node - holds serialized Lua value
*/
struct MessageNode {
  char *data;        /* serialized value buffer */
  size_t size;       /* buffer size */
  MessageNode *next; /* linked list */
};

/*
** Message queue with synchronization
*/
typedef struct MessageQueue {
  MessageNode *head;
  MessageNode *tail;
  int count;
} MessageQueue;

/*
** Worker state (N workers scheduled onto M threads)
*/
struct WorkerState {
  lua_State *L;             /* worker's Lua state */
  lua_State *parent;        /* parent state (for permission inheritance) */
  struct WorkerState *next; /* for runnable queue or GC list */
  lus_mutex_t mutex;        /* protects this worker's state */
  lus_cond_t outbox_cond;   /* signal: message in outbox */
  lus_cond_t inbox_cond;    /* signal: message in inbox */
  MessageQueue outbox;      /* worker → main */
  MessageQueue inbox;       /* main → worker */
  int status;               /* LUS_WORKER_* status */
  char *error_msg;          /* error message if status == ERROR */
  char *script_path;        /* path to worker script */
  int nargs;                /* number of arguments (for initial varargs) */
  int refcount;             /* reference count */
};

/*
** Global thread pool (M threads servicing N workers)
*/
struct WorkerPool {
  lus_thread_t *threads;      /* array of M OS threads */
  int nthreads;               /* M = num CPU cores */
  lus_mutex_t queue_mutex;    /* protects runnable queue */
  lus_cond_t queue_cond;      /* signal when work available */
  WorkerState *runnable_head; /* queue of runnable workers */
  WorkerState *runnable_tail;
  int shutdown;    /* 1 = shutting down */
  int initialized; /* 1 = pool is ready */
};

/*
** Worker setup callback - called when a new worker state is created
*/
typedef void (*lus_WorkerSetup)(lua_State *parent, lua_State *worker);

/*
** =====================================================================
** C API for Embedders
** =====================================================================
*/

/* Initialize the global worker pool (called once at startup) */
LUA_API void lus_worker_pool_init(lua_State *L);

/* Shutdown the worker pool (called at program exit) */
LUA_API void lus_worker_pool_shutdown(void);

/* Set callback invoked when a worker state is created */
LUA_API void lus_onworker(lua_State *L, lus_WorkerSetup fn);

/* Create a worker from C (path on stack at idx, returns userdata) */
LUA_API WorkerState *lus_worker_create(lua_State *L, const char *path);

/* Send value at stack index to worker's inbox */
LUA_API int lus_worker_send(lua_State *L, WorkerState *w, int idx);

/* Pop message from worker's outbox, push to stack (returns 1 if got msg) */
LUA_API int lus_worker_receive(lua_State *L, WorkerState *w);

/* Get worker status */
LUA_API int lus_worker_status(WorkerState *w);

#endif
