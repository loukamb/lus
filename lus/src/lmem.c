/*
** $Id: lmem.c $
** Interface to Memory Manager
** See Copyright Notice in lua.h
*/

#define lmem_c
#define LUA_CORE

#include "lprefix.h"

#include <stddef.h>

#include "lua.h"

#include "ldebug.h"
#include "ldo.h"
#include "lgc.h"
#include "lmem.h"
#include "lobject.h"
#include "lstate.h"

/*
** About the realloc function:
** void *frealloc (void *ud, void *ptr, size_t osize, size_t nsize);
** ('osize' is the old size, 'nsize' is the new size)
**
** - frealloc(ud, p, x, 0) frees the block 'p' and returns NULL.
** Particularly, frealloc(ud, NULL, 0, 0) does nothing,
** which is equivalent to free(NULL) in ISO C.
**
** - frealloc(ud, NULL, x, s) creates a new block of size 's'
** (no matter 'x'). Returns NULL if it cannot create the new block.
**
** - otherwise, frealloc(ud, b, x, y) reallocates the block 'b' from
** size 'x' to size 'y'. Returns NULL if it cannot reallocate the
** block to the new size.
*/

/*
** Macro to call the allocation function.
*/
#define callfrealloc(g, block, os, ns) ((*g->frealloc)(g->ud, block, os, ns))

/*
** When an allocation fails, it will try again after an emergency
** collection, except when it cannot run a collection.  The GC should
** not be called while the state is not fully built, as the collector
** is not yet fully initialized. Also, it should not be called when
** 'gcstopem' is true, because then the interpreter is in the middle of
** a collection step.
*/
#define cantryagain(g) (completestate(g) && !g->gcstopem)

#if defined(EMERGENCYGCTESTS)
/*
** First allocation will fail except when freeing a block (frees never
** fail) and when it cannot try again; this fail will trigger 'tryagain'
** and a full GC cycle at every allocation.
*/
static void *firsttry(global_State *g, void *block, size_t os, size_t ns) {
  if (ns > 0 && cantryagain(g))
    return NULL; /* fail */
  else           /* normal allocation */
    return callfrealloc(g, block, os, ns);
}
#else
#define firsttry(g, block, os, ns) callfrealloc(g, block, os, ns)
#endif

/*
** {==================================================================
** Functions to allocate/deallocate arrays for the Parser
** ===================================================================
*/

/*
** Minimum size for arrays during parsing, to avoid overhead of
** reallocating to size 1, then 2, and then 4. All these arrays
** will be reallocated to exact sizes or erased when parsing ends.
*/
#define MINSIZEARRAY 4

void *luaM_growaux_(lua_State *L, void *block, int nelems, int *psize,
                    unsigned size_elems, int limit, const char *what) {
  void *newblock;
  int size = *psize;
  if (nelems + 1 <= size)          /* does one extra element still fit? */
    return block;                  /* nothing to be done */
  if (size >= limit / 2) {         /* cannot double it? */
    if (l_unlikely(size >= limit)) /* cannot grow even a little? */
      luaG_runerror(L, "too many %s (limit is %d)", what, limit);
    size = limit; /* still have at least one free place */
  } else {
    size *= 2;
    if (size < MINSIZEARRAY)
      size = MINSIZEARRAY; /* minimum size */
  }
  lua_assert(nelems + 1 <= size && size <= limit);
  /* 'limit' ensures that multiplication will not overflow */
  newblock = luaM_saferealloc_(L, block, cast_sizet(*psize) * size_elems,
                               cast_sizet(size) * size_elems);
  *psize = size; /* update only when everything else is OK */
  return newblock;
}

/*
** In prototypes, the size of the array is also its number of
** elements (to save memory). So, if it cannot shrink an array
** to its number of elements, the only option is to raise an
** error.
*/
void *luaM_shrinkvector_(lua_State *L, void *block, int *size, int final_n,
                         unsigned size_elem) {
  void *newblock;
  size_t oldsize = cast_sizet(*size) * size_elem;
  size_t newsize = cast_sizet(final_n) * size_elem;
  lua_assert(newsize <= oldsize);
  newblock = luaM_saferealloc_(L, block, oldsize, newsize);
  *size = final_n;
  return newblock;
}

/* }================================================================== */

l_noret luaM_toobig(lua_State *L) {
  luaG_runerror(L, "memory allocation error: block too big");
}

/*
** Free memory
*/
void luaM_free_(lua_State *L, void *block, size_t osize) {
  global_State *g = G(L);
  lua_assert((osize == 0) == (block == NULL));
  callfrealloc(g, block, osize, 0);
  g->GCdebt += cast(l_mem, osize);
}

/*
** In case of allocation fail, this function will do an emergency
** collection to free some memory and then try the allocation again.
*/
static void *tryagain(lua_State *L, void *block, size_t osize, size_t nsize) {
  global_State *g = G(L);
  if (cantryagain(g)) {
    luaC_fullgc(L, 1); /* try to free some memory... */
    return callfrealloc(g, block, osize, nsize); /* try again */
  } else
    return NULL; /* cannot run an emergency collection */
}

/*
** Generic allocation routine.
*/
void *luaM_realloc_(lua_State *L, void *block, size_t osize, size_t nsize) {
  void *newblock;
  global_State *g = G(L);
  lua_assert((osize == 0) == (block == NULL));
  newblock = firsttry(g, block, osize, nsize);
  if (l_unlikely(newblock == NULL && nsize > 0)) {
    newblock = tryagain(L, block, osize, nsize);
    if (newblock == NULL) /* still no memory? */
      return NULL;        /* do not update 'GCdebt' */
  }
  lua_assert((nsize == 0) == (newblock == NULL));
  g->GCdebt -= cast(l_mem, nsize) - cast(l_mem, osize);
  return newblock;
}

void *luaM_saferealloc_(lua_State *L, void *block, size_t osize, size_t nsize) {
  void *newblock = luaM_realloc_(L, block, osize, nsize);
  if (l_unlikely(newblock == NULL && nsize > 0)) /* allocation failed? */
    luaM_error(L);
  return newblock;
}

void *luaM_malloc_(lua_State *L, size_t size, int tag) {
  if (size == 0)
    return NULL; /* that's all */
  else {
    global_State *g = G(L);
    void *newblock = firsttry(g, NULL, cast_sizet(tag), size);
    if (l_unlikely(newblock == NULL)) {
      newblock = tryagain(L, NULL, cast_sizet(tag), size);
      if (newblock == NULL)
        luaM_error(L);
    }
    g->GCdebt -= cast(l_mem, size);
    return newblock;
  }
}

/*
** {==================================================================
** Arena allocator implementation
** ===================================================================
*/

/*
** Create a new arena block with the given minimum size.
*/
static lusM_ArenaBlock *lusM_newblock(lua_State *L, size_t minsize) {
  size_t blocksize = sizeof(lusM_ArenaBlock) - 1 + minsize;
  lusM_ArenaBlock *block =
      cast(lusM_ArenaBlock *, luaM_malloc_(L, blocksize, 0));
  block->next = NULL;
  block->size = minsize;
  block->used = 0;
  return block;
}

/*
** Create a new arena with the given default block size.
** If blocksize is 0, uses LUSM_ARENA_BLOCKSIZE.
*/
lusM_Arena *lusM_newarena(lua_State *L, size_t blocksize) {
  lusM_Arena *a;
  if (blocksize == 0)
    blocksize = LUSM_ARENA_BLOCKSIZE;
  a = cast(lusM_Arena *, luaM_malloc_(L, sizeof(lusM_Arena), 0));
  a->L = L;
  a->blocksize = blocksize;
  a->head = lusM_newblock(L, blocksize);
  a->current = a->head;
  return a;
}

/*
** Allocate 'size' bytes from the arena.
** Uses bump allocation from current block; creates new block if needed.
** Returns pointer aligned to sizeof(void*).
*/
void *lusM_arenaalloc(lusM_Arena *a, size_t size) {
  size_t align = sizeof(void *);
  size_t aligned_size;
  lusM_ArenaBlock *block = a->current;
  size_t offset;

  /* align size to pointer boundary */
  aligned_size = (size + align - 1) & ~(align - 1);

  /* check if current block has space */
  offset = block->used;
  if (offset + aligned_size <= block->size) {
    block->used += aligned_size;
    return cast(void *, block->data + offset);
  }

  /* need a new block - use larger of aligned_size or default blocksize */
  {
    size_t newsize =
        (aligned_size > a->blocksize) ? aligned_size : a->blocksize;
    lusM_ArenaBlock *newblock = lusM_newblock(a->L, newsize);
    a->current->next = newblock;
    a->current = newblock;
    newblock->used = aligned_size;
    return cast(void *, newblock->data);
  }
}

/*
** Free all blocks in the arena and the arena struct itself.
*/
void lusM_freearena(lusM_Arena *a) {
  lua_State *L = a->L;
  lusM_ArenaBlock *block = a->head;
  while (block != NULL) {
    lusM_ArenaBlock *next = block->next;
    size_t blocksize = sizeof(lusM_ArenaBlock) - 1 + block->size;
    luaM_free_(L, block, blocksize);
    block = next;
  }
  luaM_free_(L, a, sizeof(lusM_Arena));
}

/* }================================================================== */
