/*
** lnetlib.c
** Network library for Lus
** Implements network.* global functions
*/

#define lnetlib_c
#define LUA_LIB

#include "lprefix.h"

#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "events/lev.h"
#include "lauxlib.h"
#include "lua.h"
#include "lualib.h"

/* OpenSSL headers */
#include <openssl/err.h>
#include <openssl/ssl.h>
#include <openssl/x509v3.h>

/* Platform-specific socket headers - must come before ares.h for fd_set */
#if defined(LUS_PLATFORM_WINDOWS)
#include <BaseTsd.h> /* For SSIZE_T */
#include <winsock2.h>
#include <ws2tcpip.h>
typedef SOCKET socket_t;
typedef SSIZE_T ssize_t; /* POSIX type not defined on MSVC */
#define SOCKET_INVALID INVALID_SOCKET
#define SOCKET_ERROR_VAL SOCKET_ERROR
#define SOCKET_ERRNO WSAGetLastError()
#define SOCKET_EINPROGRESS WSAEWOULDBLOCK
#define SOCKET_EWOULDBLOCK WSAEWOULDBLOCK
#define sock_close closesocket
#else
#include <arpa/inet.h>
#include <fcntl.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
typedef int socket_t;
#define SOCKET_INVALID -1
#define SOCKET_ERROR_VAL -1
#define SOCKET_ERRNO errno
#define SOCKET_EINPROGRESS EINPROGRESS
#define SOCKET_EWOULDBLOCK EWOULDBLOCK
#define sock_close close
#endif

/* c-ares headers - needs fd_set from above */
#include <ares.h>

/*
** {======================================================
** Constants and Type Definitions
** =======================================================
*/

#define SOCKET_METATABLE "network.socket"
#define SERVER_METATABLE "network.server"
#define UDPSOCKET_METATABLE "network.udpsocket"

#define DEFAULT_BACKLOG 128
#define DEFAULT_RECV_SIZE 4096
#define DEFAULT_UDP_SIZE 8192
#define MAX_RECV_SIZE 1048576 /* 1MB max single read */

/* TCP Socket (client connection) */
typedef struct {
  socket_t fd;
  int timeout_ms; /* -1 = blocking, 0 = non-blocking, >0 = timeout */
  int closed;
  SSL *ssl;            /* NULL if not using TLS */
  luaL_Buffer readbuf; /* internal read buffer for line-based reads */
  char *buffer;        /* buffered data not yet consumed */
  size_t buflen;       /* length of buffered data */
  size_t bufcap;       /* capacity of buffer */
} LSocket;

/* TCP Server (listening socket) */
typedef struct {
  socket_t fd;
  int timeout_ms;
  int closed;
} LServer;

/* UDP Socket */
typedef struct {
  socket_t fd;
  int timeout_ms;
  int bound;
  int closed;
} LUDPSocket;

/* Global SSL context */
static SSL_CTX *ssl_ctx = NULL;

/* Global state */
#if defined(LUS_PLATFORM_WINDOWS)
static int winsock_initialized = 0;
#endif
static int cares_initialized = 0;

/* }====================================================== */

/*
** {======================================================
** Platform Initialization
** =======================================================
*/

static void init_winsock(lua_State *L) {
#if defined(LUS_PLATFORM_WINDOWS)
  if (!winsock_initialized) {
    WSADATA wsa;
    int result = WSAStartup(MAKEWORD(2, 2), &wsa);
    if (result != 0) {
      luaL_error(L, "WSAStartup failed: %d", result);
    }
    winsock_initialized = 1;
  }
#else
  (void)L; /* unused */
#endif
}

static void init_ssl(lua_State *L) {
  if (ssl_ctx == NULL) {
    SSL_library_init();
    SSL_load_error_strings();
    OpenSSL_add_all_algorithms();

    ssl_ctx = SSL_CTX_new(TLS_client_method());
    if (ssl_ctx == NULL) {
      luaL_error(L, "failed to create SSL context");
    }

    /* Load system CA certificates */
    SSL_CTX_set_default_verify_paths(ssl_ctx);
    SSL_CTX_set_verify(ssl_ctx, SSL_VERIFY_PEER, NULL);
  }
}

static void init_cares(lua_State *L) {
  if (!cares_initialized) {
    int status = ares_library_init(ARES_LIB_INIT_ALL);
    if (status != ARES_SUCCESS) {
      luaL_error(L, "c-ares initialization failed: %s", ares_strerror(status));
    }
    cares_initialized = 1;
  }
}

/* }====================================================== */

/*
** {======================================================
** Error Handling Helpers
** =======================================================
*/

static const char *sock_strerror(int err) {
#if defined(LUS_PLATFORM_WINDOWS)
  static char buf[256];
  FormatMessageA(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                 NULL, err, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), buf,
                 sizeof(buf), NULL);
  /* Remove trailing newlines */
  size_t len = strlen(buf);
  while (len > 0 && (buf[len - 1] == '\n' || buf[len - 1] == '\r')) {
    buf[--len] = '\0';
  }
  return buf;
#else
  return strerror(err);
#endif
}

static int push_socket_error(lua_State *L, const char *prefix) {
  int err = SOCKET_ERRNO;
  lua_pushnil(L);
  if (prefix) {
    lua_pushfstring(L, "%s: %s", prefix, sock_strerror(err));
  } else {
    lua_pushstring(L, sock_strerror(err));
  }
  return 2;
}

static int push_ssl_error(lua_State *L, const char *prefix) {
  unsigned long err = ERR_get_error();
  char buf[256];
  ERR_error_string_n(err, buf, sizeof(buf));
  lua_pushnil(L);
  if (prefix) {
    lua_pushfstring(L, "%s: %s", prefix, buf);
  } else {
    lua_pushstring(L, buf);
  }
  return 2;
}

/* }====================================================== */

/*
** {======================================================
** Socket Utilities
** =======================================================
*/

static void set_nonblocking(socket_t fd, int nonblock) {
#if defined(LUS_PLATFORM_WINDOWS)
  u_long mode = nonblock ? 1 : 0;
  ioctlsocket(fd, FIONBIO, &mode);
#else
  int flags = fcntl(fd, F_GETFL, 0);
  if (nonblock) {
    fcntl(fd, F_SETFL, flags | O_NONBLOCK);
  } else {
    fcntl(fd, F_SETFL, flags & ~O_NONBLOCK);
  }
#endif
}

/* Wait for socket to become readable/writable with timeout (blocking version)
 */
static int wait_socket(socket_t fd, int for_write, int timeout_ms) {
  fd_set fds;
  struct timeval tv, *tvp = NULL;

  FD_ZERO(&fds);
  FD_SET(fd, &fds);

  if (timeout_ms >= 0) {
    tv.tv_sec = timeout_ms / 1000;
    tv.tv_usec = (timeout_ms % 1000) * 1000;
    tvp = &tv;
  }

  if (for_write) {
    return select((int)fd + 1, NULL, &fds, NULL, tvp);
  } else {
    return select((int)fd + 1, &fds, NULL, NULL, tvp);
  }
}

/* }====================================================== */

/*
** {======================================================
** DNS Resolution (c-ares)
** =======================================================
*/

typedef struct {
  int done;
  int status;
  struct sockaddr_storage addr;
  socklen_t addrlen;
} DnsResult;

static void addrinfo_callback(void *arg, int status, int timeouts,
                              struct ares_addrinfo *result) {
  DnsResult *res = (DnsResult *)arg;
  (void)timeouts;

  res->done = 1;
  res->status = status;

  if (status == ARES_SUCCESS && result != NULL && result->nodes != NULL) {
    struct ares_addrinfo_node *node = result->nodes;
    memcpy(&res->addr, node->ai_addr, node->ai_addrlen);
    res->addrlen = (socklen_t)node->ai_addrlen;
    ares_freeaddrinfo(result);
  }
}

static int resolve_hostname(lua_State *L, const char *hostname, int port,
                            struct sockaddr_storage *addr, socklen_t *addrlen) {
  ares_channel_t *channel;
  DnsResult result = {0};
  int status;
  struct ares_options opts;
  struct ares_addrinfo_hints hints;
  char port_str[16];

  init_cares(L);

  memset(&opts, 0, sizeof(opts));
  opts.timeout = 5000; /* 5 second timeout */
  opts.tries = 3;

  status =
      ares_init_options(&channel, &opts, ARES_OPT_TIMEOUTMS | ARES_OPT_TRIES);
  if (status != ARES_SUCCESS) {
    return luaL_error(L, "DNS resolver init failed: %s", ares_strerror(status));
  }

  /* Prepare hints for getaddrinfo */
  memset(&hints, 0, sizeof(hints));
  hints.ai_family = AF_UNSPEC; /* Allow IPv4 or IPv6 */
  hints.ai_socktype = SOCK_STREAM;

  snprintf(port_str, sizeof(port_str), "%d", port);

  /* Start async lookup */
  ares_getaddrinfo(channel, hostname, port_str, &hints, addrinfo_callback,
                   &result);

  /* Process events until done */
  while (!result.done) {
    ares_socket_t socks[ARES_GETSOCK_MAXNUM];
    int bitmask;
    int i, nfds = 0;
    fd_set read_fds, write_fds;
    struct timeval tv, *tvp;

    FD_ZERO(&read_fds);
    FD_ZERO(&write_fds);

    bitmask = ares_getsock(channel, socks, ARES_GETSOCK_MAXNUM);
    for (i = 0; i < ARES_GETSOCK_MAXNUM; i++) {
      /* Use unsigned literal to avoid UBSan "left shift of 1 by 31" error */
      if (bitmask & (1U << i)) { /* readable */
        FD_SET(socks[i], &read_fds);
        if ((int)socks[i] + 1 > nfds)
          nfds = (int)socks[i] + 1;
      }
      if (bitmask & (1U << (i + ARES_GETSOCK_MAXNUM))) { /* writable */
        FD_SET(socks[i], &write_fds);
        if ((int)socks[i] + 1 > nfds)
          nfds = (int)socks[i] + 1;
      }
    }

    if (nfds == 0)
      break;

    tvp = ares_timeout(channel, NULL, &tv);
    select(nfds, &read_fds, &write_fds, NULL, tvp);
    ares_process_fd(channel, ARES_SOCKET_BAD, ARES_SOCKET_BAD);

    /* Process all ready sockets */
    for (i = 0; i < ARES_GETSOCK_MAXNUM; i++) {
      ares_socket_t rfd = ARES_SOCKET_BAD, wfd = ARES_SOCKET_BAD;
      if ((bitmask & (1U << i)) && FD_ISSET(socks[i], &read_fds)) {
        rfd = socks[i];
      }
      if ((bitmask & (1U << (i + ARES_GETSOCK_MAXNUM))) &&
          FD_ISSET(socks[i], &write_fds)) {
        wfd = socks[i];
      }
      if (rfd != ARES_SOCKET_BAD || wfd != ARES_SOCKET_BAD) {
        ares_process_fd(channel, rfd, wfd);
      }
    }
  }

  ares_destroy(channel);

  if (result.status != ARES_SUCCESS) {
    return luaL_error(L, "cannot resolve '%s': %s", hostname,
                      ares_strerror(result.status));
  }

  memcpy(addr, &result.addr, result.addrlen);
  *addrlen = result.addrlen;

  return 0;
}

/* }====================================================== */

/*
** {======================================================
** TCP Socket Methods
** =======================================================
*/

static LSocket *check_socket(lua_State *L, int idx) {
  LSocket *sock = (LSocket *)luaL_checkudata(L, idx, SOCKET_METATABLE);
  if (sock->closed) {
    luaL_error(L, "attempt to use a closed socket");
  }
  return sock;
}

static LSocket *new_socket(lua_State *L) {
  LSocket *sock = (LSocket *)lua_newuserdata(L, sizeof(LSocket));
  sock->fd = SOCKET_INVALID;
  sock->timeout_ms = -1; /* blocking by default */
  sock->closed = 0;
  sock->ssl = NULL;
  sock->buffer = NULL;
  sock->buflen = 0;
  sock->bufcap = 0;
  luaL_setmetatable(L, SOCKET_METATABLE);
  return sock;
}

/*
** Async socket send implementation
** Uses lua_yieldk for proper continuation when running in detached coroutine.
** The context (lua_KContext) stores the number of bytes already sent.
*/

static int socket_send_impl(lua_State *L, int status, lua_KContext ctx);

static int socket_send(lua_State *L) {
  /* Initial call - start with 0 bytes sent */
  return socket_send_impl(L, LUA_OK, 0);
}

static int socket_send_impl(lua_State *L, int status, lua_KContext ctx) {
  LSocket *sock = check_socket(L, 1);
  size_t len;
  const char *data = luaL_checklstring(L, 2, &len);
  size_t total = (size_t)ctx; /* Bytes already sent from previous calls */
  int detached = is_detached(L);
  ssize_t sent;

  (void)status; /* Unused - we don't distinguish resume reasons */

  /* For detached coroutines, set non-blocking mode */
  if (detached) {
    set_nonblocking(sock->fd, 1);
  }

  while (total < len) {
    if (sock->timeout_ms >= 0 && !detached) {
      int ready = wait_socket(sock->fd, 1, sock->timeout_ms);
      if (ready <= 0) {
        if (ready == 0) {
          return luaL_error(L, "send timeout");
        }
        return push_socket_error(L, "send");
      }
    }

    if (sock->ssl) {
      sent = SSL_write(sock->ssl, data + total, (int)(len - total));
      if (sent <= 0) {
        int ssl_err = SSL_get_error(sock->ssl, (int)sent);
        if (ssl_err == SSL_ERROR_WANT_WRITE || ssl_err == SSL_ERROR_WANT_READ) {
          if (detached) {
            /* Yield with continuation - will resume at socket_send_impl */
            set_yield_reason(L, YIELD_IO);
            set_yield_fd(L, (int)sock->fd);
            set_yield_events(L, EVLOOP_WRITE);
            return lua_yieldk(L, 0, (lua_KContext)total, socket_send_impl);
          }
          continue;
        }
        if (detached)
          set_nonblocking(sock->fd, 0);
        return push_ssl_error(L, "SSL send");
      }
    } else {
      sent = send(sock->fd, data + total, (int)(len - total), 0);
      if (sent == SOCKET_ERROR_VAL) {
        int err = SOCKET_ERRNO;
        if (err == SOCKET_EWOULDBLOCK || err == SOCKET_EINPROGRESS) {
          if (detached) {
            /* Yield with continuation - will resume at socket_send_impl */
            set_yield_reason(L, YIELD_IO);
            set_yield_fd(L, (int)sock->fd);
            set_yield_events(L, EVLOOP_WRITE);
            return lua_yieldk(L, 0, (lua_KContext)total, socket_send_impl);
          }
          continue;
        }
        if (detached)
          set_nonblocking(sock->fd, 0);
        return push_socket_error(L, "send");
      }
    }
    total += (size_t)sent;
  }

  /* Restore blocking mode */
  if (detached) {
    set_nonblocking(sock->fd, 0);
  }

  lua_pushinteger(L, (lua_Integer)total);
  return 1;
}

/* Read exactly n bytes, or until EOF/error - with async support */
static int recv_bytes(lua_State *L, LSocket *sock, size_t n) {
  luaL_Buffer b;
  size_t total = 0;
  char buf[DEFAULT_RECV_SIZE];
  int detached = is_detached(L);

  /* For detached coroutines, set non-blocking mode */
  if (detached) {
    set_nonblocking(sock->fd, 1);
  }

  luaL_buffinit(L, &b);

  /* First consume any buffered data */
  if (sock->buflen > 0) {
    size_t take = (sock->buflen < n) ? sock->buflen : n;
    luaL_addlstring(&b, sock->buffer, take);
    total += take;
    if (take < sock->buflen) {
      memmove(sock->buffer, sock->buffer + take, sock->buflen - take);
    }
    sock->buflen -= take;
  }

  while (total < n) {
    ssize_t got;
    size_t want = n - total;
    if (want > sizeof(buf))
      want = sizeof(buf);

    if (sock->timeout_ms >= 0 && !detached) {
      int ready = wait_socket(sock->fd, 0, sock->timeout_ms);
      if (ready <= 0) {
        if (ready == 0) {
          if (detached)
            set_nonblocking(sock->fd, 0);
          return luaL_error(L, "receive timeout");
        }
        if (detached)
          set_nonblocking(sock->fd, 0);
        return push_socket_error(L, "receive");
      }
    }

    if (sock->ssl) {
      got = SSL_read(sock->ssl, buf, (int)want);
      if (got <= 0) {
        int ssl_err = SSL_get_error(sock->ssl, (int)got);
        if (ssl_err == SSL_ERROR_ZERO_RETURN) {
          break; /* EOF */
        }
        if (ssl_err == SSL_ERROR_WANT_READ || ssl_err == SSL_ERROR_WANT_WRITE) {
          if (detached) {
            set_yield_reason(L, YIELD_IO);
            set_yield_fd(L, (int)sock->fd);
            set_yield_events(L, EVLOOP_READ);
            return lua_yield(L, 0);
          }
          continue;
        }
        if (detached)
          set_nonblocking(sock->fd, 0);
        return push_ssl_error(L, "SSL receive");
      }
    } else {
      got = recv(sock->fd, buf, (int)want, 0);
      if (got == SOCKET_ERROR_VAL) {
        int err = SOCKET_ERRNO;
        if (err == SOCKET_EWOULDBLOCK || err == SOCKET_EINPROGRESS) {
          if (detached) {
            set_yield_reason(L, YIELD_IO);
            set_yield_fd(L, (int)sock->fd);
            set_yield_events(L, EVLOOP_READ);
            return lua_yield(L, 0);
          }
          continue;
        }
        if (detached)
          set_nonblocking(sock->fd, 0);
        return push_socket_error(L, "receive");
      }
      if (got == 0)
        break; /* EOF */
    }

    luaL_addlstring(&b, buf, (size_t)got);
    total += (size_t)got;
  }

  if (detached) {
    set_nonblocking(sock->fd, 0);
  }

  luaL_pushresult(&b);
  return 1;
}

/* Read until newline - with async support */
static int recv_line(lua_State *L, LSocket *sock) {
  luaL_Buffer b;
  char buf[DEFAULT_RECV_SIZE];
  int detached = is_detached(L);

  /* For detached coroutines, set non-blocking mode */
  if (detached) {
    set_nonblocking(sock->fd, 1);
  }

  luaL_buffinit(L, &b);

  /* First check buffered data for newline */
  if (sock->buflen > 0) {
    char *nl = memchr(sock->buffer, '\n', sock->buflen);
    if (nl) {
      size_t linelen = (size_t)(nl - sock->buffer);
      /* Strip \r if present */
      if (linelen > 0 && sock->buffer[linelen - 1] == '\r') {
        luaL_addlstring(&b, sock->buffer, linelen - 1);
      } else {
        luaL_addlstring(&b, sock->buffer, linelen);
      }
      /* Remove line + newline from buffer */
      size_t consumed = linelen + 1;
      if (consumed < sock->buflen) {
        memmove(sock->buffer, sock->buffer + consumed, sock->buflen - consumed);
      }
      sock->buflen -= consumed;
      if (detached)
        set_nonblocking(sock->fd, 0);
      luaL_pushresult(&b);
      return 1;
    }
    /* No newline, push entire buffer and continue reading */
    luaL_addlstring(&b, sock->buffer, sock->buflen);
    sock->buflen = 0;
  }

  for (;;) {
    ssize_t got;

    if (sock->timeout_ms >= 0 && !detached) {
      int ready = wait_socket(sock->fd, 0, sock->timeout_ms);
      if (ready <= 0) {
        if (ready == 0) {
          if (detached)
            set_nonblocking(sock->fd, 0);
          return luaL_error(L, "receive timeout");
        }
        if (detached)
          set_nonblocking(sock->fd, 0);
        return push_socket_error(L, "receive");
      }
    }

    if (sock->ssl) {
      got = SSL_read(sock->ssl, buf, sizeof(buf));
      if (got <= 0) {
        int ssl_err = SSL_get_error(sock->ssl, (int)got);
        if (ssl_err == SSL_ERROR_ZERO_RETURN) {
          if (detached)
            set_nonblocking(sock->fd, 0);
          luaL_pushresult(&b);
          return 1; /* EOF, return what we have */
        }
        if (ssl_err == SSL_ERROR_WANT_READ || ssl_err == SSL_ERROR_WANT_WRITE) {
          if (detached) {
            set_yield_reason(L, YIELD_IO);
            set_yield_fd(L, (int)sock->fd);
            set_yield_events(L, EVLOOP_READ);
            return lua_yield(L, 0);
          }
          continue;
        }
        if (detached)
          set_nonblocking(sock->fd, 0);
        return push_ssl_error(L, "SSL receive");
      }
    } else {
      got = recv(sock->fd, buf, sizeof(buf), 0);
      if (got == SOCKET_ERROR_VAL) {
        int err = SOCKET_ERRNO;
        if (err == SOCKET_EWOULDBLOCK || err == SOCKET_EINPROGRESS) {
          if (detached) {
            set_yield_reason(L, YIELD_IO);
            set_yield_fd(L, (int)sock->fd);
            set_yield_events(L, EVLOOP_READ);
            return lua_yield(L, 0);
          }
          continue;
        }
        if (detached)
          set_nonblocking(sock->fd, 0);
        return push_socket_error(L, "receive");
      }
      if (got == 0) {
        if (detached)
          set_nonblocking(sock->fd, 0);
        luaL_pushresult(&b);
        return 1; /* EOF */
      }
    }

    /* Search for newline in received data */
    char *nl = memchr(buf, '\n', (size_t)got);
    if (nl) {
      size_t linelen = (size_t)(nl - buf);
      /* Strip \r if present */
      if (linelen > 0 && buf[linelen - 1] == '\r') {
        luaL_addlstring(&b, buf, linelen - 1);
      } else {
        luaL_addlstring(&b, buf, linelen);
      }
      /* Buffer remainder */
      size_t remainder = (size_t)got - linelen - 1;
      if (remainder > 0) {
        if (sock->bufcap < remainder) {
          sock->buffer = realloc(sock->buffer, remainder);
          sock->bufcap = remainder;
        }
        memcpy(sock->buffer, nl + 1, remainder);
        sock->buflen = remainder;
      }
      if (detached)
        set_nonblocking(sock->fd, 0);
      luaL_pushresult(&b);
      return 1;
    }

    luaL_addlstring(&b, buf, (size_t)got);
  }
}

/* Read until connection closed - with async support for detached coroutines */
static int recv_all(lua_State *L, LSocket *sock) {
  luaL_Buffer b;
  char buf[DEFAULT_RECV_SIZE];
  int detached = is_detached(L);

  /* For detached coroutines, set non-blocking mode */
  if (detached) {
    set_nonblocking(sock->fd, 1);
  }

  luaL_buffinit(L, &b);

  /* First push any buffered data */
  if (sock->buflen > 0) {
    luaL_addlstring(&b, sock->buffer, sock->buflen);
    sock->buflen = 0;
  }

  for (;;) {
    ssize_t got;

    if (sock->timeout_ms >= 0 && !detached) {
      int ready = wait_socket(sock->fd, 0, sock->timeout_ms);
      if (ready <= 0) {
        if (ready == 0) {
          if (detached)
            set_nonblocking(sock->fd, 0);
          return luaL_error(L, "receive timeout");
        }
        if (detached)
          set_nonblocking(sock->fd, 0);
        return push_socket_error(L, "receive");
      }
    }

    if (sock->ssl) {
      got = SSL_read(sock->ssl, buf, sizeof(buf));
      if (got <= 0) {
        int ssl_err = SSL_get_error(sock->ssl, (int)got);
        if (ssl_err == SSL_ERROR_ZERO_RETURN) {
          break; /* EOF */
        }
        if (ssl_err == SSL_ERROR_WANT_READ || ssl_err == SSL_ERROR_WANT_WRITE) {
          if (detached) {
            /* Yield to event loop for read readiness */
            set_yield_reason(L, YIELD_IO);
            set_yield_fd(L, (int)sock->fd);
            set_yield_events(L, EVLOOP_READ);
            /* Note: We can't preserve luaL_Buffer state across yield
               so we restart recv_all on resume. This is acceptable for
               most use cases since we're reading until EOF anyway. */
            return lua_yield(L, 0);
          }
          continue;
        }
        if (detached)
          set_nonblocking(sock->fd, 0);
        return push_ssl_error(L, "SSL receive");
      }
    } else {
      got = recv(sock->fd, buf, sizeof(buf), 0);
      if (got == SOCKET_ERROR_VAL) {
        int err = SOCKET_ERRNO;
        if (err == SOCKET_EWOULDBLOCK || err == SOCKET_EINPROGRESS) {
          if (detached) {
            /* Yield to event loop for read readiness */
            set_yield_reason(L, YIELD_IO);
            set_yield_fd(L, (int)sock->fd);
            set_yield_events(L, EVLOOP_READ);
            return lua_yield(L, 0);
          }
          continue;
        }
        if (detached)
          set_nonblocking(sock->fd, 0);
        return push_socket_error(L, "receive");
      }
      if (got == 0)
        break; /* EOF */
    }

    luaL_addlstring(&b, buf, (size_t)got);
  }

  if (detached) {
    set_nonblocking(sock->fd, 0);
  }

  luaL_pushresult(&b);
  return 1;
}

static int socket_receive(lua_State *L) {
  LSocket *sock = check_socket(L, 1);

  if (lua_type(L, 2) == LUA_TNUMBER) {
    lua_Integer n = luaL_checkinteger(L, 2);
    if (n <= 0 || n > MAX_RECV_SIZE) {
      return luaL_error(L, "invalid receive size: %d", (int)n);
    }
    return recv_bytes(L, sock, (size_t)n);
  }

  const char *pattern = luaL_optstring(L, 2, "*l");

  if (strcmp(pattern, "*l") == 0) {
    return recv_line(L, sock);
  } else if (strcmp(pattern, "*a") == 0) {
    return recv_all(L, sock);
  } else {
    return luaL_error(L, "invalid receive pattern: %s", pattern);
  }
}

static int socket_close(lua_State *L) {
  LSocket *sock = (LSocket *)luaL_checkudata(L, 1, SOCKET_METATABLE);
  if (!sock->closed) {
    if (sock->ssl) {
      SSL_shutdown(sock->ssl);
      SSL_free(sock->ssl);
      sock->ssl = NULL;
    }
    if (sock->fd != SOCKET_INVALID) {
      sock_close(sock->fd);
      sock->fd = SOCKET_INVALID;
    }
    if (sock->buffer) {
      free(sock->buffer);
      sock->buffer = NULL;
    }
    sock->closed = 1;
  }
  return 0;
}

static int socket_settimeout(lua_State *L) {
  LSocket *sock = check_socket(L, 1);
  lua_Number seconds = luaL_checknumber(L, 2);

  if (seconds < 0) {
    sock->timeout_ms = -1; /* blocking */
  } else if (seconds == 0) {
    sock->timeout_ms = 0; /* non-blocking */
  } else {
    sock->timeout_ms = (int)(seconds * 1000);
  }

  return 0;
}

static int socket_gc(lua_State *L) { return socket_close(L); }

static int socket_tostring(lua_State *L) {
  LSocket *sock = (LSocket *)luaL_checkudata(L, 1, SOCKET_METATABLE);
  if (sock->closed) {
    lua_pushstring(L, "socket (closed)");
  } else {
    lua_pushfstring(L, "socket (%p)", (void *)sock);
  }
  return 1;
}

static const luaL_Reg socket_methods[] = {{"send", socket_send},
                                          {"receive", socket_receive},
                                          {"close", socket_close},
                                          {"settimeout", socket_settimeout},
                                          {"__gc", socket_gc},
                                          {"__close", socket_close},
                                          {"__tostring", socket_tostring},
                                          {NULL, NULL}};

/* }====================================================== */

/*
** {======================================================
** TCP Server Methods
** =======================================================
*/

static LServer *check_server(lua_State *L, int idx) {
  LServer *srv = (LServer *)luaL_checkudata(L, idx, SERVER_METATABLE);
  if (srv->closed) {
    luaL_error(L, "attempt to use a closed server");
  }
  return srv;
}

static LServer *new_server(lua_State *L) {
  LServer *srv = (LServer *)lua_newuserdata(L, sizeof(LServer));
  srv->fd = SOCKET_INVALID;
  srv->timeout_ms = -1;
  srv->closed = 0;
  luaL_setmetatable(L, SERVER_METATABLE);
  return srv;
}

static int server_accept(lua_State *L) {
  LServer *srv = check_server(L, 1);
  struct sockaddr_storage addr;
  socklen_t addrlen = sizeof(addr);
  socket_t client_fd;

  if (srv->timeout_ms >= 0) {
    int ready = wait_socket(srv->fd, 0, srv->timeout_ms);
    if (ready <= 0) {
      if (ready == 0) {
        return luaL_error(L, "accept timeout");
      }
      return push_socket_error(L, "accept");
    }
  }

  client_fd = accept(srv->fd, (struct sockaddr *)&addr, &addrlen);
  if (client_fd == SOCKET_INVALID) {
    return push_socket_error(L, "accept");
  }

  LSocket *sock = new_socket(L);
  sock->fd = client_fd;

  return 1;
}

static int server_close(lua_State *L) {
  LServer *srv = (LServer *)luaL_checkudata(L, 1, SERVER_METATABLE);
  if (!srv->closed) {
    if (srv->fd != SOCKET_INVALID) {
      sock_close(srv->fd);
      srv->fd = SOCKET_INVALID;
    }
    srv->closed = 1;
  }
  return 0;
}

static int server_settimeout(lua_State *L) {
  LServer *srv = check_server(L, 1);
  lua_Number seconds = luaL_checknumber(L, 2);

  if (seconds < 0) {
    srv->timeout_ms = -1;
  } else if (seconds == 0) {
    srv->timeout_ms = 0;
  } else {
    srv->timeout_ms = (int)(seconds * 1000);
  }

  return 0;
}

static int server_gc(lua_State *L) { return server_close(L); }

static int server_tostring(lua_State *L) {
  LServer *srv = (LServer *)luaL_checkudata(L, 1, SERVER_METATABLE);
  if (srv->closed) {
    lua_pushstring(L, "server (closed)");
  } else {
    lua_pushfstring(L, "server (%p)", (void *)srv);
  }
  return 1;
}

static const luaL_Reg server_methods[] = {{"accept", server_accept},
                                          {"close", server_close},
                                          {"settimeout", server_settimeout},
                                          {"__gc", server_gc},
                                          {"__close", server_close},
                                          {"__tostring", server_tostring},
                                          {NULL, NULL}};

/* }====================================================== */

/*
** {======================================================
** UDP Socket Methods
** =======================================================
*/

static LUDPSocket *check_udpsocket(lua_State *L, int idx) {
  LUDPSocket *sock = (LUDPSocket *)luaL_checkudata(L, idx, UDPSOCKET_METATABLE);
  if (sock->closed) {
    luaL_error(L, "attempt to use a closed UDP socket");
  }
  return sock;
}

static LUDPSocket *new_udpsocket(lua_State *L) {
  LUDPSocket *sock = (LUDPSocket *)lua_newuserdata(L, sizeof(LUDPSocket));
  sock->fd = SOCKET_INVALID;
  sock->timeout_ms = -1;
  sock->bound = 0;
  sock->closed = 0;
  luaL_setmetatable(L, UDPSOCKET_METATABLE);
  return sock;
}

static int udp_sendto(lua_State *L) {
  LUDPSocket *sock = check_udpsocket(L, 1);
  size_t len;
  const char *data = luaL_checklstring(L, 2, &len);
  const char *address = luaL_checkstring(L, 3);
  int port = (int)luaL_checkinteger(L, 4);

  struct sockaddr_storage addr;
  socklen_t addrlen;

  resolve_hostname(L, address, port, &addr, &addrlen);

  if (sock->timeout_ms >= 0) {
    int ready = wait_socket(sock->fd, 1, sock->timeout_ms);
    if (ready <= 0) {
      if (ready == 0) {
        return luaL_error(L, "sendto timeout");
      }
      return push_socket_error(L, "sendto");
    }
  }

  ssize_t sent =
      sendto(sock->fd, data, (int)len, 0, (struct sockaddr *)&addr, addrlen);
  if (sent == SOCKET_ERROR_VAL) {
    return push_socket_error(L, "sendto");
  }

  lua_pushinteger(L, (lua_Integer)sent);
  return 1;
}

static int udp_receive(lua_State *L) {
  LUDPSocket *sock = check_udpsocket(L, 1);
  lua_Integer size = luaL_optinteger(L, 2, DEFAULT_UDP_SIZE);

  if (size <= 0 || size > MAX_RECV_SIZE) {
    return luaL_error(L, "invalid receive size: %d", (int)size);
  }

  char *buf = lua_newuserdata(L, (size_t)size);
  struct sockaddr_storage addr;
  socklen_t addrlen = sizeof(addr);

  if (sock->timeout_ms >= 0) {
    int ready = wait_socket(sock->fd, 0, sock->timeout_ms);
    if (ready <= 0) {
      if (ready == 0) {
        return luaL_error(L, "receive timeout");
      }
      return push_socket_error(L, "recvfrom");
    }
  }

  ssize_t got =
      recvfrom(sock->fd, buf, (int)size, 0, (struct sockaddr *)&addr, &addrlen);
  if (got == SOCKET_ERROR_VAL) {
    return push_socket_error(L, "recvfrom");
  }

  lua_pushlstring(L, buf, (size_t)got);

  /* Get sender IP and port */
  char ip[INET6_ADDRSTRLEN];
  int sender_port;

  if (addr.ss_family == AF_INET) {
    struct sockaddr_in *sin = (struct sockaddr_in *)&addr;
    inet_ntop(AF_INET, &sin->sin_addr, ip, sizeof(ip));
    sender_port = ntohs(sin->sin_port);
  } else {
    struct sockaddr_in6 *sin6 = (struct sockaddr_in6 *)&addr;
    inet_ntop(AF_INET6, &sin6->sin6_addr, ip, sizeof(ip));
    sender_port = ntohs(sin6->sin6_port);
  }

  lua_pushstring(L, ip);
  lua_pushinteger(L, sender_port);

  return 3; /* data, ip, port */
}

static int udp_setsockname(lua_State *L) {
  LUDPSocket *sock = check_udpsocket(L, 1);
  const char *address = luaL_checkstring(L, 2);
  int port = (int)luaL_checkinteger(L, 3);

  struct sockaddr_storage addr;
  socklen_t addrlen;

  resolve_hostname(L, address, port, &addr, &addrlen);

  if (bind(sock->fd, (struct sockaddr *)&addr, addrlen) == SOCKET_ERROR_VAL) {
    return push_socket_error(L, "bind");
  }

  sock->bound = 1;
  return 0;
}

static int udp_close(lua_State *L) {
  LUDPSocket *sock = (LUDPSocket *)luaL_checkudata(L, 1, UDPSOCKET_METATABLE);
  if (!sock->closed) {
    if (sock->fd != SOCKET_INVALID) {
      sock_close(sock->fd);
      sock->fd = SOCKET_INVALID;
    }
    sock->closed = 1;
  }
  return 0;
}

static int udp_gc(lua_State *L) { return udp_close(L); }

static int udp_tostring(lua_State *L) {
  LUDPSocket *sock = (LUDPSocket *)luaL_checkudata(L, 1, UDPSOCKET_METATABLE);
  if (sock->closed) {
    lua_pushstring(L, "udpsocket (closed)");
  } else {
    lua_pushfstring(L, "udpsocket (%p)", (void *)sock);
  }
  return 1;
}

static const luaL_Reg udpsocket_methods[] = {{"sendto", udp_sendto},
                                             {"receive", udp_receive},
                                             {"setsockname", udp_setsockname},
                                             {"close", udp_close},
                                             {"__gc", udp_gc},
                                             {"__close", udp_close},
                                             {"__tostring", udp_tostring},
                                             {NULL, NULL}};

/* }====================================================== */

/*
** {======================================================
** network.tcp functions
** =======================================================
*/

static int tcp_connect(lua_State *L) {
  const char *address = luaL_checkstring(L, 1);
  int port = (int)luaL_checkinteger(L, 2);

  init_winsock(L);

  struct sockaddr_storage addr;
  socklen_t addrlen;

  resolve_hostname(L, address, port, &addr, &addrlen);

  socket_t fd = socket(addr.ss_family, SOCK_STREAM, IPPROTO_TCP);
  if (fd == SOCKET_INVALID) {
    return luaL_error(L, "cannot create socket: %s",
                      sock_strerror(SOCKET_ERRNO));
  }

  if (connect(fd, (struct sockaddr *)&addr, addrlen) == SOCKET_ERROR_VAL) {
    int err = SOCKET_ERRNO;
    sock_close(fd);
    return luaL_error(L, "cannot connect to %s:%d: %s", address, port,
                      sock_strerror(err));
  }

  LSocket *sock = new_socket(L);
  sock->fd = fd;

  return 1;
}

static int tcp_bind(lua_State *L) {
  const char *address = luaL_checkstring(L, 1);
  int port = (int)luaL_checkinteger(L, 2);
  int backlog = (int)luaL_optinteger(L, 3, DEFAULT_BACKLOG);

  init_winsock(L);

  struct sockaddr_storage addr;
  socklen_t addrlen;

  resolve_hostname(L, address, port, &addr, &addrlen);

  socket_t fd = socket(addr.ss_family, SOCK_STREAM, IPPROTO_TCP);
  if (fd == SOCKET_INVALID) {
    return luaL_error(L, "cannot create socket: %s",
                      sock_strerror(SOCKET_ERRNO));
  }

  /* Allow address reuse */
  int optval = 1;
  setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (const char *)&optval,
             sizeof(optval));

  if (bind(fd, (struct sockaddr *)&addr, addrlen) == SOCKET_ERROR_VAL) {
    int err = SOCKET_ERRNO;
    sock_close(fd);
    return luaL_error(L, "cannot bind to %s:%d: %s", address, port,
                      sock_strerror(err));
  }

  if (listen(fd, backlog) == SOCKET_ERROR_VAL) {
    int err = SOCKET_ERRNO;
    sock_close(fd);
    return luaL_error(L, "cannot listen on %s:%d: %s", address, port,
                      sock_strerror(err));
  }

  LServer *srv = new_server(L);
  srv->fd = fd;

  return 1;
}

static const luaL_Reg tcp_funcs[] = {
    {"connect", tcp_connect}, {"bind", tcp_bind}, {NULL, NULL}};

/* }====================================================== */

/*
** {======================================================
** network.udp functions
** =======================================================
*/

static int udp_open(lua_State *L) {
  int port = (int)luaL_optinteger(L, 1, 0);
  const char *address = luaL_optstring(L, 2, NULL);

  init_winsock(L);

  socket_t fd = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
  if (fd == SOCKET_INVALID) {
    return luaL_error(L, "cannot create UDP socket: %s",
                      sock_strerror(SOCKET_ERRNO));
  }

  LUDPSocket *sock = new_udpsocket(L);
  sock->fd = fd;

  /* Bind if port specified */
  if (port > 0 || address != NULL) {
    struct sockaddr_in addr;
    memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_port = htons((unsigned short)port);

    if (address) {
      if (inet_pton(AF_INET, address, &addr.sin_addr) != 1) {
        sock_close(fd);
        return luaL_error(L, "invalid address: %s", address);
      }
    } else {
      addr.sin_addr.s_addr = INADDR_ANY;
    }

    if (bind(fd, (struct sockaddr *)&addr, sizeof(addr)) == SOCKET_ERROR_VAL) {
      int err = SOCKET_ERRNO;
      sock_close(fd);
      return luaL_error(L, "cannot bind UDP socket: %s", sock_strerror(err));
    }
    sock->bound = 1;
  }

  return 1;
}

static const luaL_Reg udp_funcs[] = {{"open", udp_open}, {NULL, NULL}};

/* }====================================================== */

/*
** {======================================================
** network.fetch (HTTP/HTTPS client)
** =======================================================
*/

/* URL parsing helper */
typedef struct {
  char scheme[16];
  char host[256];
  int port;
  char path[2048];
} ParsedURL;

static int parse_url(const char *url, ParsedURL *parsed) {
  memset(parsed, 0, sizeof(*parsed));
  strcpy(parsed->path, "/");

  /* Extract scheme */
  const char *p = strstr(url, "://");
  if (!p)
    return -1;

  size_t scheme_len = (size_t)(p - url);
  if (scheme_len >= sizeof(parsed->scheme))
    return -1;
  memcpy(parsed->scheme, url, scheme_len);
  parsed->scheme[scheme_len] = '\0';

  /* Set default port based on scheme */
  if (strcmp(parsed->scheme, "http") == 0) {
    parsed->port = 80;
  } else if (strcmp(parsed->scheme, "https") == 0) {
    parsed->port = 443;
  } else {
    return -1; /* unsupported scheme */
  }

  p += 3; /* skip "://" */

  /* Extract host (and optional port) */
  const char *host_end = p;
  while (*host_end && *host_end != '/' && *host_end != ':') {
    host_end++;
  }

  size_t host_len = (size_t)(host_end - p);
  if (host_len == 0 || host_len >= sizeof(parsed->host))
    return -1;
  memcpy(parsed->host, p, host_len);
  parsed->host[host_len] = '\0';

  /* Check for port */
  if (*host_end == ':') {
    parsed->port = atoi(host_end + 1);
    while (*host_end && *host_end != '/')
      host_end++;
  }

  /* Extract path */
  if (*host_end == '/') {
    size_t path_len = strlen(host_end);
    if (path_len >= sizeof(parsed->path))
      return -1;
    strcpy(parsed->path, host_end);
  }

  return 0;
}

/* Read HTTP response line by line */
static int read_http_line(LSocket *sock, char *buf, size_t bufsize) {
  size_t pos = 0;

  /* First check buffered data */
  while (sock->buflen > 0 && pos < bufsize - 1) {
    char c = sock->buffer[0];

    /* Shift buffer */
    sock->buflen--;
    if (sock->buflen > 0) {
      memmove(sock->buffer, sock->buffer + 1, sock->buflen);
    }

    if (c == '\n') {
      /* Strip trailing \r if present */
      if (pos > 0 && buf[pos - 1] == '\r') {
        pos--;
      }
      buf[pos] = '\0';
      return (int)pos;
    }
    buf[pos++] = c;
  }

  /* Continue reading from socket */
  while (pos < bufsize - 1) {
    char c;
    ssize_t got;

    if (sock->ssl) {
      got = SSL_read(sock->ssl, &c, 1);
      if (got <= 0) {
        buf[pos] = '\0';
        return (got == 0) ? (int)pos : -1;
      }
    } else {
      got = recv(sock->fd, &c, 1, 0);
      if (got <= 0) {
        buf[pos] = '\0';
        return (got == 0) ? (int)pos : -1;
      }
    }

    if (c == '\n') {
      if (pos > 0 && buf[pos - 1] == '\r') {
        pos--;
      }
      buf[pos] = '\0';
      return (int)pos;
    }
    buf[pos++] = c;
  }

  buf[pos] = '\0';
  return (int)pos;
}

static int net_fetch(lua_State *L) {
  const char *url = luaL_checkstring(L, 1);
  const char *method = luaL_optstring(L, 2, "GET");
  /* headers table at index 3 (optional) */
  size_t body_len = 0;
  const char *body = luaL_optlstring(L, 4, NULL, &body_len);

  ParsedURL parsed;
  if (parse_url(url, &parsed) != 0) {
    return luaL_error(L, "invalid URL: %s", url);
  }

  init_winsock(L);

  /* Resolve hostname */
  struct sockaddr_storage addr;
  socklen_t addrlen;
  resolve_hostname(L, parsed.host, parsed.port, &addr, &addrlen);

  /* Create socket */
  socket_t fd = socket(addr.ss_family, SOCK_STREAM, IPPROTO_TCP);
  if (fd == SOCKET_INVALID) {
    return luaL_error(L, "cannot create socket: %s",
                      sock_strerror(SOCKET_ERRNO));
  }

  /* Connect */
  if (connect(fd, (struct sockaddr *)&addr, addrlen) == SOCKET_ERROR_VAL) {
    int err = SOCKET_ERRNO;
    sock_close(fd);
    return luaL_error(L, "cannot connect to %s:%d: %s", parsed.host,
                      parsed.port, sock_strerror(err));
  }

  /* Create socket userdata for cleanup */
  LSocket sock;
  memset(&sock, 0, sizeof(sock));
  sock.fd = fd;
  sock.timeout_ms = 30000; /* 30 second timeout */

  /* SSL handshake for HTTPS */
  if (strcmp(parsed.scheme, "https") == 0) {
    init_ssl(L);

    sock.ssl = SSL_new(ssl_ctx);
    if (!sock.ssl) {
      sock_close(fd);
      return luaL_error(L, "SSL_new failed");
    }

    SSL_set_fd(sock.ssl, (int)fd);
    SSL_set_tlsext_host_name(sock.ssl, parsed.host);

    if (SSL_connect(sock.ssl) != 1) {
      SSL_free(sock.ssl);
      sock_close(fd);
      return push_ssl_error(L, "SSL handshake failed");
    }
  }

  /* Build request */
  luaL_Buffer req;
  luaL_buffinit(L, &req);

  /* Request line */
  lua_pushfstring(L, "%s %s HTTP/1.1\r\n", method, parsed.path);
  luaL_addvalue(&req);

  /* Host header */
  if ((strcmp(parsed.scheme, "http") == 0 && parsed.port != 80) ||
      (strcmp(parsed.scheme, "https") == 0 && parsed.port != 443)) {
    lua_pushfstring(L, "Host: %s:%d\r\n", parsed.host, parsed.port);
  } else {
    lua_pushfstring(L, "Host: %s\r\n", parsed.host);
  }
  luaL_addvalue(&req);

  /* Connection header */
  luaL_addstring(&req, "Connection: close\r\n");

  /* User-Agent */
  luaL_addstring(&req, "User-Agent: Lus/1.0\r\n");

  /* Custom headers */
  if (lua_istable(L, 3)) {
    lua_pushnil(L);
    while (lua_next(L, 3) != 0) {
      const char *key = lua_tostring(L, -2);
      const char *val = lua_tostring(L, -1);
      if (key && val) {
        lua_pushfstring(L, "%s: %s\r\n", key, val);
        luaL_addvalue(&req);
      }
      lua_pop(L, 1);
    }
  }

  /* Content-Length if body present */
  if (body && body_len > 0) {
    lua_pushfstring(L, "Content-Length: %d\r\n", (int)body_len);
    luaL_addvalue(&req);
  }

  /* End of headers */
  luaL_addstring(&req, "\r\n");

  /* Body */
  if (body && body_len > 0) {
    luaL_addlstring(&req, body, body_len);
  }

  luaL_pushresult(&req);
  size_t req_len;
  const char *req_data = lua_tolstring(L, -1, &req_len);

  /* Send request */
  size_t sent = 0;
  while (sent < req_len) {
    ssize_t n;
    if (sock.ssl) {
      n = SSL_write(sock.ssl, req_data + sent, (int)(req_len - sent));
    } else {
      n = send(fd, req_data + sent, (int)(req_len - sent), 0);
    }
    if (n <= 0) {
      if (sock.ssl)
        SSL_free(sock.ssl);
      sock_close(fd);
      return luaL_error(L, "failed to send request");
    }
    sent += (size_t)n;
  }
  lua_pop(L, 1); /* pop request string */

  /* Read status line */
  char line[4096];
  if (read_http_line(&sock, line, sizeof(line)) < 0) {
    if (sock.ssl)
      SSL_free(sock.ssl);
    sock_close(fd);
    return luaL_error(L, "failed to read response");
  }

  /* Parse status code */
  int status = 0;
  if (sscanf(line, "HTTP/%*d.%*d %d", &status) != 1) {
    if (sock.ssl)
      SSL_free(sock.ssl);
    sock_close(fd);
    return luaL_error(L, "invalid HTTP response: %s", line);
  }

  /* Read headers */
  lua_newtable(L);                 /* response headers table */
  int headers_idx = lua_gettop(L); /* save absolute index */
  int content_length = -1;
  int chunked = 0;

  while (read_http_line(&sock, line, sizeof(line)) > 0) {
    char *colon = strchr(line, ':');
    if (colon) {
      *colon = '\0';
      char *value = colon + 1;
      while (*value == ' ')
        value++;

      /* Store header (lowercase key) */
      for (char *p = line; *p; p++)
        *p = (char)tolower((unsigned char)*p);
      lua_pushstring(L, value);
      lua_setfield(L, headers_idx, line);

      /* Check for Content-Length and Transfer-Encoding */
      if (strcmp(line, "content-length") == 0) {
        content_length = atoi(value);
      } else if (strcmp(line, "transfer-encoding") == 0) {
        if (strstr(value, "chunked"))
          chunked = 1;
      }
    }
  }

  /* Read body */
  luaL_Buffer resp_body;
  luaL_buffinit(L, &resp_body);

  if (chunked) {
    /* Chunked transfer encoding */
    for (;;) {
      if (read_http_line(&sock, line, sizeof(line)) < 0)
        break;

      int chunk_size = 0;
      sscanf(line, "%x", &chunk_size);
      if (chunk_size == 0)
        break;

      /* Read chunk data */
      int remaining = chunk_size;
      char buf[4096];
      while (remaining > 0) {
        int to_read = remaining;
        if (to_read > (int)sizeof(buf))
          to_read = (int)sizeof(buf);

        ssize_t got;
        if (sock.ssl) {
          got = SSL_read(sock.ssl, buf, to_read);
        } else {
          got = recv(fd, buf, to_read, 0);
        }
        if (got <= 0)
          break;

        luaL_addlstring(&resp_body, buf, (size_t)got);
        remaining -= (int)got;
      }

      /* Read trailing CRLF */
      read_http_line(&sock, line, sizeof(line));
    }
  } else if (content_length >= 0) {
    /* Fixed Content-Length */
    int remaining = content_length;
    char buf[4096];
    while (remaining > 0) {
      int to_read = remaining;
      if (to_read > (int)sizeof(buf))
        to_read = (int)sizeof(buf);

      ssize_t got;
      if (sock.ssl) {
        got = SSL_read(sock.ssl, buf, to_read);
      } else {
        got = recv(fd, buf, to_read, 0);
      }
      if (got <= 0)
        break;

      luaL_addlstring(&resp_body, buf, (size_t)got);
      remaining -= (int)got;
    }
  } else {
    /* Read until connection close */
    char buf[4096];
    for (;;) {
      ssize_t got;
      if (sock.ssl) {
        got = SSL_read(sock.ssl, buf, sizeof(buf));
        if (got <= 0) {
          int ssl_err = SSL_get_error(sock.ssl, (int)got);
          if (ssl_err == SSL_ERROR_ZERO_RETURN)
            break;
          if (got < 0)
            break;
        }
      } else {
        got = recv(fd, buf, sizeof(buf), 0);
        if (got <= 0)
          break;
      }
      if (got > 0) {
        luaL_addlstring(&resp_body, buf, (size_t)got);
      }
    }
  }

  /* Cleanup */
  if (sock.ssl) {
    SSL_shutdown(sock.ssl);
    SSL_free(sock.ssl);
  }
  sock_close(fd);

  /* Push the result body string */
  luaL_pushresult(&resp_body); /* Stack: [headers, ..., body] */

  /* Clean up stack - we only want headers and body */
  /* At this point: headers_idx has headers, top has body */
  /* We need to return: status, body, headers */

  /* First, get body string and save it */
  lua_insert(L,
             headers_idx +
                 1); /* Move body right after headers: [headers, body, ...] */
  lua_settop(L, headers_idx +
                    1); /* Discard anything above body: [headers, body] */

  /* Now push status at the beginning */
  lua_pushinteger(L, status); /* [headers, body, status] */
  lua_insert(L, headers_idx); /* [status, headers, body] */

  /* Swap headers and body to get [status, body, headers] */
  lua_pushvalue(L, -1); /* [status, headers, body, body] */
  lua_pushvalue(L, -3); /* [status, headers, body, body, headers] */
  lua_remove(L, -4);    /* [status, body, body, headers] */
  lua_remove(L, -3);    /* [status, body, headers] */

  return 3;
}

/* }====================================================== */

/*
** {======================================================
** Library Registration
** =======================================================
*/

static void create_metatable(lua_State *L, const char *name,
                             const luaL_Reg *methods) {
  luaL_newmetatable(L, name);
  lua_pushvalue(L, -1);
  lua_setfield(L, -2, "__index");
  luaL_setfuncs(L, methods, 0);
  lua_pop(L, 1);
}

static const luaL_Reg network_funcs[] = {{"fetch", net_fetch}, {NULL, NULL}};

LUAMOD_API int luaopen_network(lua_State *L) {
  /* Create metatables */
  create_metatable(L, SOCKET_METATABLE, socket_methods);
  create_metatable(L, SERVER_METATABLE, server_methods);
  create_metatable(L, UDPSOCKET_METATABLE, udpsocket_methods);

  /* Create main network table */
  luaL_newlib(L, network_funcs);

  /* Create network.tcp subtable */
  luaL_newlib(L, tcp_funcs);
  lua_setfield(L, -2, "tcp");

  /* Create network.udp subtable */
  luaL_newlib(L, udp_funcs);
  lua_setfield(L, -2, "udp");

  return 1;
}

/* }====================================================== */
