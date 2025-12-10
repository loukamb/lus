/*
** lglob.c
** Glob matching utilities for Lus
*/

#define lglob_c
#define LUA_CORE

#include "lprefix.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lglob.h"

/* Platform specific for canonicalization */
#if defined(LUS_PLATFORM_WINDOWS)
#include <windows.h>
#else
#include <fnmatch.h>
#include <limits.h>
#endif

/*
** Basic glob matching: * matches any sequence, ? matches one character.
** This is used on Windows (no fnmatch) and for URL matching.
*/
static int glob_match_internal(const char *pattern, const char *string) {
  const char *p = pattern;
  const char *s = string;

  while (*p) {
    if (*p == '*') {
      /* Skip consecutive asterisks */
      while (*(p + 1) == '*')
        p++;
      /* If * is at end, it matches everything remaining */
      if (*(p + 1) == '\0')
        return 1;
      /* Try matching the rest of pattern against each suffix of string */
      const char *next = p + 1;
      while (*s) {
        if (glob_match_internal(next, s))
          return 1;
        s++;
      }
      return 0;
    } else if (*p == '?') {
      /* ? matches exactly one character */
      if (*s == '\0')
        return 0;
      p++;
      s++;
    } else {
      /* Literal character match */
      if (*p != *s)
        return 0;
      p++;
      s++;
    }
  }
  return *s == '\0';
}

int lus_glob_match(const char *pattern, const char *string) {
#if defined(LUS_PLATFORM_WINDOWS)
  return glob_match_internal(pattern, string);
#else
  return fnmatch(pattern, string, 0) == 0;
#endif
}

int lus_glob_match_path(const char *pattern, const char *path,
                        int canonicalize) {
  if (!canonicalize) {
    return lus_glob_match(pattern, path);
  }

#if defined(LUS_PLATFORM_WINDOWS)
  /*
  ** On Windows, use GetFullPathName for canonicalization.
  ** Note: This doesn't resolve symlinks fully like realpath.
  */
  char canon_path[MAX_PATH];
  char canon_pattern[MAX_PATH];

  DWORD path_len = GetFullPathNameA(path, MAX_PATH, canon_path, NULL);
  if (path_len == 0 || path_len >= MAX_PATH) {
    /* Path canonicalization failed, use raw path */
    strcpy(canon_path, path);
  }

  /* Only canonicalize pattern if it looks like a path (no wildcards at start)
   */
  if (pattern[0] != '*' && pattern[0] != '?') {
    DWORD pattern_len =
        GetFullPathNameA(pattern, MAX_PATH, canon_pattern, NULL);
    if (pattern_len == 0 || pattern_len >= MAX_PATH) {
      /* Pattern canonicalization failed, use raw pattern */
      strcpy(canon_pattern, pattern);
    }
    return glob_match_internal(canon_pattern, canon_path);
  }
  return glob_match_internal(pattern, canon_path);
#else
  /*
  ** On POSIX, use realpath for canonicalization.
  ** This resolves symlinks and .. segments.
  */
  char canon_path[PATH_MAX];
  char canon_pattern[PATH_MAX];
  const char *pattern_to_use = pattern;

  /* Canonicalize the path being checked */
  if (realpath(path, canon_path) == NULL) {
    /* Path doesn't exist or error - use raw path */
    strncpy(canon_path, path, PATH_MAX - 1);
    canon_path[PATH_MAX - 1] = '\0';
  }

  /* Canonicalize the pattern if it looks like a path (starts with / or .) */
  if (pattern[0] == '/' || pattern[0] == '.') {
    /* Try to resolve the base part of the pattern (without wildcards) */
    size_t len = strlen(pattern);
    size_t i;

    /* Find where wildcards start */
    for (i = 0; i < len; i++) {
      if (pattern[i] == '*' || pattern[i] == '?') {
        break;
      }
    }

    if (i == len) {
      /* No wildcards - canonicalize the whole pattern */
      if (realpath(pattern, canon_pattern) != NULL) {
        pattern_to_use = canon_pattern;
      }
    } else if (i > 0) {
      /* Has wildcards - canonicalize the prefix */
      char prefix[PATH_MAX];
      strncpy(prefix, pattern, i);
      prefix[i] = '\0';

      /* Find last directory separator in prefix */
      char *last_sep = strrchr(prefix, '/');
      if (last_sep != NULL && last_sep != prefix) {
        *last_sep = '\0';
        char resolved_prefix[PATH_MAX];
        if (realpath(prefix, resolved_prefix) != NULL) {
          /* Rebuild pattern with resolved prefix */
          snprintf(canon_pattern, PATH_MAX, "%s%s", resolved_prefix,
                   pattern + (last_sep - prefix));
          pattern_to_use = canon_pattern;
        }
      } else if (last_sep == NULL && i > 0) {
        /* Pattern like "dir/*" - try to resolve the prefix as a directory */
        char resolved_prefix[PATH_MAX];
        if (realpath(prefix, resolved_prefix) != NULL) {
          snprintf(canon_pattern, PATH_MAX, "%s%s", resolved_prefix,
                   pattern + i);
          pattern_to_use = canon_pattern;
        }
      }
    }
  }

  return lus_glob_match(pattern_to_use, canon_path);
#endif
}

/*
** Helper: extract host from URL (handles http:// and https://)
*/
static const char *get_url_host(const char *url, size_t *host_len) {
  const char *p = url;

  /* Skip scheme */
  if (strncmp(p, "http://", 7) == 0) {
    p += 7;
  } else if (strncmp(p, "https://", 8) == 0) {
    p += 8;
  }

  /* Find end of host (port, path, or end of string) */
  const char *host_start = p;
  while (*p && *p != ':' && *p != '/' && *p != '?') {
    p++;
  }

  *host_len = p - host_start;
  return host_start;
}

int lus_glob_match_url(const char *pattern, const char *url) {
  /*
  ** URL pattern matching:
  ** - Pattern can be a domain: "example.com" matches "http://example.com/..."
  ** - Pattern can have wildcard subdomain: "*.example.com"
  ** - Pattern can have wildcard path: "example.com/*"
  ** - Pattern can be full URL with wildcards: "https://api.example.com/v1/*"
  */

  /* If pattern has no scheme, only match against host */
  if (strncmp(pattern, "http://", 7) != 0 &&
      strncmp(pattern, "https://", 8) != 0) {
    /* Pattern is host-only (possibly with wildcards and path) */
    size_t url_host_len;
    const char *url_host = get_url_host(url, &url_host_len);

    /* Check if pattern has a path component */
    const char *pattern_slash = strchr(pattern, '/');
    if (pattern_slash) {
      /* Pattern has path: match host + path */
      size_t pattern_host_len = pattern_slash - pattern;
      char host_buf[256];
      if (url_host_len >= sizeof(host_buf))
        return 0;
      memcpy(host_buf, url_host, url_host_len);
      host_buf[url_host_len] = '\0';

      /* Match host part */
      char pattern_host[256];
      if (pattern_host_len >= sizeof(pattern_host))
        return 0;
      memcpy(pattern_host, pattern, pattern_host_len);
      pattern_host[pattern_host_len] = '\0';

      if (!glob_match_internal(pattern_host, host_buf))
        return 0;

      /* Match path part */
      const char *url_path = url_host + url_host_len;
      if (*url_path == ':') {
        /* Skip port */
        while (*url_path && *url_path != '/')
          url_path++;
      }
      if (*url_path == '\0')
        url_path = "/";

      return glob_match_internal(pattern_slash, url_path);
    } else {
      /* Pattern is host only: match just the host */
      char host_buf[256];
      if (url_host_len >= sizeof(host_buf))
        return 0;
      memcpy(host_buf, url_host, url_host_len);
      host_buf[url_host_len] = '\0';

      return glob_match_internal(pattern, host_buf);
    }
  }

  /* Pattern has scheme: do full URL match */
  return glob_match_internal(pattern, url);
}
