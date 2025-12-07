/*
** lfslib.c
** Filesystem library for Lus
** Implements fs.* global functions
*/

#define lfslib_c
#define LUA_LIB

#include "lprefix.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lauxlib.h"
#include "lua.h"
#include "lualib.h"

/* Platform specific headers */
#if defined(LUS_PLATFORM_WINDOWS)
#include <direct.h>
#include <windows.h>
#define mkdir(p) _mkdir(p)
#else
#include <dirent.h>
#include <fcntl.h>
#include <fnmatch.h>
#include <sys/stat.h>
#include <unistd.h>
#endif

/*
** {======================================================
** fs.path functions
** =======================================================
*/

#if defined(LUS_PLATFORM_WINDOWS)
#define FS_DIRSEP '\\'
#define FS_DIRSEPSTR "\\"
#define FS_PATH_SEP ";"
#else
#define FS_DIRSEP '/'
#define FS_DIRSEPSTR "/"
#define FS_PATH_SEP ":"
#endif

/* Helper: check if character is a path separator */
static int is_sep(char c) {
#if defined(LUS_PLATFORM_WINDOWS)
  return c == '\\' || c == '/';
#else
  return c == '/';
#endif
}

/* Helper: check if path is absolute */
static int is_absolute(const char *path, size_t len) {
  if (len == 0) return 0;
#if defined(LUS_PLATFORM_WINDOWS)
  /* Absolute: starts with \ or / or has drive letter like C:\ */
  if (is_sep(path[0])) return 1;
  if (len >= 3 && ((path[0] >= 'A' && path[0] <= 'Z') ||
                   (path[0] >= 'a' && path[0] <= 'z')) &&
      path[1] == ':' && is_sep(path[2]))
    return 1;
  return 0;
#else
  return path[0] == '/';
#endif
}

static int fs_path_join(lua_State *L) {
  int n = lua_gettop(L);
  if (n == 0) {
    lua_pushliteral(L, "");
    return 1;
  }

  luaL_Buffer b;
  luaL_buffinit(L, &b);
  size_t result_len = 0;

  for (int i = 1; i <= n; i++) {
    size_t len;
    const char *s = luaL_checklstring(L, i, &len);

    if (len == 0) continue; /* Skip empty components */

    /* If this component is absolute, it replaces everything before it */
    if (is_absolute(s, len)) {
      /* Reset buffer by re-initializing */
      luaL_buffinit(L, &b);
      result_len = 0;
      luaL_addlstring(&b, s, len);
      result_len = len;
      continue;
    }

    /* Add separator if needed: result is non-empty and doesn't end with sep,
       and current component doesn't start with sep */
    if (result_len > 0) {
      /* Check last char in buffer - we track result_len for this */
      char *buf = luaL_buffaddr(&b);
      if (!is_sep(buf[result_len - 1]) && !is_sep(s[0])) {
        luaL_addchar(&b, FS_DIRSEP);
        result_len++;
      }
    }

    luaL_addlstring(&b, s, len);
    result_len += len;
  }

  luaL_pushresult(&b);
  return 1;
}

// Simple split implementation
static int fs_path_split(lua_State *L) {
  const char *path = luaL_checkstring(L, 1);
  lua_newtable(L);
  int i = 1;
  const char *start = path;
  const char *p = path;
  while (*p) {
    if (*p == FS_DIRSEP ||
        *p == '/') { // Handle both / and \ on Windows? stick to platform sep
                     // primarily but allow / as common
      lua_pushlstring(L, start, p - start);
      lua_rawseti(L, -2, i++);
      start = p + 1;
    }
    p++;
  }
  if (p > start) {
    lua_pushlstring(L, start, p - start);
    lua_rawseti(L, -2, i++);
  }
  return 1;
}

static int fs_path_name(lua_State *L) {
  const char *path = luaL_checkstring(L, 1);
  const char *last_sep = strrchr(path, FS_DIRSEP);
#if defined(LUS_PLATFORM_WINDOWS)
  const char *alt_sep = strrchr(path, '/');
  if (alt_sep > last_sep)
    last_sep = alt_sep;
#endif

  if (last_sep) {
    lua_pushstring(L, last_sep + 1);
  } else {
    lua_pushstring(L, path);
  }
  return 1;
}

static int fs_path_parent(lua_State *L) {
  const char *path = luaL_checkstring(L, 1);
  const char *last_sep = strrchr(path, FS_DIRSEP);
#if defined(LUS_PLATFORM_WINDOWS)
  const char *alt_sep = strrchr(path, '/');
  if (alt_sep > last_sep)
    last_sep = alt_sep;
#endif

  if (last_sep) {
    if (last_sep == path) { // Root
      lua_pushlstring(L, path, 1);
    } else {
      lua_pushlstring(L, path, last_sep - path);
    }
  } else {
    lua_pushliteral(L, ".");
  }
  return 1;
}

/* }====================================================== */

/*
** {======================================================
** fs.* functions
** =======================================================
*/

// Glob matching helper
static int glob_match(const char *pattern, const char *string) {
#if defined(LUS_PLATFORM_WINDOWS)
  // Simple wildcard matching for Windows (could be improved)
  const char *p = pattern, *s = string;
  while (*p) {
    if (*p == '*') {
      while (*(p + 1) == '*')
        p++; // skip multiple *
      if (*(p + 1) == 0)
        return 1; // * at end matches everything
      const char *next = p + 1;
      while (*s) {
        if (glob_match(next, s))
          return 1;
        s++;
      }
      return 0;
    } else if (*p == '?') {
      if (!*s)
        return 0;
      p++;
      s++;
    } else {
      if (*p != *s)
        return 0;
      p++;
      s++;
    }
  }
  return !*s;
#else
  return fnmatch(pattern, string, 0) == 0;
#endif
}

static int fs_list(lua_State *L) {
  const char *path = luaL_checkstring(L, 1);
  const char *glob = luaL_optstring(L, 2, NULL);

  lua_newtable(L);
  int i = 1;

#if defined(LUS_PLATFORM_WINDOWS)
  char search_path[MAX_PATH];
  size_t path_len = strlen(path);
  if (path_len + 3 > MAX_PATH) { /* path + \* + null */
    return luaL_error(L, "path too long: '%s'", path);
  }
  snprintf(search_path, sizeof(search_path), "%s\\*", path);
  WIN32_FIND_DATA fd;
  HANDLE hFind = FindFirstFile(search_path, &fd);
  if (hFind == INVALID_HANDLE_VALUE) {
    DWORD err = GetLastError();
    if (err == ERROR_FILE_NOT_FOUND || err == ERROR_PATH_NOT_FOUND)
      return luaL_error(L, "cannot list directory '%s': path not found", path);
    return luaL_error(L, "cannot list directory '%s': error %lu", path,
                      (unsigned long)err);
  }
  do {
    if (strcmp(fd.cFileName, ".") != 0 && strcmp(fd.cFileName, "..") != 0) {
      if (glob == NULL || glob_match(glob, fd.cFileName)) {
        lua_pushstring(L, fd.cFileName);
        lua_rawseti(L, -2, i++);
      }
    }
  } while (FindNextFile(hFind, &fd));
  FindClose(hFind);
#else
  DIR *d = opendir(path);
  if (d == NULL) {
    if (errno == ENOTDIR)
      return luaL_error(L, "%s is not a directory", path);
    return luaL_error(L, "cannot list directory %s: %s", path, strerror(errno));
  }
  struct dirent *dir;
  while ((dir = readdir(d)) != NULL) {
    if (strcmp(dir->d_name, ".") != 0 && strcmp(dir->d_name, "..") != 0) {
      if (glob == NULL || glob_match(glob, dir->d_name)) {
        lua_pushstring(L, dir->d_name);
        lua_rawseti(L, -2, i++);
      }
    }
  }
  closedir(d);
#endif
  return 1;
}

static int fs_copy(lua_State *L) {
  const char *src = luaL_checkstring(L, 1);
  const char *dst = luaL_checkstring(L, 2);

#if defined(LUS_PLATFORM_WINDOWS)
  if (!CopyFile(src, dst, TRUE)) { /* TRUE = fail if exists */
    DWORD err = GetLastError();
    if (err == ERROR_FILE_NOT_FOUND)
      return luaL_error(L, "cannot open source '%s': file not found", src);
    if (err == ERROR_FILE_EXISTS || err == ERROR_ALREADY_EXISTS)
      return luaL_error(L, "target '%s' already exists", dst);
    return luaL_error(L, "failed to copy '%s' to '%s': error %lu", src, dst,
                      (unsigned long)err);
  }
#else
  /* Check if target exists - use O_EXCL for atomic create */
  struct stat st;
  if (lstat(dst, &st) == 0) {
    return luaL_error(L, "target '%s' already exists", dst);
  }

  FILE *in = fopen(src, "rb");
  if (!in)
    return luaL_error(L, "cannot open source '%s': %s", src, strerror(errno));

  /* Open with O_EXCL via fdopen to avoid race condition */
  int out_fd = open(dst, O_WRONLY | O_CREAT | O_EXCL, 0666);
  if (out_fd < 0) {
    int saved_errno = errno;
    fclose(in);
    if (saved_errno == EEXIST)
      return luaL_error(L, "target '%s' already exists", dst);
    return luaL_error(L, "cannot create target '%s': %s", dst,
                      strerror(saved_errno));
  }
  FILE *out = fdopen(out_fd, "wb");
  if (!out) {
    int saved_errno = errno;
    close(out_fd);
    fclose(in);
    unlink(dst); /* Clean up partial file */
    return luaL_error(L, "cannot open target '%s': %s", dst,
                      strerror(saved_errno));
  }

  char buf[8192];
  size_t n;
  int copy_error = 0;

  while ((n = fread(buf, 1, sizeof(buf), in)) > 0) {
    if (fwrite(buf, 1, n, out) != n) {
      copy_error = errno;
      break;
    }
  }

  /* Check for read error */
  if (!copy_error && ferror(in)) {
    copy_error = errno ? errno : EIO;
  }

  fclose(in);
  if (fclose(out) != 0 && !copy_error) {
    copy_error = errno;
  }

  if (copy_error) {
    unlink(dst); /* Clean up partial file */
    return luaL_error(L, "error copying '%s' to '%s': %s", src, dst,
                      strerror(copy_error));
  }
#endif
  return 0;
}

static int fs_move(lua_State *L) {
  const char *src = luaL_checkstring(L, 1);
  const char *dst = luaL_checkstring(L, 2);

#if defined(LUS_PLATFORM_WINDOWS)
  /* MoveFile fails if dst exists, which is what we want */
  if (!MoveFileA(src, dst)) {
    DWORD err = GetLastError();
    if (err == ERROR_FILE_NOT_FOUND || err == ERROR_PATH_NOT_FOUND)
      return luaL_error(L, "cannot move '%s': source does not exist", src);
    if (err == ERROR_FILE_EXISTS || err == ERROR_ALREADY_EXISTS)
      return luaL_error(L, "cannot move to '%s': target already exists", dst);
    return luaL_error(L, "cannot move '%s' to '%s': error %lu", src, dst,
                      (unsigned long)err);
  }
#else
  /* Check if target exists using lstat (don't follow symlinks) */
  struct stat st;
  if (lstat(dst, &st) == 0) {
    return luaL_error(L, "cannot move to '%s': target already exists", dst);
  }
  if (errno != ENOENT) {
    /* Error other than "doesn't exist" */
    return luaL_error(L, "cannot check target '%s': %s", dst, strerror(errno));
  }
  if (rename(src, dst) != 0) {
    int err = errno;
    if (err == ENOENT)
      return luaL_error(L, "cannot move '%s': source does not exist", src);
    if (err == EXDEV)
      return luaL_error(L, "cannot move '%s' to '%s': cross-device move", src,
                        dst);
    return luaL_error(L, "cannot move '%s' to '%s': %s", src, dst,
                      strerror(err));
  }
#endif
  return 0;
}

static int fs_remove_recursive(const char *path) {
#if defined(LUS_PLATFORM_WINDOWS)
  DWORD attrs = GetFileAttributes(path);
  if (attrs == INVALID_FILE_ATTRIBUTES)
    return -1;

  /* Handle reparse points (symlinks) - don't follow them, just delete */
  if (attrs & FILE_ATTRIBUTE_REPARSE_POINT) {
    if (attrs & FILE_ATTRIBUTE_DIRECTORY)
      return RemoveDirectoryA(path) ? 0 : -1;
    else
      return DeleteFileA(path) ? 0 : -1;
  }

  if (attrs & FILE_ATTRIBUTE_DIRECTORY) {
    size_t path_len = strlen(path);
    if (path_len + 3 > MAX_PATH)
      return -1; /* Path too long */

    char search_path[MAX_PATH];
    snprintf(search_path, sizeof(search_path), "%s\\*", path);
    WIN32_FIND_DATA fd;
    HANDLE hFind = FindFirstFileA(search_path, &fd);
    if (hFind == INVALID_HANDLE_VALUE) {
      /* Empty directory or error - try to remove anyway */
      return RemoveDirectoryA(path) ? 0 : -1;
    }
    do {
      if (strcmp(fd.cFileName, ".") != 0 && strcmp(fd.cFileName, "..") != 0) {
        size_t name_len = strlen(fd.cFileName);
        if (path_len + 1 + name_len + 1 > MAX_PATH) {
          FindClose(hFind);
          return -1; /* Subpath too long */
        }
        char subpath[MAX_PATH];
        snprintf(subpath, sizeof(subpath), "%s\\%s", path, fd.cFileName);
        if (fs_remove_recursive(subpath) != 0) {
          FindClose(hFind);
          return -1;
        }
      }
    } while (FindNextFileA(hFind, &fd));
    FindClose(hFind);
    return RemoveDirectoryA(path) ? 0 : -1;
  } else {
    return DeleteFileA(path) ? 0 : -1;
  }
#else
  struct stat st;
  if (lstat(path, &st) != 0)
    return -1;

  /* Handle symlinks - don't follow them, just unlink */
  if (S_ISLNK(st.st_mode)) {
    return unlink(path);
  }

  if (S_ISDIR(st.st_mode)) {
    DIR *d = opendir(path);
    if (!d)
      return -1;
    struct dirent *dir;
    size_t path_len = strlen(path);

    while ((dir = readdir(d)) != NULL) {
      if (strcmp(dir->d_name, ".") != 0 && strcmp(dir->d_name, "..") != 0) {
        size_t name_len = strlen(dir->d_name);
/* Use PATH_MAX if available */
#ifdef PATH_MAX
        char subpath[PATH_MAX];
        if (path_len + 1 + name_len + 1 > PATH_MAX) {
          closedir(d);
          return -1; /* Path too long */
        }
#else
        char subpath[4096];
        if (path_len + 1 + name_len + 1 > sizeof(subpath)) {
          closedir(d);
          return -1;
        }
#endif
        snprintf(subpath, sizeof(subpath), "%s/%s", path, dir->d_name);
        if (fs_remove_recursive(subpath) != 0) {
          closedir(d);
          return -1;
        }
      }
    }
    closedir(d);
    return rmdir(path);
  } else {
    return unlink(path);
  }
#endif
}

static int fs_remove(lua_State *L) {
  const char *path = luaL_checkstring(L, 1);
  int recursive = lua_toboolean(L, 2);

#if defined(LUS_PLATFORM_WINDOWS)
  DWORD attrs = GetFileAttributes(path);
  if (attrs == INVALID_FILE_ATTRIBUTES) {
    DWORD err = GetLastError();
    if (err == ERROR_FILE_NOT_FOUND || err == ERROR_PATH_NOT_FOUND)
      return luaL_error(L, "cannot remove '%s': path does not exist", path);
    return luaL_error(L, "cannot remove '%s': error %lu", path,
                      (unsigned long)err);
  }
  if (attrs & FILE_ATTRIBUTE_DIRECTORY) {
    if (recursive) {
      if (fs_remove_recursive(path) != 0)
        return luaL_error(
            L, "failed to remove directory '%s' recursively: error %lu", path,
            (unsigned long)GetLastError());
    } else {
      if (!RemoveDirectoryA(path)) {
        DWORD err = GetLastError();
        if (err == ERROR_DIR_NOT_EMPTY)
          return luaL_error(L, "cannot remove directory '%s': not empty", path);
        return luaL_error(L, "cannot remove directory '%s': error %lu", path,
                          (unsigned long)err);
      }
    }
  } else {
    if (!DeleteFileA(path))
      return luaL_error(L, "cannot remove file '%s': error %lu", path,
                        (unsigned long)GetLastError());
  }
#else
  struct stat st;
  if (lstat(path, &st) != 0) {
    int err = errno;
    if (err == ENOENT)
      return luaL_error(L, "cannot remove '%s': path does not exist", path);
    return luaL_error(L, "cannot remove '%s': %s", path, strerror(err));
  }
  if (S_ISDIR(st.st_mode)) {
    if (recursive) {
      if (fs_remove_recursive(path) != 0)
        return luaL_error(L, "failed to remove directory '%s' recursively: %s",
                          path, strerror(errno));
    } else {
      if (rmdir(path) != 0) {
        int err = errno;
        if (err == ENOTEMPTY)
          return luaL_error(L, "cannot remove directory '%s': not empty", path);
        return luaL_error(L, "cannot remove directory '%s': %s", path,
                          strerror(err));
      }
    }
  } else {
    if (unlink(path) != 0)
      return luaL_error(L, "cannot remove '%s': %s", path, strerror(errno));
  }
#endif
  return 0;
}

static int fs_type(lua_State *L) {
  const char *path = luaL_checkstring(L, 1);

#if defined(LUS_PLATFORM_WINDOWS)
  DWORD attrs = GetFileAttributes(path);
  if (attrs == INVALID_FILE_ATTRIBUTES) {
    DWORD err = GetLastError();
    if (err == ERROR_FILE_NOT_FOUND || err == ERROR_PATH_NOT_FOUND)
      return luaL_error(L, "path '%s' does not exist", path);
    return luaL_error(L, "cannot get type of '%s': error %lu", path,
                      (unsigned long)err);
  }
  if (attrs & FILE_ATTRIBUTE_DIRECTORY)
    lua_pushliteral(L, "directory");
  else
    lua_pushliteral(L, "file");
  /* FILE_ATTRIBUTE_REPARSE_POINT indicates symlink (or junction) */
  lua_pushboolean(L, attrs & FILE_ATTRIBUTE_REPARSE_POINT);
#else
  struct stat st;
  if (lstat(path, &st) != 0) {
    int err = errno;
    if (err == ENOENT)
      return luaL_error(L, "path '%s' does not exist", path);
    return luaL_error(L, "cannot get type of '%s': %s", path, strerror(err));
  }
  if (S_ISDIR(st.st_mode))
    lua_pushliteral(L, "directory");
  else
    lua_pushliteral(L, "file");
  lua_pushboolean(L, S_ISLNK(st.st_mode));
#endif
  return 2;
}

static int fs_follow(lua_State *L) {
  const char *path = luaL_checkstring(L, 1);
#if defined(LUS_PLATFORM_WINDOWS)
  HANDLE hFile = CreateFile(path, GENERIC_READ, FILE_SHARE_READ, NULL,
                            OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);
  if (hFile == INVALID_HANDLE_VALUE) {
    DWORD err = GetLastError();
    if (err == ERROR_FILE_NOT_FOUND || err == ERROR_PATH_NOT_FOUND)
      return luaL_error(L, "path '%s' not found", path);
    return luaL_error(L, "cannot open '%s': error %lu", path, (unsigned long)err);
  }
  char buf[MAX_PATH];
  /* VOLUME_NAME_DOS returns path with drive letter, usually prefixed with \\?\  */
  DWORD len = GetFinalPathNameByHandle(hFile, buf, MAX_PATH, VOLUME_NAME_DOS);
  if (len == 0) {
    DWORD err = GetLastError();
    CloseHandle(hFile);
    return luaL_error(L, "failed to resolve path '%s': error %lu", path,
                      (unsigned long)err);
  }
  if (len >= MAX_PATH) {
    CloseHandle(hFile);
    return luaL_error(L, "resolved path too long for '%s'", path);
  }
  CloseHandle(hFile);

  /* Strip \\?\ prefix if present */
  const char *res = buf;
  if (strncmp(buf, "\\\\?\\", 4) == 0) {
    res = buf + 4;
  }
  lua_pushstring(L, res);
  return 1;
#else
  /* First check if it's actually a symlink */
  struct stat st;
  if (lstat(path, &st) != 0) {
    return luaL_error(L, "cannot stat '%s': %s", path, strerror(errno));
  }
  if (!S_ISLNK(st.st_mode)) {
    return luaL_error(L, "'%s' is not a symbolic link", path);
  }

  /* Use PATH_MAX if available, otherwise a reasonable default */
#ifdef PATH_MAX
  char buf[PATH_MAX];
#else
  char buf[4096];
#endif
  ssize_t len = readlink(path, buf, sizeof(buf) - 1);
  if (len == -1)
    return luaL_error(L, "cannot read link '%s': %s", path, strerror(errno));
  if ((size_t)len >= sizeof(buf) - 1)
    return luaL_error(L, "symlink target too long for '%s'", path);
  buf[len] = '\0';
  lua_pushstring(L, buf);
  return 1;
#endif
}

static int fs_createlink(lua_State *L) {
  const char *at = luaL_checkstring(L, 1);
  const char *target = luaL_checkstring(L, 2);

#if defined(LUS_PLATFORM_WINDOWS)
  /* Determine if target is a directory to use correct flag */
  DWORD flags = 0;
  DWORD attrs = GetFileAttributes(target);
  if (attrs != INVALID_FILE_ATTRIBUTES &&
      (attrs & FILE_ATTRIBUTE_DIRECTORY)) {
    flags = SYMBOLIC_LINK_FLAG_DIRECTORY;
  }
  /* SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE for dev mode (Win10+) */
  flags |= 0x2; /* SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE */

  if (!CreateSymbolicLinkA(at, target, flags)) {
    DWORD err = GetLastError();
    if (err == ERROR_PRIVILEGE_NOT_HELD)
      return luaL_error(
          L, "failed to create symlink '%s': requires administrator or "
             "developer mode",
          at);
    return luaL_error(L, "failed to create symlink '%s' -> '%s': error %lu", at,
                      target, (unsigned long)err);
  }
#else
  if (symlink(target, at) != 0)
    return luaL_error(L, "failed to create symlink '%s' -> '%s': %s", at,
                      target, strerror(errno));
#endif
  return 0;
}

/* Helper: create a single directory, returns 0 on success, 1 if exists, -1 on error */
static int create_single_dir(const char *path) {
#if defined(LUS_PLATFORM_WINDOWS)
  if (CreateDirectoryA(path, NULL))
    return 0;
  DWORD err = GetLastError();
  if (err == ERROR_ALREADY_EXISTS)
    return 1;
  return -1;
#else
  if (mkdir(path, 0777) == 0)
    return 0;
  if (errno == EEXIST)
    return 1;
  return -1;
#endif
}

/* Helper: create directory and all parent directories */
static int create_dirs_recursive(lua_State *L, const char *path) {
  size_t len = strlen(path);
  if (len == 0)
    return 0;

  /* Make a mutable copy */
#ifdef PATH_MAX
  char buf[PATH_MAX];
  if (len >= PATH_MAX)
    return luaL_error(L, "path too long: '%s'", path);
#else
  char buf[4096];
  if (len >= sizeof(buf))
    return luaL_error(L, "path too long: '%s'", path);
#endif
  memcpy(buf, path, len + 1);

  /* Walk through path and create each component */
  char *p = buf;

#if defined(LUS_PLATFORM_WINDOWS)
  /* Skip drive letter if present (e.g., "C:\") */
  if (len >= 2 && ((p[0] >= 'A' && p[0] <= 'Z') ||
                   (p[0] >= 'a' && p[0] <= 'z')) && p[1] == ':') {
    p += 2;
    if (*p == '\\' || *p == '/')
      p++;
  } else if (*p == '\\' || *p == '/') {
    p++;
  }
#else
  /* Skip leading slash for absolute paths */
  if (*p == '/')
    p++;
#endif

  while (*p) {
    /* Find next separator */
    while (*p && !is_sep(*p))
      p++;

    if (*p) {
      char saved = *p;
      *p = '\0';

      int res = create_single_dir(buf);
      if (res < 0) {
        /* Failed to create - check if it's a file blocking us */
#if defined(LUS_PLATFORM_WINDOWS)
        DWORD attrs = GetFileAttributes(buf);
        if (attrs != INVALID_FILE_ATTRIBUTES &&
            !(attrs & FILE_ATTRIBUTE_DIRECTORY))
          return luaL_error(L, "cannot create directory '%s': '%s' is a file",
                            path, buf);
        return luaL_error(L, "cannot create directory '%s': error at '%s'",
                          path, buf);
#else
        struct stat st;
        if (lstat(buf, &st) == 0 && !S_ISDIR(st.st_mode))
          return luaL_error(L, "cannot create directory '%s': '%s' is a file",
                            path, buf);
        return luaL_error(L, "cannot create directory '%s': %s at '%s'", path,
                          strerror(errno), buf);
#endif
      }

      *p = saved;
      p++;
    }
  }

  /* Create final directory */
  int res = create_single_dir(buf);
  if (res < 0) {
#if defined(LUS_PLATFORM_WINDOWS)
    return luaL_error(L, "cannot create directory '%s': error %lu", path,
                      (unsigned long)GetLastError());
#else
    return luaL_error(L, "cannot create directory '%s': %s", path,
                      strerror(errno));
#endif
  }

  return 0;
}

static int fs_createdirectory(lua_State *L) {
  const char *path = luaL_checkstring(L, 1);
  int recursive = lua_toboolean(L, 2);

  if (recursive) {
    return create_dirs_recursive(L, path);
  }

#if defined(LUS_PLATFORM_WINDOWS)
  if (!CreateDirectoryA(path, NULL)) {
    DWORD err = GetLastError();
    if (err == ERROR_ALREADY_EXISTS)
      return luaL_error(L, "cannot create directory '%s': already exists",
                        path);
    if (err == ERROR_PATH_NOT_FOUND)
      return luaL_error(L, "cannot create directory '%s': parent does not exist",
                        path);
    return luaL_error(L, "cannot create directory '%s': error %lu", path,
                      (unsigned long)err);
  }
#else
  if (mkdir(path, 0777) != 0) {
    int err = errno;
    if (err == EEXIST)
      return luaL_error(L, "cannot create directory '%s': already exists",
                        path);
    if (err == ENOENT)
      return luaL_error(L, "cannot create directory '%s': parent does not exist",
                        path);
    if (err == EACCES)
      return luaL_error(L, "cannot create directory '%s': permission denied",
                        path);
    return luaL_error(L, "cannot create directory '%s': %s", path,
                      strerror(err));
  }
#endif
  return 0;
}

static const luaL_Reg fslib[] = {{"list", fs_list},
                                 {"copy", fs_copy},
                                 {"move", fs_move},
                                 {"remove", fs_remove},
                                 {"type", fs_type},
                                 {"follow", fs_follow},
                                 {"createlink", fs_createlink},
                                 {"createdirectory", fs_createdirectory},
                                 {NULL, NULL}};

static const luaL_Reg fspathlib[] = {{"join", fs_path_join},
                                     {"split", fs_path_split},
                                     {"name", fs_path_name},
                                     {"parent", fs_path_parent},
                                     {NULL, NULL}};

LUAMOD_API int luaopen_fs(lua_State *L) {
  luaL_newlib(L, fslib);
  // Add fs.path
  luaL_newlib(L, fspathlib);
  // Add constants
  lua_pushliteral(L, FS_DIRSEPSTR);
  lua_setfield(L, -2, "separator");
  lua_pushliteral(L, FS_PATH_SEP);
  lua_setfield(L, -2, "delimiter");

  lua_setfield(L, -2, "path");
  return 1;
}
