project('lus', 'c',
  version: '5.5.0',
  default_options: [
    'c_std=gnu99',
    'warning_level=2',
    'optimization=2',
  ],
)

cc = meson.get_compiler('c')

# Math library (required)
m_dep = cc.find_library('m', required: false)

# Platform detection and configuration
system = host_machine.system()

lua_c_args = []
lua_link_args = []
lua_deps = [m_dep]

if system == 'linux'
  lua_c_args += '-DLUA_USE_LINUX'
  lua_link_args += '-Wl,-E'
  dl_dep = cc.find_library('dl', required: false)
  lua_deps += dl_dep
elif system == 'darwin'
  lua_c_args += ['-DLUA_USE_MACOSX', '-DLUA_USE_READLINE']
  readline_dep = cc.find_library('readline', required: true)
  lua_deps += readline_dep
elif system == 'freebsd' or system == 'netbsd' or system == 'openbsd'
  lua_c_args += ['-DLUA_USE_LINUX', '-DLUA_USE_READLINE']
  lua_link_args += '-Wl,-E'
  edit_dep = cc.find_library('edit', required: true)
  lua_deps += edit_dep
elif system == 'sunos'
  lua_c_args += ['-DLUA_USE_POSIX', '-DLUA_USE_DLOPEN', '-D_REENTRANT']
  dl_dep = cc.find_library('dl', required: false)
  lua_deps += dl_dep
elif system == 'windows'
  # Static build - no special defines needed
else
  # Generic POSIX fallback
  lua_c_args += '-DLUA_USE_POSIX'
endif

# Core source files
core_sources = files(
  'src/lapi.c',
  'src/lcode.c',
  'src/lctype.c',
  'src/ldebug.c',
  'src/ldo.c',
  'src/ldump.c',
  'src/lfunc.c',
  'src/lgc.c',
  'src/llex.c',
  'src/lmem.c',
  'src/lobject.c',
  'src/lopcodes.c',
  'src/lparser.c',
  'src/lstate.c',
  'src/lstring.c',
  'src/ltable.c',
  'src/ltm.c',
  'src/lundump.c',
  'src/lvm.c',
  'src/lzio.c',
)

# Library source files
lib_sources = files(
  'src/lauxlib.c',
  'src/lbaselib.c',
  'src/lcorolib.c',
  'src/ldblib.c',
  'src/liolib.c',
  'src/lmathlib.c',
  'src/loadlib.c',
  'src/loslib.c',
  'src/lstrlib.c',
  'src/ltablib.c',
  'src/lutf8lib.c',
  'src/linit.c',
)

# Headers to install
install_headers(
  'src/lua.h',
  'src/luaconf.h',
  'src/lualib.h',
  'src/lauxlib.h',
  'src/lua.hpp',
)

# Static library
liblua = static_library('lus',
  core_sources + lib_sources,
  c_args: lua_c_args,
  dependencies: lua_deps,
  install: true,
)

# Lua interpreter
lua_exe = executable('lus',
  'src/lua.c',
  c_args: lua_c_args,
  link_args: lua_link_args,
  link_with: liblua,
  dependencies: lua_deps,
  install: true,
)

# Lua compiler
luac_exe = executable('lusc',
  'src/luac.c',
  c_args: lua_c_args,
  link_args: lua_link_args,
  link_with: liblua,
  dependencies: lua_deps,
  install: true,
)

# pkg-config file
pkg = import('pkgconfig')
pkg.generate(liblua,
  name: 'lus',
  description: 'A small, productive programming language.',
  url: 'https://lus-lang.org/',
)

# Test suite
test_files = [
  'errors',
  'from',
  'ifassign',
  'optchain',
]

foreach t : test_files
  test(t, lua_exe,
    args: ['lus-tests' / t + '.lus'],
    workdir: meson.project_source_root() / '..',
  )
endforeach

