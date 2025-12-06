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
  'src/lenum.c',
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
fs = import('fs')
pkg.generate(liblua,
  name: 'lus',
  description: 'A small, productive programming language.',
  url: 'https://lus.dev/',
)

# Test suite
test_files = [
  'enum',
  'errors',
  'from',
  'ifassign',
  'optchain',
]


# H1: Language Integrity (Pure Lus)
h1_tests = [
  'h1/enum',
  'h1/errors',
  'h1/from',
  'h1/ifassign',
  'h1/optchain',
  'h1/bitwise',
  'h1/math',
  'h1/strings',
  'h1/attrib',
  'h1/calls',
  'h1/closure',
  'h1/constructs',
  'h1/coroutine',
  'h1/events',
  'h1/gc',
  'h1/goto',
  'h1/literals',
  'h1/locals',
  'h1/nextvar',
  'h1/pm',
  'h1/sort',
  'h1/tpack',
  'h1/utf8',
  'h1/vararg',
  'h1/db',
  # 'h1/big', maybe not now
  'h1/bwcoercion',
  'h1/cstack',
  'h1/gengc',
  'h1/memerr',
]

foreach t : h1_tests
  test('h1/' + fs.name(t), lua_exe,
    args: ['lus-tests' / t + '.lus'],
    workdir: meson.project_source_root() / '..',
    suite: 'h1',
  )
endforeach

# H2: C API (Compiled Executables)
h2_stack = executable('test_stack', '../lus-tests/h2/test_stack.c',
  include_directories: include_directories('src'),
  link_with: liblua,
  dependencies: lua_deps,
)
test('h2/stack', h2_stack, suite: 'h2')

h2_call = executable('test_call', '../lus-tests/h2/test_call.c',
  include_directories: include_directories('src'),
  link_with: liblua,
  dependencies: lua_deps,
)
test('h2/call', h2_call, suite: 'h2')


# H3: System Integration (Container Wrapper)
# Runs the Lus runner script
test('h3/io', lua_exe,
  args : ['lus-tests/h3/runner_local.lus'],
  workdir : meson.project_source_root() / '..',
  suite : ['h3', 'h3-local'])

test('h3/ci', lua_exe,
  args : ['lus-tests/h3/runner_direct.lus'],
  workdir : meson.project_source_root() / '..',
  suite : ['h3-ci'],
  timeout: 60,
)

# H4: Performance (Lus Runner)
test('h4/bench', lua_exe,
  args: ['lus-tests/h4/runner.lus'],
  workdir: meson.project_source_root() / '..',
  suite: 'h4',
  env: ['BUILD_TYPE=Unstable'], # Default to Unstable for local tests
)


