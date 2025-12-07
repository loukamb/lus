project('lus', 'c',
  version: '1.2.0',
  default_options: [
    'c_std=gnu11',
    'warning_level=2',
    'optimization=2',
    'b_lto=true',
    'b_ndebug=if-release',
  ],
)

cc = meson.get_compiler('c')

# Math library (required)
m_dep = cc.find_library('m', required: false)

# Platform detection and configuration
system = host_machine.system()

lus_c_args = []
lus_link_args = []
lus_deps = [m_dep]

if system == 'linux'
  lus_c_args += ['-DLUA_USE_LINUX', '-DLUS_PLATFORM_LINUX']
  lus_link_args += '-Wl,-E'
  dl_dep = cc.find_library('dl', required: false)
  lus_deps += dl_dep
elif system == 'darwin'
  lus_c_args += ['-DLUA_USE_MACOSX', '-DLUA_USE_READLINE', '-DLUS_PLATFORM_DARWIN']
  readline_dep = cc.find_library('readline', required: true)
  lus_deps += readline_dep
elif system == 'freebsd'
  lus_c_args += ['-DLUA_USE_LINUX', '-DLUA_USE_READLINE', '-DLUS_PLATFORM_FREEBSD']
  lus_link_args += '-Wl,-E'
  edit_dep = cc.find_library('edit', required: true)
  lus_deps += edit_dep
elif system == 'netbsd'
  lus_c_args += ['-DLUA_USE_LINUX', '-DLUA_USE_READLINE', '-DLUS_PLATFORM_NETBSD']
  lus_link_args += '-Wl,-E'
  edit_dep = cc.find_library('edit', required: true)
  lus_deps += edit_dep
elif system == 'openbsd'
  lus_c_args += ['-DLUA_USE_LINUX', '-DLUA_USE_READLINE', '-DLUS_PLATFORM_OPENBSD']
  lus_link_args += '-Wl,-E'
  edit_dep = cc.find_library('edit', required: true)
  lus_deps += edit_dep
elif system == 'sunos'
  lus_c_args += ['-DLUA_USE_POSIX', '-DLUA_USE_DLOPEN', '-D_REENTRANT', '-DLUS_PLATFORM_SUNOS']
  dl_dep = cc.find_library('dl', required: false)
  lus_deps += dl_dep
elif system == 'windows'
  lus_c_args += '-DLUS_PLATFORM_WINDOWS'
else
  # Generic POSIX fallback
  lus_c_args += ['-DLUA_USE_POSIX', '-DLUS_PLATFORM_POSIX']
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
  'src/ljsonlib.c',
  'src/lfslib.c',
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
liblus = static_library('lus',
  core_sources + lib_sources,
  c_args: lus_c_args,
  dependencies: lus_deps,
  install: true,
)

# Lus interpreter
lus_exe = executable('lus',
  'src/lua.c',
  c_args: lus_c_args,
  link_args: lus_link_args,
  link_with: liblus,
  dependencies: lus_deps,
  install: true,
)

# Lus compiler
lusc_exe = executable('lusc',
  'src/luac.c',
  c_args: lus_c_args,
  link_args: lus_link_args,
  link_with: liblus,
  dependencies: lus_deps,
  install: true,
)

# pkg-config file
pkg = import('pkgconfig')
fs = import('fs')
pkg.generate(liblus,
  name: 'lus',
  description: 'A small, productive programming language.',
  url: 'https://lus.dev/',
)

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
  'h1/catchstat',
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
  'h1/transcode',
  'h1/utf8',
  'h1/vararg',
  'h1/db',
  # 'h1/big', maybe not now
  'h1/bwcoercion',
  'h1/cstack',
  'h1/gengc',
  'h1/memerr',
  'h1/json',
]

foreach t : h1_tests
  test('h1/' + fs.name(t), lus_exe,
    args: ['lus-tests' / t + '.lus'],
    workdir: meson.project_source_root() / '..',
    suite: 'h1',
  )
endforeach

# H2: C API (Compiled Executables)
h2_stack = executable('test_stack', '../lus-tests/h2/test_stack.c',
  include_directories: include_directories('src'),
  link_with: liblus,
  dependencies: lus_deps,
)
test('h2/stack', h2_stack, suite: 'h2')

h2_call = executable('test_call', '../lus-tests/h2/test_call.c',
  include_directories: include_directories('src'),
  link_with: liblus,
  dependencies: lus_deps,
)
test('h2/call', h2_call, suite: 'h2')


# H3: System Integration (Container Wrapper)
# Runs the Lus runner script
test('h3/io', lus_exe,
  args : ['lus-tests/h3/runner_local.lus'],
  workdir : meson.project_source_root() / '..',
  suite : ['h3', 'h3-local'])

# h3/ci runs directly on the host (CI environments only)
test('h3/ci', lus_exe,
  args : ['lus-tests/h3/runner_direct.lus'],
  workdir : meson.project_source_root() / '..',
  suite : ['h3-ci'],
  timeout: 60,
)

# H4: Performance (Lus Runner)
test('h4/bench', lus_exe,
  args: ['lus-tests/h4/runner.lus'],
  workdir: meson.project_source_root() / '..',
  suite: 'h4',
  env: ['BUILD_TYPE=Unstable'], # Default to Unstable for local tests
)


