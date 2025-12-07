#!/bin/bash
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SRC_DIR="$SCRIPT_DIR/../src"
OUT_DIR="$SCRIPT_DIR/dist"

mkdir -p "$OUT_DIR"

# Source files (core + libraries, excluding standalone interpreter main)
SOURCES=(
  "$SRC_DIR/lapi.c"
  "$SRC_DIR/lauxlib.c"
  "$SRC_DIR/lbaselib.c"
  "$SRC_DIR/lcode.c"
  "$SRC_DIR/lcorolib.c"
  "$SRC_DIR/lctype.c"
  "$SRC_DIR/ldblib.c"
  "$SRC_DIR/ldebug.c"
  "$SRC_DIR/ldo.c"
  "$SRC_DIR/ldump.c"
  "$SRC_DIR/lenum.c"
  "$SRC_DIR/lfunc.c"
  "$SRC_DIR/lfslib.c"
  "$SRC_DIR/lgc.c"
  "$SRC_DIR/linit.c"
  "$SRC_DIR/liolib.c"
  "$SRC_DIR/ljsonlib.c"
  "$SRC_DIR/llex.c"
  "$SRC_DIR/lmathlib.c"
  "$SRC_DIR/lmem.c"
  "$SRC_DIR/loadlib.c"
  "$SRC_DIR/lobject.c"
  "$SRC_DIR/lopcodes.c"
  "$SRC_DIR/loslib.c"
  "$SRC_DIR/lparser.c"
  "$SRC_DIR/lstate.c"
  "$SRC_DIR/lstring.c"
  "$SRC_DIR/lstrlib.c"
  "$SRC_DIR/ltable.c"
  "$SRC_DIR/ltablib.c"
  "$SRC_DIR/ltm.c"
  "$SRC_DIR/lundump.c"
  "$SRC_DIR/lutf8lib.c"
  "$SRC_DIR/lvm.c"
  "$SRC_DIR/lzio.c"
  "$SCRIPT_DIR/lus_wasm.c"
  "$SCRIPT_DIR/lev_wasm_stubs.c"
)

echo "Building Lus WASM..."

emcc "${SOURCES[@]}" \
  -I"$SRC_DIR" \
  -o "$OUT_DIR/lus.js" \
  -s MODULARIZE=1 \
  -s EXPORT_ES6=1 \
  -s EXPORTED_FUNCTIONS='["_lus_create","_lus_execute","_lus_destroy","_malloc","_free"]' \
  -s EXPORTED_RUNTIME_METHODS='["ccall","cwrap","UTF8ToString","stringToUTF8","lengthBytesUTF8"]' \
  -s ALLOW_MEMORY_GROWTH=1 \
  -s ENVIRONMENT='web,worker' \
  -s NO_EXIT_RUNTIME=1 \
  -O2 \
  -DLUA_USE_C89

echo "Build complete: $OUT_DIR/lus.js, $OUT_DIR/lus.wasm"

