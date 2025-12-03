# Changes between Lus and Lua

## `catch` expression

The `catch` expression is implemented as a compile-time construct that generates two new VM opcodes, with error handling integrated into `luaD_throw`.

#### New Opcodes

**`OP_CATCH` (format: iAsBx)**

- `A`: Destination register for status (results start at A+1)
- `sBx`: Signed offset to jump on error (to instruction after ENDCATCH)

Sets up a protected execution context using `setjmp`. The jump buffer is stored in `CatchInfo` (part of `CallInfo`) to persist across VM instructions.

**`OP_ENDCATCH` (format: iABC)**

- `A`: Destination register (same as OP_CATCH)
- `B`: Expected number of results + 1 (0 = LUA_MULTRET, return all)
- `C`: Jump offset to skip past error handler

Marks successful completion: sets `R[A] = true`, nil-fills if needed, restores the previous error handler, and sets `L->top` appropriately.

#### Modified Structures

**`CatchInfo`** (added to `CallInfo.u.l` in `lstate.h`):

```c
typedef struct CatchInfo {
  jmp_buf jmpbuf;                    /* setjmp buffer for error recovery */
  struct lua_longjmp *prev_errorJmp; /* saved previous error handler */
  volatile TStatus status;           /* error status (if error occurred) */
  const Instruction *errorpc;        /* PC to jump to on error */
  lu_byte destreg;                   /* destination register for status */
  lu_byte nresults;                  /* expected number of results */
  lu_byte active;                    /* 1 if catch block is active */
} CatchInfo;
```

#### Error Handling Flow

1. **Setup (OP_CATCH):**

   - Saves `L->errorJmp` to `catchinfo.prev_errorJmp`
   - Calls `setjmp(catchinfo.jmpbuf)`
   - Sets `catchinfo.active = 1`
   - Normal path: continues to next instruction

2. **Error occurs (luaD_throw modified):**

   - Walks `CallInfo` chain looking for active catch
   - If found: sets `L->ci` to catch frame, `longjmp` to catch's jmpbuf
   - Error path in OP_CATCH: restores `cl`, `k`, `base` (stale after longjmp)
   - Sets `R[A] = false`, `R[A+1] = error message`
   - Reads nresults from ENDCATCH instruction, nil-fills remaining slots
   - Jumps to `errorpc` (past ENDCATCH)

3. **Success (OP_ENDCATCH):**
   - Restores `L->errorJmp` from `prev_errorJmp`
   - Sets `R[A] = true`
   - If B=0 (MULTRET): keeps L->top from inner expression
   - Otherwise: nil-fills up to expected nresults, sets L->top
   - Continues execution

#### Multi-Return Implementation

For inner expressions that are multi-return (VCALL, VVARARG):

- Parser calls `luaK_setmultret()` to let inner expression return all values
- ENDCATCH's B field encodes expected result count (set by `luaK_setreturns`)
- VM nil-fills if actual returns < expected, truncates if more

#### Files Modified

| File         | Changes                                             |
| ------------ | --------------------------------------------------- |
| `llex.h`     | Added `TK_CATCH` token                              |
| `llex.c`     | Added "catch" to reserved words                     |
| `lopcodes.h` | Added `OP_CATCH`, `OP_ENDCATCH`                     |
| `lopcodes.c` | Added opmode entries (ENDCATCH uses iABC)           |
| `lopnames.h` | Added opcode names for disassembler                 |
| `ljumptab.h` | Added jump table entries                            |
| `lstate.h`   | Added `CatchInfo` struct, `#include <setjmp.h>`     |
| `lparser.h`  | Added `VCATCH` expression kind                      |
| `lparser.c`  | Added `catchexpr()`, updated `hasmultret()`         |
| `lcode.c`    | Updated `luaK_setreturns()`, `luaK_dischargevars()` |
| `ldo.c`      | Modified `luaD_throw()` to check for active catch   |
| `lvm.c`      | Implemented `OP_CATCH` and `OP_ENDCATCH` handlers   |
| `luac.c`     | Added disassembly support for new opcodes           |
| `lbaselib.c` | Removed `pcall` and `xpcall` functions              |

#### Key Implementation Details

- The `jmp_buf` must be stored in `CatchInfo` (not as a local variable) because it needs to persist across VM instruction boundaries.
- After `longjmp`, local variables in `luaV_execute` are stale. The error path must refresh `ci`, `cl`, `k`, and `base` from `L->ci`.
- Register allocation follows VCALL conventions: `freereg = base + 1` after parsing, letting `adjust_assign` handle multi-value cases.
- For multi-return inner expressions, `luaK_setmultret()` is called to preserve all return values.
- ENDCATCH reads nresults from its B field and nil-fills/truncates accordingly.
- On error, nresults is read from the ENDCATCH instruction (at errorpc - 1) for proper nil-filling.

## Assignments in `if`/`elseif` conditions

The `if`/`elseif` assignment condition feature is implemented entirely in the parser (`lparser.c`), requiring no new opcodes or VM changes. It leverages existing local variable and test instruction machinery.

#### Parser Changes

**New function `isassigncond()`:**

Detects assignment conditions by checking if the current token is `TK_NAME` and the lookahead is `=` (single equals) or `,`. This distinguishes `if a = 1 then` from `if a == 1 then`.

**New function `assigncond()`:**

Parses the assignment condition syntax: `NAME { ',' NAME } '=' explist`

1. Parses variable names using `str_checkname()`, declaring each with `new_localvar()`
2. Consumes `=` and parses expression list with `explist()`
3. Uses `adjust_assign()` to handle value count mismatches (nil-filling)
4. Activates variables with `adjustlocalvars()` (values now in registers)
5. For each variable, creates a `VNONRELOC` expression and calls `luaK_goiftrue()` to generate a `TEST` instruction
6. Chains all false-jump lists together (if ANY variable is false/nil, jump to next condition)

**Modified `test_then_block()`:**

Now checks `isassigncond()` after skipping `IF`/`ELSEIF`. If true, calls `assigncond()` instead of `cond()` for condition parsing.

**Modified `ifstat()`:**

Wraps the entire `if`/`elseif`/`else`/`end` construct in an outer `BlockCnt` scope using `enterblock()`/`leaveblock()`. This ensures:

- Variables declared in any condition remain visible through all subsequent blocks
- Variables from a failed `if` condition are accessible in `elseif` and `else` blocks
- All condition variables go out of scope when `end` is reached

#### Code Generation

For `if a, b = x, y then`:

```
EVAL x -> R[base]          ; evaluate first expression
EVAL y -> R[base+1]        ; evaluate second expression
; (variables a, b now active)
TEST R[base]               ; if a is false, jump to false_branch
JMP false_branch
TEST R[base+1]             ; if b is false, jump to false_branch
JMP false_branch
... then block ...
JMP end                    ; escape to end (if elseif/else follows)
false_branch:              ; next condition or else block
```

Each variable's truthiness is tested with `OP_TEST`. The false-jump lists are concatenated, so if ANY value is false/nil, execution jumps to the next condition.

#### Scoping Implementation

The outer block in `ifstat()` creates a scope that encompasses:

- All `if`/`elseif` condition assignments
- All `then` blocks
- The `else` block (if any)

This differs from standard Lua where each block has its own scope. The nested `block()` calls create inner scopes, but condition variables live in the outer scope and remain accessible across all branches.

#### Files Modified

| File        | Changes                                                                             |
| ----------- | ----------------------------------------------------------------------------------- |
| `lparser.c` | Added `isassigncond()`, `assigncond()`, modified `test_then_block()` and `ifstat()` |

#### Key Implementation Details

- No new tokens, opcodes, or VM changes are required
- Detection relies on lookahead: `NAME` followed by `=` or `,` indicates assignment
- Multiple variables generate multiple `TEST` instructions with chained false-jumps
- The outer `BlockCnt` ensures proper variable lifetime across the entire if statement
- Variables from earlier conditions are visible in later conditions due to shared outer scope
- Standard `adjust_assign()` handles expression count mismatches (extra variables become nil, which causes the condition to fail)

## `?` conditional (optional chaining) expression

The `?` operator enables safe navigation through potentially nil values. If the value before `?` is falsy (nil or false), subsequent suffix operations (field access, indexing, method calls, function calls) short-circuit and return the falsy value instead of raising an error.

#### Parser Changes

The implementation modifies `suffixedexp()` in `lparser.c` to handle the `?` token as a special suffix operation.

**New function `discharge2basereg()`:**

Helper function that discharges an expression to a specific register (the "base register" for the optional chain). This ensures all operations in a chain write to the same register, so when we short-circuit, the register already contains the falsy value.

**Modified `suffixedexp()`:**

Tracks two new pieces of state:

- `niljumps`: A jump list for all short-circuit exits
- `basereg`: The register holding the chain result (-1 if not in a chain)

When `?` is encountered:

1. On first `?`: Put value in a fresh register (not a local!) and record as `basereg`
2. On subsequent `?`: Discharge current expression to `basereg`
3. Emit `TEST basereg 0` followed by `JMP` to test for falsy value
4. Add the jump to `niljumps`

After each suffix operation (`.`, `[]`, `:`, `()`), if in an optional chain, discharge the result to `basereg` to maintain the single-register invariant.

At the end of `suffixedexp()`, patch all `niljumps` to the current position.

#### Code Generation

For `t?.a?.b`:

```
GETTABUP R0 _ENV "t"      ; get t
MOVE     R1 R0            ; copy to chain register R1
TEST     R1 0             ; if R1 is truthy, skip next
JMP      L1               ; jump to end if falsy
GETFIELD R1 R1 "a"        ; R1 = t.a
TEST     R1 0             ; if R1 is truthy, skip next
JMP      L1               ; jump to end if falsy
GETFIELD R1 R1 "b"        ; R1 = t.a.b
L1:                       ; result in R1
```

Key insight: All operations write to the same register (`R1`). When we jump due to a falsy value, the register already contains that value, so no explicit nil-load is needed.

#### Register Management

Critical implementation detail: The base register must be a fresh register, not a local variable's register. If we used the local's register directly, we would corrupt the local's value during the chain operations.

The `discharge2basereg()` function ensures:

1. For `VRELOC` expressions: Sets the instruction's destination to `basereg`
2. For `VNONRELOC` in different register: Emits `MOVE` to `basereg`
3. Updates `freereg` to `basereg + 1` to prevent register allocation conflicts

#### Files Modified

| File        | Changes                                                                                    |
| ----------- | ------------------------------------------------------------------------------------------ |
| `lparser.c` | Added `discharge2basereg()`, modified `suffixedexp()` to handle `?` token and track chains |

#### Key Implementation Details

- No new tokens or opcodes required; `?` is handled as ASCII value 63
- Uses existing `TEST` opcode for falsy check (nil OR false)
- When falsy, short-circuits by jumping over remaining suffix operations
- The result is the original falsy value (not explicitly nil) because the register isn't modified after the jump
- Parenthesized expressions like `(expr)?.field` work naturally because each `suffixedexp()` call has its own chain state
- Deep chains like `t?.a?.b?.c?.d?.e` are efficient: each `?` adds only `TEST` + `JMP` (2 instructions)

## `from` table deconstruction expression

The `from` keyword enables extracting multiple fields from a table using the variable names as field keys. It works with local declarations, global declarations, and regular assignments.

**Syntax:**

```lua
local a, b, c from t    -- equivalent to: local a, b, c = t.a, t.b, t.c
global a, b, c from t   -- equivalent to: global a, b, c = t.a, t.b, t.c
a, b, c from t          -- equivalent to: a, b, c = t.a, t.b, t.c
```

#### Lexer Changes

Added `TK_FROM` to the reserved words enum in `llex.h` and the corresponding "from" string in `llex.c`.

#### Parser Changes

**New function `localfrom()`:**

Handles `local NAME { ',' NAME } from expr`:

1. Reserves registers for all local variables first (`luaK_reserveregs`)
2. Parses the source table expression into the next available register
3. For each variable, generates a `GETFIELD` instruction with the variable name as key, writing directly to the reserved register slot
4. Sets `freereg` to after the local variables (dropping the temp table register)
5. Activates the local variables with `adjustlocalvars()`

**New function `globalfrom()`:**

Handles `global NAME { ',' NAME } from expr`:

Uses a recursive structure (like `initglobal`):

1. Recursively builds global variable descriptions with `buildglobal()`
2. At the base case (n == nvars): reserves registers, parses table, generates `GETFIELD` for each field
3. On the way back: calls `checkglobal()` (Lua 5.5 global checking) and `storevartop()` to store each value

**New function `assignfrom()`:**

Handles bare `NAME { ',' NAME } from expr` assignment:

1. Collects variable names from the LHS chain (stored in reverse order)
2. Reserves registers for field values
3. Parses the source table expression
4. Generates `GETFIELD` for each field into reserved registers
5. Stores each value to its variable using `storevartop()`

**Modified `restassign()`:**

- Changed return type to `int` to signal whether `from` was used
- Added `TK_FROM` case that calls `assignfrom()` and returns 1
- Outer recursive calls check return value and skip `storevartop` if inner call used `from`

**Modified `exprstat()`:**

Added `TK_FROM` to the condition that triggers assignment parsing.

**Modified `globalnames()`:**

Added `TK_FROM` case that calls `globalfrom()` instead of `initglobal()`.

**Modified `localstat()`:**

Added `TK_FROM` case that calls `localfrom()` instead of the normal initialization path.

#### Code Generation

For `local a, b, c from t`:

```
; t is in R[0]
MOVE     R[4] R[0]       ; copy table to temp register
GETFIELD R[1] R[4] "a"   ; R[1] = t.a
GETFIELD R[2] R[4] "b"   ; R[2] = t.b
GETFIELD R[3] R[4] "c"   ; R[3] = t.c
; locals a, b, c are now in R[1], R[2], R[3]
```

For `global a, b, c from t`:

```
; t is in R[0]
MOVE     R[4] R[0]       ; copy table to temp register
GETFIELD R[1] R[4] "a"   ; R[1] = t.a
GETFIELD R[2] R[4] "b"   ; R[2] = t.b
GETFIELD R[3] R[4] "c"   ; R[3] = t.c
GETTABUP R[4] _ENV "c"   ; check _ENV.c exists (Lua 5.5)
ERRNNIL  R[4] "c"
SETTABUP _ENV "c" R[3]   ; _ENV.c = R[3]
GETTABUP R[3] _ENV "b"   ; check _ENV.b exists
ERRNNIL  R[3] "b"
SETTABUP _ENV "b" R[2]   ; _ENV.b = R[2]
GETTABUP R[2] _ENV "a"   ; check _ENV.a exists
ERRNNIL  R[2] "a"
SETTABUP _ENV "a" R[1]   ; _ENV.a = R[1]
```

#### Register Management

Critical insight: After reserving registers for field values, the table expression must go into a register AFTER those reserved slots. This prevents the `freereg(fs, tblreg)` calls in `luaK_dischargevars` from corrupting the field value registers.

The pattern used:

1. `base = fs->freereg` - remember where field values will go
2. `luaK_reserveregs(fs, nvars)` - reserve slots for field values
3. Parse table, put in next register (`luaK_exp2nextreg`)
4. Generate `GETFIELD` with explicit destination registers (`base + i`)
5. Set `freereg = base + nvars` - drop temp table register

#### Files Modified

| File        | Changes                                                                                                                    |
| ----------- | -------------------------------------------------------------------------------------------------------------------------- |
| `llex.h`    | Added `TK_FROM` to reserved words enum                                                                                     |
| `llex.c`    | Added "from" to token strings array                                                                                        |
| `lparser.c` | Added `localfrom()`, `globalfrom()`, `assignfrom()`, modified `localstat()`, `globalnames()`, `restassign()`, `exprstat()` |

#### Key Implementation Details

- `from` is a new reserved word (required because it appears in statement context)
- Variable names become field keys: `local x, y from t` accesses `t.x` and `t.y`
- Fields that don't exist evaluate to `nil` (standard Lua table semantics)
- The table expression is evaluated only once (not once per field)
- Works with Lua 5.5's global declaration system (`global` keyword and checking)
- No new opcodes required; uses existing `GETFIELD` instruction
