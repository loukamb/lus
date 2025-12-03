# Changes between Lus and Lua

## TL;DR

- `pcall`/`xpcall` have been removed; use `catch <expr>` instead.
- Assignments can now be made in `if`/`elseif` statements.
- `?` suffix operator for safely navigating around nil values.
- `from` expression deconstructs a table's fields.

## `catch` expression

The `pcall` and `xpcall` functions have been removed from Lus. All protected execution is now done via `catch`, which computes any expression with protected execution and returns the status plus all results on success, or status plus error message on failure.

**Syntax:**

```lua
local status, results... = catch <expression>
```

**Return values:**

- On success: `true, <all expression results...>`
- On error: `false, <error message with stack trace>`

**Examples:**

```lua
-- Catch errors in function calls
function risky()
    error("Something went wrong!")
end

local ok, err = catch risky()
if not ok then
    print("Error:", err)
end

-- Catch errors in expressions
local ok, result = catch 1 + nil  -- ok=false, result=error message
local ok, result = catch 1 + 2    -- ok=true, result=3

-- Multi-return support (returns ALL values from expression)
function multi() return 1, 2, 3 end
local ok, a, b, c = catch multi()  -- ok=true, a=1, b=2, c=3

-- Works with method calls
local ok, val = catch obj:method()

-- Works in any expression context
print(catch someFunction())           -- prints all return values
return catch riskyOperation()         -- returns status and all results
local t = { catch getValue() }        -- table contains all values
```

**Nil-filling behavior:**

When assigning to more variables than the expression returns, extras are filled with `nil`:

```lua
function two() return 10, 20 end
local ok, a, b, c = catch two()  -- ok=true, a=10, b=20, c=nil

-- Same for error case
local x, y, z = catch error("oops")  -- x=false, y=error_msg, z=nil
```

## Assignments in `if`/`elseif` conditions

`if`/`elseif` statements can now list assignments, which survive for the entirety of the condition including any accompanying `elseif`/`else` blocks. If at least one assignment is false or nil, then the condition is false and the next `elseif`/`else` is executed.

**Examples:**

```lua
local function keepIfSumOverTen(a, b)
    local c = a + b
    return c > 10 and c or nil
end

if s = keepIfSumOverTen(9, 2) then
    print(s .. " is over ten!")
end

-- Also works with multiple values.
if a, b, c = 1, 5, 9 then
    print(a, b, c)
end

-- Will NOT work if ONE of the values are falsy.
if a, b, c = 3, 6, nil then
    print("Unexpected")
elseif d = 9 then
    -- Elseif blocks can also have values!
    -- Values previously declared are available here.
    print("At least one value was missing!", a, b, c, d)
end
```

## `?` conditional expression

The `?` operator enables safe navigation through potentially nil values. If the value before `?` is falsy (nil or false), subsequent suffix operations (field access, indexing, method calls, function calls) short-circuit and return the falsy value instead of raising an error.

**Examples:**

```lua
local t = { a = {} }
local x = t? -- `x` will be equal to `t`.
local y = t?.a -- `y` will be equal to `t.a`.
local z = t?.a?.r?.k?.e?.w -- `z` will be nil; `r?` cancels any following expression employing the value.
local k = (w and {} or nil)?.op -- `k` will be nil, as the expression between parens evals to nil.

local j = 1 + (w and 5 or nil)? -- This will error, as it is essentially the same as "1 + nil".
local i = 1 + (w and 5 or nil)?.a.g.x or 10 -- This will work, as it is essentially the same as "1 + nil or 10".

local e = t?["a"] -- Any operation works; it is '?' that is implemented, not '?.'.
```

## `from` expression

Deconstructing a table is possible with `from` in an assignment statement, local or global.

**Examples:**

```lua
local t = { a = 1, b = 2, c = 3, d = 4, e = 5, f = 6 }

-- Local deconstruction.
local c, a from t
assert(a == 1 and c == 3)

-- Global deconstruction.
f, e, d from t
global d, e, f from t -- Same as above; Lua 5.5 feature.
assert(d == 4 and e == 5 and f == 6)
```
