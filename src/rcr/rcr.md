# RCR

RCR is a low-level language which compiles to brainfuck. It turns brainfuck into
a regular language by allowing normal loops, variable, functions, comments, and
arrays to be used. It also has (overridable) mutability checks on variables.

Here are some code examples to help you understand what RCR looks like.

```rs
// While RCR works for standard procedural programming, it does not add much
// value to small programs. This null-terminated `cat` utility is quite verbose
// (although future plans will make this script much shorter).

fn zero(mut a) {
  while a {
    dec a;
  }
}

fn main() {
  mut x;
  read x;
  while x {
    write x;
    zero x;
    read x;
  }
}

// compiled result: ,[.[-],]
```

```rs
// RCR is better suited to larger programs, such as this one which computes a
// large arithmetic value.

// `add_into` takes advantage of RCR's return value syntax. Note that return
// values can only reference parameters.
fn add_into(a, mut b) -> b {
  mut c;
  while a {
    // `unsafe` lets us treat `a` as mutable, even though it's passed as
    // mutable. We make sure to keep `a`'s original value using the second loop.
    // This tells callers that we keep `a` constant, and tells the compiler to
    // trust us, as it's too simple to check `a`'s const-ness.
    unsafe dec a;
    inc b c;
  }
  while c {
    // this loop resets `a` back to its original value
    unsafe inc a;
  }
}

fn add_and_zero(mut a, mut b) -> b {
  while a {
    dec a;
    inc b;
  }
}

// `mul` uses a default value for `result` so that callers don't need to pass
// it. This can then be chained into other function calls.
fn mul(mut a, b, mut result = 0) -> result {
  while a {
    dec a;
    // Since `add_into` used `unsafe`, we have the freedom to declare `b` as
    // non-mutable. If our program seems to have incorrect behavior, we can
    // then quickly search for `unsafe` to find anywhere where mutability or
    // memory constraints are violated.
    add_into b result;
  }
}

fn main() {
  // If we want to compute 3*7 + 5*8, one way is to do...
  mut a = 3;
  let b = 7;
  mut c = 0;
  mul a b c;

  mut a = 5;
  let b = 8;
  mut c2 = 0;
  mul a b c2;

  add_and_zero c c2;
  // c2 now has the value `3*7 + 5*8`

  // However, there is an easier way.
  add_and_zero (mul 3 7) (mul 5 8 (mut result));
  // Notice how `(mul result)` is shorthand for declaring `result` and then
  // referencing it in the function call. This provides simple-ish syntax to put
  // the final value in `result`. Currently, this function call result cannot be
  // assigned to a local. This may change in the future.
}
```

```rs
// An example of traditional arrays.

fn add_and_zero_into(mut a, mut b[]) -> b {
  while a {
    dec a;
    for x in b {
      inc x;
    }
  }
}

fn add_two_to_each(mut b[]) -> b {
  add_and_zero_into 2 b;
}

fn main() {
  // Note that arrays are declared using spaces as delimeters, not commas.
  add_two_to_each [(mut a = 3) (mut b = 4)];
  // `a` is now 5
  // `b` is now 6
}
```

```rs
// An example of arrays with rest parameters.

fn add_and_zero_into(mut a, ...mut b[]) -> b {
  while a {
    dec a;
    for x in b {
      inc x;
    }
  }
}

fn add_two_to_each(...mut b[]) -> b {
  add_and_zero_into 2 ...b;
}

fn main() {
  add_two_to_each (mut a = 3) (mut b = 4);
  // `a` is now 5
  // `b` is now 6
}
```

```rs
// An example of using a sized array to store structured data. This also takes
// advantage of that names can include `::` for namespace-like effects.

fn inc_and_return(mut next_id, mut my_id) -> my_id {
  mut c;
  while next_id {
    dec next_id;
    inc my_id c;
  }
  while c {
    inc next_id;
  }
  inc next_id;
}

fn User::create(mut next_id) -> [0 (inc_and_return next_id)];

fn User::inc_age(mut user[2]) -> user {
  inc user.0;
}

fn main() {
  mut next_id;
  // these arrays are automatically-sized based on their return values
  mut alice[] = User::create(next_id);
  mut bob[] = User::create(next_id);
  User::inc_age alice;
  User::inc_age alice;
  User::inc_age alice;
  User::inc_age bob;
  // arrays can act as structured data
}
```

(The Rust syntax highlighter is used since RCR and Rust have similar keywords,
but the exact syntax is quite different.)

# Syntax

RCR is made up of four crucial item types: statements, targets, literals, and
functions. A statement is a piece of procedural code. A target points to a value
in memory or an array of values in memory. A literal is a compile-time constant.
A function is a block of code which takes parameters and produces an optional
return target.

Here are all statement types:

```rs
// BUILTIN — Calls a builtin function
//
// `inc`, `dec`, and `read` require their argument(s) to be mutable.
// `write`'s argument may be readonly.
inc a;
dec a;
read a;
write a;

// WHILE LOOP — Executes code while `var` is nonzero
//
// If `var` is an expression, it is computed once and the memory cell it points
// to is checked. To create a true `while` loop, update the condition at the end
// of each loop iteration.
while var { ... }

// FOR LOOP — Executes code for each element in an array.
//
// This is, practically speaking, a compile-time macro, since arrays do not
// exist in brainfuck.
//
// The `mut` keyword can be appended to make the item mutable within the loop.
// If an item is marked `mut` on a readonly array, an error is thrown.
for item in array { ... }
for mut item in array { ... }

// LET BINDING — Creates a local stored in memory
//
// Destructuring is supported. `let` bindings may be assigned integers, arrays
// of integers, or UTF-8 strings. If a single-byte string is assigned to a
// scalar local, the single byte's value will be used. Uninitialized bindings
// default to zero.
//
// The `mut` keyword may replace `let` to create a mutable binding.
mut a;
let [h e l o w r d] = "helowrd";
let hello_world[] = "Hello, world!";
let a[5..] = "this string better be at least 5 bytes long";
let [a b c d _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ e=78 f=49] =
  "not enough bytes maybe";
let space = " ";

// GOTO — Goes to the specified cell in memory
//
// `goto` expects a single local to be passed to it.
goto a;

// ASSERTION — Asserts the value of a local to the compiler
//
// The compiler automatically zeroes all memory cells when a block of code is
// exited to make future assignments easier. It uses simple heuristics to
// determine when this is necessary, and these are correct almost all the time.
// If you ever need advanced control, these functions will do it.
//
// Misusing `assert::is_zero` will result in incorrect code. This means that it
// must always be called with `unsafe`. For a safe variant, use a `while` loop
// with no body, which will hang instead of causing UB if the value is nonzero.
//
// Misusing `assert::is_unknown` will result in correct, but lengthier code.
//
// The compiler's default heuristics:
//
// - The target of a `while` loop is zero when the loop exits.
// - A call to `inc`, `dec`, or `read` marks all targets as unknown.
// - If a local is zero at the end of both branches of a `while` loop, it is
//   zero when the loop exits. Otherwise, it is unknown.
//
// These heuristics will never mark a possible-nonzero value as definitely zero,
// although values which are definitely zero may be marked as possibly nonzero.
unsafe assert::is_zero a; // tells the compiler `a` is definitely zero
assert::is_unknown a; // tells the compiler `a` will need to be zeroed
```

Here are all target types:

```rs
// INTEGER — A plain integer
3
-4

// LOCAL — A variable
a
b
hello_world
space

// ARRAY — An array of targets
[3 -4]
[4 hello_world]
[5 6 7 23 (mut a)]

// STRING — A literal string
"hello world"
"these can be escaped: \\ \n \r \""
"all other escapes fail"

// RELATIVE — A cell relative to the cursor's position
//
// This type of target is highly volatile and should only be used after a goto
// statement.
@       // this cell
<<<     // the cell three cells to the left of this one
>78     // the cell 78 cells to the right of this one

// LET BINDING — The local(s) created by a `let` or `mut` binding
(let a)
(mut [h i ...] = "hi world")
(let result = 23)

// FUNCTION RETURN VALUE — The return value of a function
(zero a)
(mult a (mut b = 23))

// BLOCK — The value of the last statement in a block
//
// The last statement must be a `let` binding or a function call
{ mult a a (mut result); inc result; add 78 result }
{ while a { inc b b b; dec a; }; mult 2 b }

// INDEX — An index into any other target
//
// Arrays are zero-indexed
a.0
[(mut x) (mut y)].1
```

Literals are like targets, but restricted to integers `3` `-4`, strings
`"hello"`, `"with an \"escape\""`, and integer arrays `[3 4 -78]`.

Here are some function declarations:

```rs
// A function declaration is composed of three parts:
//
// 1. The parameters of a function define what values it takes, their types,
//    default values, destructuring patterns, and mutabilities.
// 2. The body of a function executes the main script.
// 3. The return value of a function provides a target for `(...)` targets.
//
// The return value has access to the parameters, but not to locals created in
// the function body.

// empty functions can use a semicolon instead of `{}`
fn do_nothing();

// this takes an immutable argument
fn println(a) {
  write a;
  write "\n";
}

// this takes a mutable argument
fn zero(mut a) {
  while a {
    dec a;
  }
}

// this has a return value
fn add_and_zero(mut a, mut b) -> b {
  while a {
    dec a;
    inc b;
  }
}

// this has an array parameter
fn println_all(a[]) {
  for item in a {
    write item;
  }
  write "\n";
}

// this has a destructured exact-length array parameter
fn zero_pair(mut [a b]) {
  zero a;
  zero b;
}

// this has a destructured inexact-length array parameter
fn zero_pair_and_ignore_more(mut [a b ...]) {
  zero a;
  zero b;
}

// this has several parameters, a return value, and no body
fn many(a, mut b, c = (add_and_zero 7 8), d[5]) -> d;

// this has a rest parameter
fn println_rest(...a[]) {
  for x in a {
    write x;
  }
  write "\n";

  // OR:

  println_all a;
}

fn println_repeat(mut count, ...text[]) {
  while count {
    dec count;
    printls_rest ...text;
  }
}

fn into_is_nonzero(mut a, mut result = 0) -> result {
  // this statement is compiled away if `result` is already zero
  zero result;

  while a {
    zero a;
    inc result;
  }
}

fn clone(a, mut b = 0) -> b {
  zero b;

  mut c;
  while a {
    unsafe dec a;
    inc b c;
  }
  while c {
    dec c;
    unsafe inc a;
  }
}

fn is_nonzero(a, mut result = 0) -> result {
  into_is_nonzero (clone a) result;
}
```
