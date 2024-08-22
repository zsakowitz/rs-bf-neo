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

Here are some statement types:

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

## Reserved Words

- whitespace
- `//`
- `->`
- `;`
- `{`
- `}`
- `inc`
- `dec`
- `read`
- `write`
- `goto`
- `fn`
- `mut`
- `each`
- `let`
- `while`
- `exitable`
- `exit`
- `=`
- `(`
- `)`
- `[`
- `]`
- `_`
- `,`
- `.`
- `%`
- integers
- floats
- characters
- strings

## Function Declarations

```
fn hello_world () {
  let a[_] = "hello world";

  each q in a {
    write q;
  }
}

// `->` is not necessary, but specifies a return target for `(...)` expressions
fn add_and_zero (mut source, mut target) -> target {
  while source {
    dec source;
    inc target;
  }
}

fn add_into(source, mut target) -> target {
  let temp;

  while source {
    unsafe dec source;
    inc target;
    inc temp;
  }

  while temp {
    dec temp;
    unsafe inc source;
  }
}

// `result = 0` means that result doesn't need to be passed, and will default
// to a local with the value `0` in the enclosing scope
fn mult(mut zeroed, kept, mut result = 0) -> result {
  while zeroed {
    dec zeroed;
    add_into kept result;
  }
}

// `mut block results` accepts a block as an argument
fn move_into(mut source, mut block results) -> results {
  while source {
    dec source;
    each result in results {
      inc result;
    }
  }
}

fn main() {
  // example using all %-syntax: (2 * 8) + (7 * (4 + 3)) - (2 * 20)
    sub (add (mult 2 8 %) %=(mult 7 (add 4 %=3) %)) x=(mult 2 20 %);
    write x;

  // example using all retval-syntax: (2 * 8) + (7 * (4 + 3)) - (2 * 20)
    sub (add (mult 2 8) (mult 7 (add 4 3))) x=(mult 2 20);
    write x;

  // this is "just" syntax sugar for
    let inter_a;
    mult 2 8 inter_a;

    let inter_b = 3;
    add 4 inter_b;

    let inter_c;
    mult 7 inter_b inter_c;

    let inter_d = inter_c;
    add inter_a inter_d;

    let inter_e;
    mult 2 20 inter_e;

    let x = inter_e;
    sub inter_d inter_e;

    write x;
}
```

## High-Level Statements

```
inc target;    // increments given target
dec target;    // decrements given target
read target;   // reads from input to target
write target;  // writes from target to output

// executes code while `target` is nonzero
while target {
  ...
}

// executes code for each cell in a block target
each name in target {
  ...
}

// creates a block which can be exited from at any point
// (it keeps track of the current index and makes a zero cell)
// (then goes to zero on `exit` so that the index position is still reliable)
exitable {
  exit;
}

// like plain `exitable`, but uses a pre-existing cell as the zero cell
exitable q {
  exit;
}

// calls a function with some arguments
fn_name t1 t2 t3;

// calls a function and provides an output for it
fn_name a b let c;

// calls a function and provides some static inputs to it
fn_name 23 q let c;
```

## Declarations

```
let name            // declares a new local variable
let name[3]         // declares a block of contiguous local variables
let name[] = STR    // declares a block sized to match a string's length
let name = 78       // initializes a declared value (default is zero)
let name = 'q'      // initializes a declared value (default is zero)
```

## Low-Level Statements

```
goto target;           // moves to the given target
unsafe inc target;     // treats the target as mutable, then increments it
unsafe dec target;     // treats the target as mutable, then decrements it
unsafe read target;    // treats the target as mutable, then reads from input
```

## Targets

```
name       // a given local variable
name.2     // a given local variable within a block (0-indexed)
[a b c]    // a block of the given variables
```

## Expression Targets

Expression targets run code in the same line where they are used, allowing for
expression-driven programming over pure procedural programming.

```
// creates a local `%` set to zero, runs a set of statements, then targets `%`
// if `%` is used at least once in the expression, it is the ultimate target
(mult 2 3 %)

// otherwise, the target depends on the type of expression:

// - basic mutable functions and `while` target their original targets
(inc a)
(while a { ... })

// - `let` statements target the last bound name
(let a = 23)

// - `each` statements target the variable block
(each a in b { ... })

// - function calls target the function's returned target, if it has one
(fn_name a b c)

// creates a local `%` set to 78, runs a set of statements, then targets `%`
(mult 2 3 %=78)

// an empty expression throws an error
() // <-- ERROR
```
