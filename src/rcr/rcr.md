## Function Declarations

```
fn hello_world() {
  let a[_] = "hello world";

  each q in a {
    write q;
  }
}

// `->` is not necessary, but specifies a return target for `(...)` expressions
fn add_and_zero(mut source, mut target) -> target {
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
exitable at q {
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
let name             // declares a new local variable
let name[3]          // declares a block of contiguous local variables
let name[_] = STR    // declares a block sized to match a string's length
let name = 78        // initializes a declared value (default is zero)
let name = 'q'       // initializes a declared value (default is zero)
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
```
