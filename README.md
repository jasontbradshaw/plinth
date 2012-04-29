plinth
====

plinth is a simple purely-functional Lisp-like language with an interpreter
written in Python.

plinth features lexical closure, first-class functions, quoting, as well as all
the standard math operations one could want.

Usage
----

To run plinth, you'll need Python installed. Simply run `python plinth.py`
from the root directory, and you'll be in business.

Reference
----
plinth supports a number of built-in functions.

 * `define` gives symbols values in the current scope. It takes as its arguments
   a symbol name and a value, and assigns the symbol to the given value.

 * `list` is the basic data-concatenation function in plinth. Unlike most Lisps,
   plinth lacks a concept of `cons` and works instead with lists entire. `list`
   takes as its arguments any number (including zero) of symbols, and returns
   them as a list.

 * `quote` (equivalently `'`) returns its only argument as a literal value.

 * `if` takes as its arguments an expression to evaluate for truthiness, an
   expression to execute if the first expression didn't return `#f`, and an
   expression to execute if the first expression returned `#f`. All non-`#f`
   (false) values are considered `#t` (true).

 * `and`, `or`, and `not` evaluate their arguments using standard logical rules
   and return at the soonest possible moment. This means that, for example, if
   `and` is called without `#f` as its first argument, it short-circuits and
   returns its second argument without evaluating it. Hence, `(and #t 3)`
   returns `3`. The same logic applies to all logical operators.

 * `lambda` returns a function. It takes as its parameters a list of symbols to
   use as arguments, and an expression to execute over the given argument
   symbols.  Functions are closed over the current scope, and carry their parent
   scope with them for their lifetime. Functions can receive variadic arguments
   by using the special `...` postfix notation on symbol names. For example, if
   a function's argument is `things...`, the function will have access to the
   symbol `things` as a list of all the symbols from that position onwards.

 * `read` takes a string and returns a list of expressions as symbol lists.

 * `eval` takes a list of expressions and evaluates them in the current scope.

 * `apply` takes a function and a list of arguments, and calls the function with
   those arguments.

 * `boolean?`, `list?`, `symbol?`, `string?`, `number?`, `integer?`, `float?`,
   and `function?` all take a single argument, and return `#t` if the argument
   is of the requisite type, and `#f` otherwise.

 * `nth` takes an integer and a list, and returns the n-th item in the list.

 * `slice` takes a start index, an end index, and a list, and returns a new list
   consisting of the items in the original list from start inclusive to end
   exclusive.

 * `length` takes a list and returns the number of items in it.

 * `insert` takes an index, an item, and a list, and returns a new list with the
   item inserted at the given index.
