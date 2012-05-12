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

 * `cons` is the basic data-concatenation function in plinth. `cons` takes as
   its arguments two symbols, and returns them as a pair. The empty cons, `()`,
   is known as 'nil', and is used to mark the end of lists.

 * `quote` (equivalently `'`) returns its only argument as a literal value.

 * `cond` takes as its arguments a list of lists containing expressions. For
   every list, if the first element evaluates to `#t`, then the second element
   is returned. If the first element evaluates to `#f`, then the next list of
   expressions is tried in sequence until one that returns `#t` is found. If no
   expressions evaluate to `#t`, an error is raised and the result is undefined.

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

 * `read` takes a string and returns a list of expressions parsed from it.

 * `eval` takes a list of expressions and evaluates them in the current scope.

 * `boolean?`, `list?`, `cons?`, `symbol?`, `string?`, `number?`, `integer?`,
   `float?`, `complex?`, and `function?` all take a single argument, and return
   `#t` if the argument is of the requisite type, and `#f` otherwise.
