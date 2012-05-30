plinth
====

plinth is a simple purely-functional Lisp-like language with an interpreter
written in Python.

plinth features lexical closure, first-class functions, quoting, macros, as well
as all the standard math operations one could want.

Usage
----

To run plinth, you'll need a modern version of Python installed (developed on
2.7.x). Simply run `python plinth.py` from the root directory, and you'll be in
business. You can load the in- progress standard library by passing its filename
(`stdlib.plinth`) into the interpreter as an argument.

Reference
----
plinth supports lots of built-in functionality.

 * `define` gives symbols values in the current scope. It takes as its arguments
   a symbol name and a value, and assigns the symbol to the given value.

 * `cons` is the basic data-concatenation function in plinth. `cons` takes as
   its arguments two symbols, and returns them as a pair. The empty cons, `()`,
   is known as 'nil' and is used to mark the end of lists.

 * `quote` (equivalently `'`) returns its only argument as a literal value.

 * `quasiquote` (equivalently `` `) functions the same as `quote` while allowing
   selective evaluation of its quoted contents via `unquote` (equivalently `,`),
   and list-replacement via `unquote-splicing` (equivalently `^`).

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
   by using the special `...` postfix symbol after symbol names. For example, if
   a function's arguments are `things ...`, the function will have access to the
   symbol `things` as a list of all the symbols from that position onwards.

 * `macro` returns a macro. It takes as its parameters a list of symbols and a
   body to evaluate with the symbols replaced with the unevaluated parameters to
   the macro call. Used to extend the language by constructing syntax that is
   subsequently evaluated. Macros are always expanded using the values of the
   environment they're expanded in.

 * `expand` takes a macro and some arguments to be passed to it, and returns the
   expanded form of the macro using the given arguments.

 * `read` takes a string and returns a list of expressions parsed from it.

 * `eval` takes a list of expressions and evaluates them in the current scope.

 * `boolean?`, `list?`, `cons?`, `symbol?`, `string?`, `number?`, `integer?`,
   `float?`, `complex?`, and `function?` all take a single argument, and return
   `#t` if the argument is of the requisite type, and `#f` otherwise.
