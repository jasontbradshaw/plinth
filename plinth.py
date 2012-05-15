#!/usr/bin/env python

import math
import inspect
import itertools
import os

import tokens
import errors

# shortcuts for all the various true number types and list types
NUMBER_TYPES = (int, long, float, complex)
LIST_TYPES = (list, tuple)

#
# language constructs
#

class Atom:
    """
    Represents anything that's not a list: numbers, strings, symbols, etc.
    """

    def __init__(self, value):
        self.value = value

    def __str__(self):
        return str(self.value)

    def __repr__(self):
        return self.__class__.__name__ + "(" + repr(self.value) + ")"

    @staticmethod
    def to_atom(token):
        """
        Takes the given string token and returns the class representing that
        string in its most natural form (int, str, Symbol, etc.).
        """

        # number types: complex are invalid floats, and floats are invalid ints
        try:
            # automatically returns 'long' if necessary
            return int(token)
        except:
            try:
                return float(token)
            except:
                try:
                    return complex(token)
                except:
                    pass

        # boolean
        if token.lower() == tokens.TRUE:
            return True
        elif token.lower() == tokens.FALSE:
            return False

        # string (strips wrapping string tokens)
        elif token.startswith(tokens.STRING) and token.endswith(tokens.STRING):
            return token[len(tokens.STRING):-len(tokens.STRING)]

        # the base case for all tokens is a symbol
        return Symbol(token)

class Cons:
    """Represents a pair of elements."""

    def __init__(self, car, cdr):
        self.car = car
        self.cdr = cdr

    @staticmethod
    def build_list(*items):
        """Build a Cons sequence recursively from some number of items."""

        result = NIL

        for item in reversed(items):
            if isinstance(item, LIST_TYPES):
                result = Cons(Cons.build_list(*item), result)
            else:
                result = Cons(item, result)

        return result

    def __str_helper(self, item):
        # nil has no contents
        if item is NIL:
            return ""

        if item.cdr is NIL:
            return str(item.car)

        if not isinstance(item.cdr, Cons):
            return str(item.car) + " . " + str(item.cdr)

        return str(item.car) + " " + self.__str_helper(item.cdr)

    def __str__(self):
        return "(" + self.__str_helper(self) + ")"

    def __repr__(self):
        return (self.__class__.__name__ +
               "(" + repr(self.car) + ", " + repr(self.cdr) + ")")

    def __len__(self):
        if self is NIL:
            # the nil val is always zero-length
            return 0
        elif isinstance(self.cdr, self.__class__):
            return 1 + len(self.cdr)
        else:
            # not a valid list if cdr isn't of the same class
            raise errors.WrongArgumentTypeError.build(self.cdr, self.__class__)

    def __eq__(self, other):
        """Compare recursively."""

        for si, oi in itertools.izip(self, other):
            if si != oi:
                return False

        return True

    def __iter__(self):
        """
        Yield all the items in the cons sequence in turn. If the final item is
        nil, None is returned instead.
        """

        item = self

        # yield the first item of a simple pair if necessary
        if not isinstance(item.cdr, Cons):
            yield item.car

        while 1:
            if isinstance(item.cdr, Cons):
                yield item.car
                item = item.cdr
            else:
                # yield the final value and give up
                yield item.cdr
                break

# the singleton 'nil' value, an empty Cons: we define it here so Cons can use it
NIL = Cons(None, None)

class Symbol(Atom):
    """
    Symbols store other values, and evaluate to their stored values.
    """

    def __init__(self, value):
        """
        Symbols are stored and looked up by their string names.
        """

        Atom.__init__(self, str(value))

    def __hash__(self):
        return hash(self.value)

    def __eq__(self, other):
        return isinstance(other, Symbol) and self.value == other.value

class Function(Atom):
    """
    Represents a function in our language. Functions take some number of
    arguments, have a body, and are evaluated in some context.
    """

    def __init__(self, arg_symbols, body, parent, name=None):
        """
        Creates a function given a list of its argument symbols, its body, and
        its parent environment, i.e. the environment it was created and will be
        evaluated in (its closure). If the last argument is marked as variadic,
        it gets stored away to allow for different processing when the function
        is called with arguments.
        """

        assert isinstance(parent, Environment)

        # all arguments must be symbols
        for item in arg_symbols:
            if not isinstance(item, Symbol):
                raise errors.WrongArgumentTypeError.build(item, Symbol)

        self.arg_symbols = arg_symbols
        self.body = body
        self.parent = parent

        # can be set by 'define' to name the function
        self.name = name

        # create a normalized argument for a final vararg if there is one
        self.vararg = NIL
        if (len(self.arg_symbols) > 0 and
                self.arg_symbols[-1].value.endswith(tokens.VARARG)):
            self.vararg = Symbol(self.arg_symbols[-1].value[:-len(tokens.VARARG)])

    def __str__(self):
        return ("<function" +
                (" " + self.name if self.name is not None else "") +
                " (" + ' '.join(map(str, self.arg_symbols)) + ")>")

    def __repr__(self):
        return (self.__class__.__name__ + "(" +
                repr(self.arg_symbols) + ", " +
                repr(self.body) + ", " +
                repr(self.parent) +
                (", name=" + repr(self.name) if self.name is not None else "") +
                ")")

    def __call__(self, *arg_values):
        """
        Evaluate this function given a list of values to use for its arguments
        and return the result.
        """

        # ensure that we've got the correct number of argument values
        if self.vararg is not NIL:
            # we only check for the minimum number when variable
            if len(arg_values) < len(self.arg_symbols) - 1:
                raise errors.IncorrectArgumentCountError.build(
                        len(self.arg_symbols) - 1, len(arg_values))
        else:
            # we ensure direct correspondence when not variable
            if len(arg_values) != len(self.arg_symbols):
                raise errors.IncorrectArgumentCountError.build(
                        len(self.arg_symbols), len(arg_values))

        # create a new environment with the parent set as our parent environment
        env = Environment(self.parent)

        # map the vararg to nil by default, to ensure it has a value
        if self.vararg is not NIL:
            env[self.vararg] = NIL

        # put the argument values into the new environment, mapping by position
        for i, (symbol, value) in enumerate(
                itertools.izip(self.arg_symbols, arg_values)):
            # see if we're on the last argument and we have a variadic arg
            if self.vararg is not NIL and i == len(self.arg_symbols) - 1:
                # map it into the environment with the remaining arg values
                env[self.vararg] = Cons.build_list(*arg_values[i:])

            # add the symbol normally otherwise
            else:
                env[symbol] = value

        # evaluate our body using the new environment and return the result
        return evaluate(self.body, env)

class PrimitiveFunction(Function):
    """
    Represents a base-level function that can't be broken down into an AST. One
    of the constructs that enables the language to function.
    """

    def __init__(self, method, name=None):
        """
        Create a primitive function that works much like a normal function,
        except that the method is a Python function that does work using the
        arguments given to __call__.
        """

        self.method = method
        self.name = name

        # get our arguments with any variadic arg
        args, vararg, _, _ = inspect.getargspec(method)

        # set the variadic argument (None if there wasn't one, else the arg)
        self.vararg = vararg

        # add the args and the only variadic arg (if there is one)
        self.arg_names = args
        if vararg is not None:
            self.arg_names.append(vararg + tokens.VARARG)

    def __str__(self):
        return ("<primitive-function" +
                (" " + self.name if self.name is not None else "") +
                " (" + ' '.join(self.arg_names) + ")>")

    def __repr__(self):
        return (self.__class__.__name__ + "(" + repr(self.method) +
                (", name=" + self.name if self.name is not None else "") + ")")

    def __call__(self, *arg_values):
        """
        Calls our internal method on the given arguments, ensuring that the
        correct number of values was passed in.
        """

        # ensure that we've got the correct number of argument values
        if self.vararg is not None:
            # we only check for the minimum number when variable
            if len(arg_values) < len(self.arg_names) - 1:
                raise errors.IncorrectArgumentCountError.build(
                        len(self.arg_names) - 1, len(arg_values))
        else:
            # we ensure direct correspondence when not variable
            if len(arg_values) != len(self.arg_names):
                raise errors.IncorrectArgumentCountError.build(
                        self.arg_names, len(arg_values))

        return self.method(*arg_values)

class Environment:
    """
    A scope that holds variables mapped to their values. Allows us to easily
    package execution state.
    """

    def __init__(self, parent):
        """
        Create an environment with the given parent and any number of predefined
        variables.
        """

        # the environment that contains this environment
        self.parent = parent

        # where we keep our symbol name to value mappings
        self.items = {}

    def find(self, symbol):
        """
        Attempts to locate a given symbol by name in the current environment,
        then in every environment up the parent chain if the symbol could not be
        found. If the symbol is not bound within the parent chain, raises an
        error.
        """

        # make sure we're getting a symbol
        assert isinstance(symbol, Symbol)

        if symbol in self:
            return self[symbol]
        elif isinstance(self.parent, Environment):
            return self.parent.find(symbol)
        else:
            raise errors.SymbolNotFoundError.build(symbol)

    def update(self, other_dict):
        """
        Copy all the values in another dict into the environment.
        """

        for key in other_dict:
            self[key] = other_dict[key]

    def put(self, symbol, value):
        """Shortcut for setting symbols to values."""
        return self.__setitem__(symbol, value)

    def __str__(self):
        return str(self.items)

    def __repr__(self):
        return (self.__class__.__name__ + "(" +
                repr(self.parent) + ", " + repr(self.items) + ")")

    def __getitem__(self, symbol):
        assert isinstance(symbol, Symbol)
        return self.items[symbol]

    def __setitem__(self, symbol, value):
        assert isinstance(symbol, Symbol)
        self.items[symbol] = value

    def __contains__(self, symbol):
        assert isinstance(symbol, Symbol)
        return symbol in self.items

    def __iter__(self):
        return self.items.__iter__()


#
# interpreter
#

def parse(token_source):
    """
    Given a token source, parses the token sequence into an abstract syntax
    tree built from the base elements of the language.
    """

    # where the abstract syntax tree is held
    ast = []

    # stack where the active scope is kept. starts with the ast as the initial
    # active scope where tokens are added.
    stack = [ast]

    def add_token(token):
        """Adds a token to the active scope on the stack."""

        # add the token to the top-most (active) scope of the stack
        stack[-1].append(Atom.to_atom(token))

    def indent():
        """Adds an indent level to the ast when an indent marker is found."""

        # add a new level to last indent scope and push same list onto stack
        new_scope = []
        stack[-1].append(new_scope)
        stack.append(new_scope)

    def dedent():
        """Reduces the indent level, changing the scope that receives tokens."""

        # remove current level of indentation from the stack
        stack.pop()

        if len(stack) < 1:
            raise errors.OpenParenError.build()

    # we keep a buffer of string parts so we can concatenate all the parts of
    # the string together at once, and so we can check whether we're in a string
    # and whether tokens are escaped.
    string_buf = []
    is_escaped = False

    # we store the locations and indexes where we added sugary tokens so we can
    # quickly post-process them when done parsing.
    sugar_locations = []

    # iterate over every character in the source string
    for token in token_source:

        # deal with strings first to avoid triggering other language constructs.
        # we know we're in a string if something has been added to the string
        # buffer.
        if len(string_buf) > 0:

            # every token in a string gets added literally
            string_buf.append(token)

            # treat escaped characters as literal. this does nothing so that on
            # the next iteration of the loop, whatever follows the escape char
            # will be appended literally. we make sure we're not currently
            # escaped so we can escape the escape character itself.
            if token == tokens.ESCAPE_CHAR and not is_escaped:
                is_escaped = True

            # if the token preceding this token is an escape char, this token
            # gets appended to the string literally and we switch off escaping.
            elif is_escaped:
                is_escaped = False

            # end the string and flush if we found an unescaped string token
            elif token == tokens.STRING:
                # add the entire string as one token and clear the string buffer
                add_token(''.join(string_buf))

                # clear the string buffer in-place
                del string_buf[:]

        # skip whitespace and comments
        elif token[0] in tokens.WHITESPACE or token.startswith(tokens.COMMENT):
            pass

        # open parenthesis indents
        elif token == tokens.OPEN_PAREN:
            indent()

        # close parenthesis dedents
        elif token == tokens.CLOSE_PAREN:
            dedent()

        # quote, unquote, quasiquote (the only sugar in our language)
        elif token in tokens.SUGAR:
            # we mark the stack and position of the token for quick reference
            sugar_locations.append((stack[-1], len(stack[-1])))
            add_token(token)

        # mark strings
        elif token == tokens.STRING:
            # mark us as being in a string, let the first case deal with rest
            string_buf.append(token)

        # just a normal token
        else:
            add_token(token)

    # ensure all strings were correctly closed
    if len(string_buf) > 0:
        raise errors.ParserError("unclosed string")

    # check to see if we matched all closing parenthesis (first item is always
    # tokens list, and it never gets popped).
    if len(stack) > 1:
        raise errors.CloseParenError.build()

    # process all the quote marks into quote functions. we process right-to-left
    # to allow for occurences of "''foo" and the like.
    for scope, i in reversed(sugar_locations):
        # quotes must have something to consume
        if i == len(scope) - 1:
            raise errors.ParserError("invalid quote syntax")

        # have the sugar mark consume the item to its right and replace the
        # slots the two once filled with a new scope containing the desugared
        # function and its argument.
        new_symbol = Symbol(tokens.SUGAR[scope[i].value])
        new_item = scope[i + 1]

        scope[i] = [new_symbol, new_item]
        del scope[i + 1]

    # return the canonical abstract syntax tree
    return ast

def ensure_type(required_class, item, *rest):
    """
    Raises a WrongArgumentTypeError if all the items aren't instances of the
    required class/classes tuple.
    """

    if not isinstance(item, required_class):
        raise errors.WrongArgumentTypeError.build(item, required_class)

    for thing in rest:
        if not isinstance(thing, required_class):
            raise errors.WrongArgumentTypeError.build(thing, required_class)

def add(a, b, *rest):
    """Adds the all the given numbers together."""

    ensure_type(NUMBER_TYPES, a, b)

    # add all the arguments together while checking type
    total = a + b
    for n in rest:
        ensure_type(NUMBER_TYPES, n)
        total += n

    return total

def sub(a, b, *rest):
    """Subtracts the given numbers in sequence."""

    ensure_type(NUMBER_TYPES, a, b)

    # subtract all the arguments in sequence while checking type
    difference = a - b
    for n in rest:
        ensure_type(NUMBER_TYPES, n)
        difference -= n

    return difference

def mul(a, b, *rest):
    """Multiplies all the given numbers together."""

    ensure_type(NUMBER_TYPES, a, b)

    # multiply all the arguments together while checking type
    product = a * b
    for n in rest:
        # stop multiplying if the product ever goes to zero
        if product == 0:
            break

        ensure_type(NUMBER_TYPES, n)
        product *= n

    return product

def div(a, b, *rest):
    """Divides the given numbers in sequence."""

    ensure_type(NUMBER_TYPES, a, b)

    # divide all the arguments in sequence while checking type
    quotient = a / b
    for n in rest:
        # TODO: should this optimization be done? complex? zeros?
        # stop dividing if the quotient hits zero
        if quotient == 0:
            break

        ensure_type(NUMBER_TYPES, n)
        quotient /= n

    return quotient

def power(a, b):
    """Raises a to the power of b."""
    ensure_type(NUMBER_TYPES, a, b)
    return a ** b

def sin(a):
    """Takes the sine of a."""
    ensure_type(NUMBER_TYPES, a)
    return math.sin(a)

def cos(a):
    """Takes the cosine of a."""
    ensure_type(NUMBER_TYPES, a)
    return math.cos(a)

def tan(a):
    """Takes the tangent of a."""
    ensure_type(NUMBER_TYPES, a)
    return math.tan(a)

def atan(a):
    """Takes the arctangent of a."""
    ensure_type(NUMBER_TYPES, a)
    return math.atan(a)

def atan2(a):
    """Takes the second arctangent of a."""
    ensure_type(NUMBER_TYPES, a)
    return math.atan2(a)

def booleanp(e):
    """Returns whether an element is a boolean or not."""
    return isinstance(e, bool)

def listp(e):
    """Returns whether an element is a list or not (nil is a list)."""

    # nil is a list
    if e is NIL:
        return True

    # non-cons can't be lists
    if not isinstance(e, Cons):
        return False

    # only cons that len() works on are lists (throws an exception otherwise)
    try:
        return bool(len(e)) or True
    except errors.WrongArgumentTypeError:
        return False

def consp(e):
    """Returns whether an element is a cons or not (nil is NOT a cons)."""
    return e is not NIL and isinstance(e, Cons)

def symbolp(e):
    """Returns whether an element is a symbol or not."""
    return isinstance(e, Symbol)

def stringp(e):
    """Returns whether an element is a string or not."""
    return isinstance(e, basestring)

def numberp(e):
    """Returns whether an element is a number or not."""
    return isinstance(e, NUMBER_TYPES)

def integerp(e):
    """Returns whether an element is an integer or not."""
    return isinstance(e, (int, long))

def floatp(e):
    """Returns whether an element is a float or not."""
    return isinstance(e, float)

def complexp(e):
    """Returns whether an element is a complex number or not."""
    return isinstance(e, complex)

def functionp(e):
    """Returns whether an element is a function or not."""
    return isinstance(e, Function)

def is_(a, b):
    """Returns true if the two items refer to the same object in memory."""
    return a is b

def equal(a, b):
    """
    Returns true if two constructs are congruent. For example, numbers are
    compared mathematically, cons are compared by structure and equivalent
    contents, etc.
    """

    # the same item is equal to itself
    if a is b:
        return True

    # things can't be equal if they're not the same class
    elif not (isinstance(a, b.__class__) and isinstance(b, a.__class__)):
        return False

    # we know both args are of the same class now, no need to check both

    # different functions can never be equal
    elif isinstance(a, Function):
        return False

    # compare everything else by value (numbers, Cons, symbols, etc.)
    return a == b

def gt(a, b):
    """Compare two numbers using '>'."""
    ensure_type(NUMBER_TYPES, a, b)
    return a > b

def gte(a, b):
    """Compare two numbers using '>='."""
    ensure_type(NUMBER_TYPES, a, b)
    return a >= b

def lt(a, b):
    """Compare two numbers using '<'."""
    ensure_type(NUMBER_TYPES, a, b)
    return a < b

def lte(a, b):
    """Compare two numbers using '<='."""
    ensure_type(NUMBER_TYPES, a, b)
    return a <= b

def not_(a):
    """
    Returns the opposite boolean of that passed in. All things that aren't #f
    are #t, so we return whether a is False.
    """

    return a is False

def cons(a, b):
    """Pair two items."""
    return Cons(a, b)

def car(e):
    """Return the first element of a pair."""
    ensure_type(Cons, e)

    # nil isn't allowed to be indexed into, since it has no car or cdr
    if e is NIL:
        raise errors.WrongArgumentTypeError("wrong argument type for car: " +
                "expected pair, got " + str(e))

    return e.car

def cdr(e):
    """Return the second element of a pair."""
    ensure_type(Cons, e)

    if e is NIL:
        raise errors.WrongArgumentTypeError("wrong argument type for cdr: " +
                "expected pair, got " + str(e))

    return e.cdr

def read(s):
    """Read a string and returns a list of the S-expressions it describes."""
    ensure_type(basestring, s)
    return Cons.build_list(*parse(tokens.tokenize(s)))

def load(fname):
    """Read a file and evaluate it into the global scope."""

    ensure_type(basestring, fname)
    fname = fname

    def file_char_iter(f):
        """Iterate over a file one character at a time."""
        for line in f:
            for c in line:
                yield c

    # evaluate every expression in the file in sequence, top to bottom
    with open(os.path.abspath(fname), "r") as f:
        for result in parse(tokens.tokenize(file_char_iter(f))):
            evaluate(result, global_env)

    # return that we were successful
    return True

# these functions serve as markers for whether the function being called is
# special. we check to see if the function for the symbol is one of these
# functions, and if so we evaluate it in whatever way it requires. this allows
# the user to define new symbols that point to these functions, but still have
# the functions work in the same way.
quote = PrimitiveFunction(lambda e: None, name=tokens.QUOTE_LONG)
unquote = PrimitiveFunction(lambda e: None, name=tokens.UNQUOTE_LONG)
quasiquote = PrimitiveFunction(lambda e: None, name=tokens.QUASIQUOTE_LONG)
lambda_ = PrimitiveFunction(lambda args, body: None, name=tokens.LAMBDA)
define = PrimitiveFunction(lambda symbol, value: None, name=tokens.DEFINE)
cond = PrimitiveFunction(lambda *e: None, name=tokens.COND)
and_ = PrimitiveFunction(lambda a, b, *rest: None, name=tokens.AND)
or_ = PrimitiveFunction(lambda a, b, *rest: None, name=tokens.OR)
eval_ = PrimitiveFunction(lambda sexp: None, name=tokens.EVAL)

# the base environment for the interpreter
global_env = Environment(None)

# functions that need special treatment during evaluation
global_env[Symbol(tokens.QUOTE_LONG)] = quote
global_env[Symbol(tokens.LAMBDA)] = lambda_
global_env[Symbol(tokens.DEFINE)] = define
global_env[Symbol(tokens.COND)] = cond
global_env[Symbol(tokens.AND)] = and_
global_env[Symbol(tokens.OR)] = or_
global_env[Symbol(tokens.EVAL)] = eval_

# adds a new primitive function to the gloval environment
add_prim = lambda t, f: global_env.put(Symbol(t), PrimitiveFunction(f, name=t))

# repl
add_prim(tokens.READ, read)
add_prim(tokens.LOAD, load)

# logical
add_prim(tokens.NOT, not_)

# math
add_prim(tokens.ADD, add)
add_prim(tokens.SUBTRACT, sub)
add_prim(tokens.MULTIPLY, mul)
add_prim(tokens.DIVIDE, div)
add_prim(tokens.POWER, power)
add_prim(tokens.SIN, sin)
add_prim(tokens.COS, cos)
add_prim(tokens.TAN, tan)
add_prim(tokens.ARCTAN, atan)
add_prim(tokens.ARCTAN2, atan2)

# comparison
add_prim(tokens.IS, is_)
add_prim(tokens.EQUAL, equal)
add_prim(tokens.GREATER_THAN, gt)
add_prim(tokens.GREATER_THAN_EQUAL, gte)
add_prim(tokens.LESS_THAN, lt)
add_prim(tokens.LESS_THAN_EQUAL, lte)

# types
add_prim(tokens.BOOLEANP, booleanp)
add_prim(tokens.CONSP, consp)
add_prim(tokens.LISTP, listp)
add_prim(tokens.SYMBOLP, symbolp)
add_prim(tokens.STRINGP, stringp)
add_prim(tokens.NUMBERP, numberp)
add_prim(tokens.INTEGERP, integerp)
add_prim(tokens.FLOATP, floatp)
add_prim(tokens.COMPLEXP, complexp)
add_prim(tokens.FUNCTIONP, functionp)

# cons
add_prim(tokens.CONS, cons)
add_prim(tokens.CAR, car)
add_prim(tokens.CDR, cdr)

def ensure_args(arg_list, count, exact=True):
    """
    Ensures that an argument list contains a number of arguments. When exact is
    True (the default), ensures that the count is exactly that provided. When
    exact is False, ensures that the count is at least the number provided.
    """

    if exact:
        if len(arg_list) != count:
            raise errors.IncorrectArgumentCountError.build(count, len(arg_list))
    else:
        if len(arg_list) < count:
            raise errors.IncorrectArgumentCountError.build(count, len(arg_list))

def evaluate(item, env):
    """
    Given an Atom or list, evaluates it using the given environment
    (global by default) and returns the result as represented in our language
    constructs.
    """

    # symbol
    if isinstance(item, Symbol):
        # look it up in the environment for its value
        return env.find(item)

    # atom (not a literal list)
    elif not isinstance(item, LIST_TYPES):
        # it's a generic atom and evaluates to itself
        return item

    # list
    else:
        # we can't evaluate functions that have nothing in them
        if len(item) == 0:
            raise errors.ApplicationError("nothing to apply")

        # evaluate functions using their arguments
        function = evaluate(item[0], env)
        args = item[1:]

        # make sure our first item evaluated to a function
        if not isinstance(function, Function):
            raise errors.ApplicationError("wrong type to apply: " + str(function))

        # quote
        if function is quote:
            ensure_args(args, 1)

            # return the argument unevaluated, as a cons if a list
            if isinstance(args[0], LIST_TYPES):
                return Cons.build_list(*args[0])

            return args[0]

        # function
        elif function is lambda_:
            ensure_args(args, 2)

            arg_symbols = args[0]
            body = args[1]

            # return a function with the current environment as the parent
            return Function(arg_symbols, body, env)

        # define
        elif function is define:
            ensure_args(args, 2)

            symbol = args[0]
            value = args[1]

            # make sure we're defining to a symbol
            if not isinstance(symbol, Symbol):
                raise errors.WrongArgumentTypeError.build(symbol, Symbol)

            # evaluate the argument, map the symbol to the result in the current
            # environment, then return the evaluated value. this allows for
            # chains of definitions, or simultaneous variable assignments to the
            # same value.
            result = evaluate(value, env)
            env[symbol] = result

            # set a function name if one isn't set yet
            if isinstance(result, Function) and result.name is None:
                result.name = symbol.value

            return result

        # cond
        elif function is cond:
            for tup in args:
                # if e is not a list, len() raises an error for us
                if len(tup) != 2:
                    # make sure each is a list of exactly two expressions
                    raise errors.IncorrectArgumentCountError.build(
                            "2 expressions", len(tup))

                # first and second list items are condition and result
                condition = tup[0]
                result = tup[1]

                # evaluate and return the result if condition is True
                if evaluate(condition, env):
                    return evaluate(result, env)

            # if no result is returned, result is undefined
            raise errors.ApplicationError("at least one condition must " +
                    "evaluate to " + prettify(True))

        # logical and
        elif function is and_:
            ensure_args(args, 2, False)

            # evaluate the arguments, returning the final one if none were #f,
            # otherwise the last evaluated item, #f.
            last_item = None
            for item in args:
                last_item = evaluate(item, env)
                if last_item is False:
                    break

            return last_item

        # logical or
        elif function is or_:
            ensure_args(args, 2, False)

            # evaluate the arguments, returning the first one that's not #f,
            last_item = None
            for item in args:
                last_item = evaluate(item, env)
                if not last_item is False:
                    break

            return last_item

        # eval
        elif function is eval_:
            # TODO: broken, fix it
            ensure_args(args, 1)

            # evaluate the given s-expression and return it
            return evaluate(args[0], env)

        else:
            # evaluate the arguments normally before passing them to the
            # function and receiving the result.
            return function(*[evaluate(arg, env) for arg in args])

        # we should never get this far
        assert False

def prettify(item):
    """Convert certain types into special strings, and all others normally."""

    if isinstance(item, bool):
        return tokens.TRUE if item else tokens.FALSE

    if isinstance(item, basestring):
        return tokens.STRING + item + tokens.STRING

    return str(item)

if __name__ == "__main__":
    import sys
    import traceback

    source = ""

    standard_prompt = "> "
    continue_prompt = ": "
    prompt = standard_prompt

    print "plinth 0.1"
    print "-----------"

    # load all provided files into the global environment on interpreter start
    for fname in sys.argv[1:]:
        if load(fname):
            print "loaded '" + os.path.abspath(fname) + "'"

    while 1:
        try:
            # get input from user and try to tokenize, parse, and print it
            source += raw_input(prompt)

            # evaluate every entered expression sequentially
            for result in parse(tokens.tokenize(source)):
                print prettify(evaluate(result, global_env))

            # reset the source and prompt on a successful evaluation
            source = ""
            prompt = standard_prompt

        except errors.ParserError:
            # allow the user to finish entering a correct expression
            prompt = continue_prompt
            source += os.linesep

        except KeyboardInterrupt:
            # reset input on Ctrl+C
            prompt = standard_prompt
            source = ""
            print
        except EOFError:
            # exit on Ctrl+D
            print
            sys.exit()
        except Exception, e:
            # print all other problems and clear source
            traceback.print_exc()

            # reset the source and prompt for the next parse
            source = ""
            prompt = standard_prompt
