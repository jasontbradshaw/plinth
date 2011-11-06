#!/usr/bin/env python

import math
import inspect

import tokens
import errors

#
# language constructs
#

class Atom(object):
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
    def atomize(token):
        """
        Takes the given string token and returns an Atom representing that
        string in its most natural form (Boolean, String, Integer, etc.).
        """

        # integer
        try:
            return Integer(token)
        except ValueError:
            pass

        # float
        try:
            return Float(token)
        except ValueError:
            pass

        # boolean
        if token.lower() == tokens.TRUE:
            return BOOLEAN_TRUE
        elif token.lower() == tokens.FALSE:
            return BOOLEAN_FALSE

        # string
        elif token[0] == tokens.STRING and token[-1] == token[0]:
            return String(token)

        # the base case for all tokens is a symbol
        return Symbol(token)

class List:
    """
    Represents a linear collection of Atom and List elements.
    """

    def __init__(self, *items):
        # save the given items
        self.items = list(items)

    def __str__(self):
        return "(" + " ".join(map(str, self.items)) + ")"

    def __repr__(self):
        return (self.__class__.__name__ +
                "(" + ", ".join(map(repr, self.items)) + ")")

    def __len__(self):
        return len(self.items)

    def __getitem__(self, index):
        return self.items[index]

    def __setitem__(self, index, value):
        self.items[index] = value

    def __getslice__(self, start=0, end=None):
        # set end to our length if not specified
        if end is None:
            end = len(self)

        # returns a List, not a list!
        return List(*self.items[start:end])

    def __iter__(self):
        return self.items.__iter__()

    def __reversed__(self):
        return List(*reversed(self.items))

    def append(self, item):
        self.items.append(item)

    def extend(self, other_items):
        self.items.extend(other_items)

    def pop(self, index=None):
        if index is None:
            index = len(self) - 1

        self.items.pop(index)

    def insert(self, index, item):
        self.items.insert(index, item)

class Number(Atom):
    """
    Numbers can be added, subtracted, etc. and hold a single value.
    """

    @staticmethod
    def to_number(item):
        """
        Transforms the given numeric primitive into one of our language's Number
        objects.
        """

        if hasattr(item, "is_integer"):
            return Float(item)
        return Integer(item)

class Integer(Number):
    """
    Integers represent numbers with no decimal part.
    """

    def __init__(self, value):
        Number.__init__(self, int(value))

class Float(Number):
    """
    Floats represent floating-point numbers.
    """

    def __init__(self, value):
        Number.__init__(self, float(value))

    def __str__(self):
        # we use repr to get the entire float value, unrounded
        return repr(self.value)

class String(Atom):
    """
    Strings are immutable collections of character data. There's no such thing
    as a 'character' in our language, only single-element strings.
    """

    def __init__(self, value):
        # take the raw string value and convert the escape sequences into Python
        # literal representations.
        s = str(value)

        # strip surrounding quotes if necessary
        if s[0] == tokens.STRING and s[-1] == s[0]:
            s = s[1:-1]

        # replace escape sequences with literal values
        s = s.replace("\\\\", "\\")
        s = s.replace("\\\"", "\"")
        s = s.replace("\\a", "\a")
        s = s.replace("\\b", "\b")
        s = s.replace("\\f", "\f")
        s = s.replace("\\n", "\n")
        s = s.replace("\\r", "\r")
        s = s.replace("\\t", "\t")
        s = s.replace("\\v", "\v")

        # set the value to the non-delimted, un-escaped string value
        Atom.__init__(self, s)

    def __str__(self):
        return tokens.STRING + repr(self.value)[1:-1] + tokens.STRING

    def __repr__(self):
        # return the literal string given to us, not the internal representation
        # since that has been un-escaoed.
        return self.__class__.__name__ + "(" + str(self) + ")"

class Symbol(Atom):
    """
    Symbols store other values, and evaluate to their stored values.
    """

    def __init__(self, value):
        """
        Symbols are stored and looked up by their string names.
        """

        Atom.__init__(self, str(value))

class Boolean(Atom):
    """
    Represents a single Boolean value.
    """

    def __init__(self, value):
        Atom.__init__(self, bool(value))

    def __str__(self):
        return tokens.TRUE if self.value else tokens.FALSE

    @staticmethod
    def to_boolean(item):
        """
        Maps the given boolean primitive to BOOLEAN_TRUE or BOOLEAN_FALSE.
        """

        return BOOLEAN_TRUE if item else BOOLEAN_FALSE

# singletons for true/false
BOOLEAN_TRUE = Boolean(True)
BOOLEAN_FALSE = Boolean(False)

class Function(Atom):
    """
    Represents a function in our language. Functions take some number of
    arguments, have a body, and are evaluated in some context.
    """

    def __init__(self, arg_symbols, body, parent):
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
                raise errors.WrongArgumentTypeError(item, Symbol)

        self.arg_symbols = arg_symbols
        self.body = body
        self.parent = parent

        # create a normalized argument for a final vararg if there is one
        self.vararg = None
        if (len(self.arg_symbols) > 0 and
                self.arg_symbols[-1].value.endswith(tokens.VARARG)):
            self.vararg = Symbol(self.arg_symbols[-1].value[:-len(tokens.VARARG)])

    def __str__(self):
        return "<function (" + ' '.join(map(str, self.arg_symbols)) + ")>"

    def __repr__(self):
        return (self.__class__.__name__ + "(" +
                repr(self.arg_symbols) + ", " +
                repr(self.body) + ", " +
                repr(self.parent) + ")")

    def __call__(self, *arg_values):
        """
        Evaluate this function given a list of values to use for its arguments
        and return the result.
        """

        # ensure that we've got the correct number of argument values
        if self.vararg is not None:
            # we only check for the minimum number when variable
            if len(arg_values) < len(self.arg_symbols) - 1:
                raise errors.IncorrectArgumentCountError(
                        len(self.arg_symbols) - 1, len(arg_values))
        else:
            # we ensure direct correspondence when not variable
            if len(arg_values) != len(self.arg_symbols):
                raise errors.IncorrectArgumentCountError(
                        len(self.arg_symbols), len(arg_values))

        # create a new environment with the parent set as our parent environment
        env = Environment(self.parent)

        # map the vararg to an empty list by default, to ensure it has a value
        if self.vararg is not None:
            env[self.vararg] = List()

        # put the argument values into the new environment, mapping by position
        for i, (symbol, value) in enumerate(zip(self.arg_symbols, arg_values)):
            # see if we're on the last argument and we have a variadic arg
            if self.vararg is not None and i == len(self.arg_symbols) - 1:
                # map it into the environment with the remaining arg values
                env[self.vararg] = List(*arg_values[i:])

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

    def __init__(self, method):
        """
        Create a primitive function that works much like a normal function,
        except that the method is a Python function that does work using the
        arguments given to __call__.
        """

        self.method = method

        # get our arguments with any variadic arg
        args, vararg, _, _ = inspect.getargspec(method)

        # set the variadic argument (None if there wasn't one, else the arg)
        self.vararg = vararg

        # add the args and the only variadic arg (if there is one)
        self.arg_names = args
        if vararg is not None:
            self.arg_names.append(vararg + tokens.VARARG)

    def __str__(self):
        return "<primitive-function (" + ' '.join(self.arg_names) + ")>"

    def __repr__(self):
        return (self.__class__.__name__ + "(" + repr(self.method) + ", " +
                ", ".join(map(repr, self.arg_names)) + ")")

    def __call__(self, *arg_values):
        """
        Calls our internal method on the given arguments, ensuring that the
        correct number of values was passed in.
        """

        # ensure that we've got the correct number of argument values
        if self.vararg is not None:
            # we only check for the minimum number when variable
            if len(arg_values) < len(self.arg_names) - 1:
                raise errors.IncorrectArgumentCountError(
                        len(self.arg_names) - 1, len(arg_values))
        else:
            # we ensure direct correspondence when not variable
            if len(arg_values) != len(self.arg_names):
                raise errors.IncorrectArgumentCountError(
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
            raise errors.SymbolNotFoundError(symbol)

    def update(self, other_dict):
        """
        Copy all the values in another dict into the environment.
        """

        for key in other_dict:
            self[key] = other_dict[key]

    def __str__(self):
        return str(self.items)

    def __repr__(self):
        return (self.__class__.__name__ + "(" +
                repr(self.parent) + ", " + repr(self.items) + ")")

    def __getitem__(self, symbol):
        assert isinstance(symbol, Symbol)

        return self.items[symbol.value]

    def __setitem__(self, symbol, value):
        assert isinstance(symbol, Symbol)
        assert isinstance(value, Atom) or isinstance(value, List)

        self.items[symbol.value] = value

    def __contains__(self, symbol):
        assert isinstance(symbol, Symbol)

        return symbol.value in self.items

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
    ast = List()

    # stack where the active scope is kept. starts with the ast as the initial
    # active scope where tokens are added.
    stack = [ast]

    def add_token(token):
        """Adds a token to the active scope on the stack."""

        # add the token to the top-most (active) scope of the stack
        stack[-1].append(Atom.atomize(token))

    def indent():
        """Adds an indent level to the ast when an indent marker is found."""

        # add a new level to last indent scope and push same list onto stack
        new_scope = List()
        stack[-1].append(new_scope)
        stack.append(new_scope)

    def dedent():
        """Reduces the indent level, changing the scope that receives tokens."""

        # remove current level of indentation from the stack
        stack.pop()

        if len(stack) < 1:
            raise errors.OpenParenError()

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
            if tokens.is_escape_char(token) and not is_escaped:
                is_escaped = True

            # if the token preceding this token is an escape char, this token
            # gets appended to the string literally and we switch off escaping.
            elif is_escaped:
                is_escaped = False

            # end the string and flush if we found an unescaped string token
            # that matched the initial string token kind. this allows us to
            # possibly define several different string delimiting tokens.
            elif token == string_buf[0]:
                # add the entire string as one token and clear the string buffer
                add_token(''.join(string_buf))

                # clear the string buffer in-place
                del string_buf[:]

        # skip whitespace
        elif tokens.is_whitespace(token):
            pass

        # open parenthesis indents
        elif tokens.is_open_paren(token):
            indent()

        # close parenthesis dedents
        elif tokens.is_close_paren(token):
            dedent()

        # quote, unquote, quasiquote (the only sugar in our language)
        elif token in tokens.DESUGAR:
            # we mark the stack and position of the token for quick reference
            sugar_locations.append((stack[-1], len(stack[-1])))
            add_token(token)

        # mark strings
        elif tokens.is_string(token):
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
        raise errors.CloseParenError()

    # process all the quote marks into quote functions. we process right-to-left
    # to allow for occurences of "''foo" and the like.
    for scope, i in reversed(sugar_locations):
        # quotes must have something to consume
        if i == len(scope) - 1:
            raise errors.ParserError("invalid quote syntax")

        # have the sugar mark consume the item to its right and replace the
        # slots the two once filled with a new scope containing the desugared
        # function and its argument.
        new_symbol = Symbol(tokens.DESUGAR[scope[i].value])
        scope[i:i + 2] = [List(new_symbol, scope[i + 1])]

    # return the canonical abstract syntax tree
    return ast

def ensure_type(required_class, item, *rest):
    """
    Raises a WrongArgumentTypeError if all the items aren't instances of the
    required class.
    """

    if not isinstance(item, required_class):
        raise errors.WrongArgumentTypeError(item, required_class)

    for thing in rest:
        if not isinstance(thing, required_class):
            raise errors.WrongArgumentTypeError(thing, required_class)

def add(a, b, *rest):
    """Adds the all the given numbers together."""

    ensure_type(Number, a, b)

    # add all the arguments together while checking type
    total = a.value + b.value
    for n in rest:
        ensure_type(Number, n)
        total += n.value

    return Number.to_number(total)

def sub(a, b, *rest):
    """Subtracts the given numbers in sequence."""

    ensure_type(Number, a, b)

    # subtract all the arguments in sequence while checking type
    difference = a.value - b.value
    for n in rest:
        ensure_type(Number, n)
        difference -= n.value

    return Number.to_number(difference)

def mul(a, b, *rest):
    """Multiplies all the given numbers together."""

    ensure_type(Number, a, b)

    # multiply all the arguments together while checking type
    product = a.value * b.value
    for n in rest:
        # stop multiplying if the product ever goes to zero
        if product == 0:
            break

        ensure_type(Number, n)
        product *= n.value

    return Number.to_number(product)

def div(a, b, *rest):
    """Divides the given numbers in sequence."""

    ensure_type(Number, a, b)

    # divide all the arguments in sequence while checking type
    quotient = a.value / b.value
    for n in rest:
        # stop dividing if the quotient hits zero
        if quotient == 0:
            break

        ensure_type(Number, n)
        quotient /= n.value

    return Number.to_number(quotient)

def power(a, b):
    """Raises a to the power of b."""

    ensure_type(Number, a, b)

    return Number.to_number(a.value ** b.value)

def sin(a):
    """Takes the sine of a."""

    ensure_type(Number, a)

    return Number.to_number(math.sin(a.value))

def cos(a):
    """Takes the cosine of a."""

    ensure_type(Number, a)

    return Number.to_number(math.cos(a.value))

def tan(a):
    """Takes the tangent of a."""

    ensure_type(Number, a)

    return Number.to_number(math.tan(a.value))

def atan(a):
    """Takes the arctangent of a."""

    ensure_type(Number, a)

    return Number.to_number(math.atan(a.value))

def atan2(a):
    """Takes the second arctangent of a."""

    ensure_type(Number, a)

    return Number.to_number(math.atan2(a.value))

def booleanp(e):
    """Returns whether an element is a boolean or not."""

    return Boolean.to_boolean(isinstance(e, Boolean))

def listp(e):
    """Returns whether an element is a list or not."""

    return Boolean.to_boolean(isinstance(e, List))

def symbolp(e):
    """Returns whether an element is a symbol or not."""

    return Boolean.to_boolean(isinstance(e, Symbol))

def stringp(e):
    """Returns whether an element is a string or not."""

    return Boolean.to_boolean(isinstance(e, String))

def numberp(e):
    """Returns whether an element is a number or not."""

    return Boolean.to_boolean(isinstance(e, Number))

def integerp(e):
    """Returns whether an element is an integer or not."""

    return Boolean.to_boolean(isinstance(e, Integer))

def floatp(e):
    """Returns whether an element is a float or not."""

    return Boolean.to_boolean(isinstance(e, Float))

def functionp(e):
    """Returns whether an element is a function or not."""

    return Boolean.to_boolean(isinstance(e, Function))

def nth(i, lst):
    """
    Returns the nth element of a list, or raises an error if no such index
    exists or the element isn't a list.
    """

    ensure_type(List, lst)
    ensure_type(Integer, i)

    # throws a nice index error by itself, we don't need to wrap it
    return lst[i.value]

def slice_(start, end, lst):
    """
    Returns a new list containing the elements from start (inclusive) to end
    (exclusive) in the given list.
    """

    ensure_type(List, lst)
    ensure_type(Integer, start)
    ensure_type(Integer, end)

    return lst[start.value:end.value]

def length(lst):
    """
    Returns the length of a list.
    """

    ensure_type(List, lst)

    return Integer(len(lst))

def insert(i, item, lst):
    """
    Insert an item before the given position in the given list and return a new
    list with the item inserted at the specified position.
    """

    ensure_type(Integer, i)
    ensure_type(List, lst)

    new_list = lst[:]
    new_list.insert(i.value, item)
    return new_list

def is_(a, b):
    """
    Returns true if the two items refer to the same object in memory.
    """

    return Boolean.to_boolean(a is b)

def equal(a, b):
    """
    Returns true if two constructs are congruent. For example, numbers are
    compared mathematically, lists are compared by structure and equivalent
    contents, etc.
    """

    # the same item is equal to itself
    if a is b:
        return BOOLEAN_TRUE

    # numbers are compared mathematically, regardless of type
    elif isinstance(a, Number) and isinstance(b, Number):
        return Boolean.to_boolean(a.value == b.value)

    # things can't be equal if they're not the same class
    elif not (isinstance(a, b.__class__) and isinstance(b, a.__class__)):
        return BOOLEAN_FALSE

    # we know both args are of the same class now, no need to check both

    # compare lists recursively
    elif isinstance(a, List):
        # must be of the same length
        if len(a) != len(b):
            return BOOLEAN_FALSE

        # compare all items in the list
        for a_item, b_item in zip(a, b):
            if not equal(a_item, b_item).value:
                return BOOLEAN_FALSE

        # if we made it to here, we were equal!
        return BOOLEAN_TRUE

    # different functions can never be equal, there are too many things to check
    elif isinstance(a, Function):
        return BOOLEAN_FALSE

    # compare everything else by value (booleans, symbols, etc.)
    return Boolean.to_boolean(a.value == b.value)

def gt(a, b):
    """
    Compares two numbers using >.
    """

    ensure_type(Number, a, b)

    return Boolean.to_boolean(a.value > b.value)

def gte(a, b):
    """
    Compares two numbers using >=.
    """

    ensure_type(Number, a, b)

    return Boolean.to_boolean(a.value >= b.value)

def lt(a, b):
    """
    Compares two numbers using <.
    """

    ensure_type(Number, a, b)

    return Boolean.to_boolean(a.value < b.value)

def lte(a, b):
    """
    Compares two numbers using <=.
    """

    ensure_type(Number, a, b)

    return Boolean.to_boolean(a.value <= b.value)

def apply_(f, args):
    """
    Applies a function to some arguments and returns the result.
    """

    ensure_type(Function, f)
    ensure_type(List, args)

    return f(*args)

def not_(a):
    """
    Returns the opposite boolean of that passed in. All things that aren't #f
    are #t, so we return the opposite of that.
    """

    return Boolean.to_boolean(isinstance(a, Boolean) and not a.value)

def read(s):
    """
    Reads a string and returns a list of the S-expressions contained therein.
    """

    ensure_type(String, s)

    return parse(s.value)

def eval_(sexp):
    """
    Evaluates an S-expression in the global environment and returns the result.
    """

    return evaluate(sexp, global_env)

# these functions serve as markers for whether the function being called is
# special. we check to see if the function for the symbol is one of these
# functions, and if so we evaluate it in whatever way it requires. this allows
# the user to define new symbols that point to these functions, but still have
# the functions work in the same way.
quote = PrimitiveFunction(lambda e: None)
unquote = PrimitiveFunction(lambda e: None)
quasiquote = PrimitiveFunction(lambda e: None)
lambda_ = PrimitiveFunction(lambda args, body: None)
define = PrimitiveFunction(lambda symbol, value: None)
if_ = PrimitiveFunction(lambda cond, success, failure: None)
and_ = PrimitiveFunction(lambda a, b, *rest: None)
or_ = PrimitiveFunction(lambda a, b, *rest: None)
list_ = PrimitiveFunction(lambda *items: None)

# the base environment for the interpreter
global_env = Environment(None)

# functions that need special treatment during evaluation
global_env[Symbol(tokens.QUOTE_LONG)] = quote
global_env[Symbol(tokens.LAMBDA)] = lambda_
global_env[Symbol(tokens.DEFINE)] = define
global_env[Symbol(tokens.IF)] = if_
global_env[Symbol(tokens.AND)] = and_
global_env[Symbol(tokens.OR)] = or_
global_env[Symbol(tokens.LIST)] = list_

# adds a new primitive function to the gloval environment
add_prim = lambda t, f: global_env.__setitem__(Symbol(t), PrimitiveFunction(f))

# repl
add_prim(tokens.READ, read)
add_prim(tokens.EVAL, eval_)

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

# functional programming
add_prim(tokens.APPLY, apply_)

# comparison
add_prim(tokens.IS, is_)
add_prim(tokens.EQUAL, equal)
add_prim(tokens.GREATER_THAN, gt)
add_prim(tokens.GREATER_THAN_OR_EQUAL, gte)
add_prim(tokens.LESS_THAN, lt)
add_prim(tokens.LESS_THAN_OR_EQUAL, lte)

# types
add_prim(tokens.BOOLEANP, booleanp)
add_prim(tokens.LISTP, listp)
add_prim(tokens.SYMBOLP, symbolp)
add_prim(tokens.STRINGP, stringp)
add_prim(tokens.NUMBERP, numberp)
add_prim(tokens.INTEGERP, integerp)
add_prim(tokens.FLOATP, floatp)
add_prim(tokens.FUNCTIONP, functionp)

# list
add_prim(tokens.NTH, nth)
add_prim(tokens.SLICE, slice_)
add_prim(tokens.LENGTH, length)
add_prim(tokens.INSERT, insert)

def ensure_args(arg_list, count, exact=True):
    """
    Ensures that an argument list contains a number of arguments. When exact is
    True (the default), ensures that the count is exactly that provided. When
    exact is False, ensures that the count is at least the number provided.
    """

    if exact:
        if len(arg_list) != count:
            raise errors.IncorrectArgumentCountError(count, len(arg_list))
    else:
        if len(arg_list) < count:
            raise errors.IncorrectArgumentCountError(count, len(arg_list))


def evaluate(item, env):
    """
    Given an Atom or List element, evaluates it using the given environment
    (global by default) and returns the result as represented in our language
    constructs.
    """

    # symbol
    if isinstance(item, Symbol):
        # look it up in the environment for its value
        return env.find(item)

    # atom
    elif not isinstance(item, List):
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

            # return the argument unevaluated
            return args[0]

        # list
        elif function is list_:
            result = List()
            for item in args:
                result.append(evaluate(item, env))
            return result

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
                raise errors.WrongArgumentTypeError(symbol, Symbol)

            # evaluate the argument, map the symbol to the result in the current
            # environment, then return the evaluated value. this allows for
            # chains of definitions, or simultaneous variable assignments to the
            # same value.
            result = evaluate(value, env)
            env[symbol] = result
            return result

        # if
        elif function is if_:
            ensure_args(args, 3)

            cond = args[0]
            success_clause = args[1]
            failure_clause = args[2]

            # every value is considered #t except for #f
            if evaluate(cond, env) is BOOLEAN_FALSE:
                return evaluate(failure_clause, env)
            return evaluate(success_clause, env)

        # logical and
        elif function is and_:
            ensure_args(args, 2, False)

            # evaluate the arguments, returning the final one if none were #f,
            # otherwise the last evaluated item, #f.
            last_item = None
            for item in args:
                last_item = evaluate(item, env)
                if last_item is BOOLEAN_FALSE:
                    break

            return last_item

        # logical or
        elif function is or_:
            ensure_args(args, 2, False)

            # evaluate the arguments, returning the first one that's not #f,
            last_item = None
            for item in args:
                last_item = evaluate(item, env)
                if not last_item is BOOLEAN_FALSE:
                    break

            return last_item

        else:
            # evaluate the arguments normally before passing them to the
            # function and receiving the result.
            return function(*map(lambda x: evaluate(x, env), args))

        # we should never get this far
        assert False

if __name__ == "__main__":
    import sys
    import os
    import traceback

    source = ""

    standard_prompt = "> "
    continue_prompt = ": "
    prompt = standard_prompt

    print "plinth 0.1"
    print "-----------"

    while 1:
        try:
            # get input from user and try to tokenize, parse, and print it
            source += raw_input(prompt)

            # strip comments from the source (it's as if they don't exist)
            source = source.split(tokens.COMMENT, 1)[0].strip()

            # evaluate every entered expression sequentially
            for result in parse(tokens.tokenize(source)):
                print evaluate(result, global_env)

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
