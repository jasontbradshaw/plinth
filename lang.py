import inspect
import itertools

import errors
import tokens
import util

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
    def build(*items):
        """Build a Cons sequence recursively from a nested list structure."""

        result = NIL

        for item in reversed(items):
            if isinstance(item, (list, tuple)):
                result = Cons(Cons.build(*item), result)
            else:
                result = Cons(item, result)

        return result

    @staticmethod
    def is_list(e):
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

    def __str_helper(self, item, str_fun):
        # nil has no contents
        if item is NIL:
            return ""

        if item.cdr is NIL:
            return str_fun(item.car)

        if not isinstance(item.cdr, Cons):
            return str_fun(item.car) + " . " + str_fun(item.cdr)

        return str_fun(item.car) + " " + self.__str_helper(item.cdr, str_fun)

    def __str__(self, str_fun=str):
        return "(" + self.__str_helper(self, str_fun) + ")"

    def __repr__(self):
        return (self.__class__.__name__ +
               "(" + repr(self.car) + ", " + repr(self.cdr) + ")")

    def __len__(self):
        # nil is the empty list
        if self is NIL:
            return 0

        return sum(1 for i in self)

    def __eq__(self, other):
        """Compare recursively to another iterable."""

        for si, oi in itertools.izip(self, other):
            if si != oi:
                return False

        return True

    def __iter__(self):
        """
        Yield all the items in this cons sequence in turn. If the final item is
        NIL, it isn't yielded. If the final item is non-NIL, raises an error.
        """

        item = self
        while 1:
            if item is NIL:
                # stop if the item is NIL
                return
            elif isinstance(item.cdr, Cons):
                yield item.car
                item = item.cdr
            else:
                raise errors.WrongArgumentTypeError("not a proper list: " +
                        str(self))

# the singleton 'nil' value, an empty Cons: we define it here so Cons can use it
NIL = Cons(None, None)

class Symbol(Atom):
    """Symbols store other values, and evaluate to their stored values."""

    def __init__(self, value):
        """Symbols are stored and looked up by their string names."""

        Atom.__init__(self, str(value))

    def __hash__(self):
        return hash(self.value)

    def __eq__(self, other):
        return isinstance(other, Symbol) and self.value == other.value

class Callable(Atom):
    """A base class for object in our language that can be 'called'."""

    def __init__(self, name=None):
        """A name can optionally be set for display purposes."""
        self.name = name

class Function(Callable):
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

        # unroll the cons
        arg_symbols = [symbol for symbol in arg_symbols]

        # all arguments must be symbols
        for item in arg_symbols:
            if not isinstance(item, Symbol):
                raise errors.WrongArgumentTypeError.build(item, Symbol)

        # separate out the vararg if the final symbol uses the vararg token
        vararg = None
        if len(arg_symbols) > 1 and arg_symbols[-1].value == tokens.VARIADIC_ARG:
            vararg = arg_symbols[-2]
            del arg_symbols[-1]

        self.arg_symbols = arg_symbols
        self.vararg = vararg
        self.body = body
        self.parent = parent

        Callable.__init__(self, name)

    def __str__(self):
        s = "<function"

        if self.name is not None:
            s += " " + self.name

        s += " ("
        s += ' '.join(map(str, self.arg_symbols))

        if self.vararg is not None:
            s += " " + tokens.VARIADIC_ARG

        s += ")>"

        return s

    def __repr__(self):
        s = self.__class__.__name__ + "("
        s += repr(self.arg_symbols) + ", "
        s += repr(self.body) + ", "
        s += repr(self.parent)

        if self.name is not None:
            s += ", name=" + repr(self.name)

        s += ")"

        return s

    def __call__(self, evaluate, *arg_values):
        """
        Evaluate this function given a list of values to use for its arguments
        and return the result.
        """

        num_args = len(self.arg_symbols)
        if self.vararg is not None:
            num_args -= 1
        util.ensure_args(arg_values, num_args, self.vararg is None)

        # create a new environment with the parent set as our parent environment
        env = Environment(self.parent)

        # map the vararg to nil by default, to ensure it is a list
        if self.vararg is not None:
            env[self.vararg] = NIL

        # put the argument values into the new environment, mapping by position
        for i, (symbol, value) in enumerate(
                itertools.izip(self.arg_symbols, arg_values)):
            # see if we're on the last argument and we have a variadic arg
            if self.vararg is not None and i == len(self.arg_symbols) - 1:
                # map it into the environment as the remaining arg values
                env[self.vararg] = Cons.build(*arg_values[i:])

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

        # get our arguments with any variadic arg
        args, vararg, _, _ = inspect.getargspec(method)

        # set the variadic argument (None if there wasn't one, else the arg)
        self.vararg = vararg

        # add the args
        self.arg_names = args

        Callable.__init__(self, name)

    def __str__(self):
        s = "<primitive-function "

        if self.name is not None:
            s += self.name + " "

        s += "("
        s += " ".join(self.arg_names)

        if self.vararg is not None:
            s += " " + self.vararg + " " + tokens.VARIADIC_ARG

        s += ")>"

        return s

    def __repr__(self):
        s = self.__class__.__name__ + "("
        s += repr(self.method)

        if self.name is not None:
            s += ", name=" + repr(self.name)

        s += ")"

        return s

    def __call__(self, evaluate, *arg_values):
        """
        Calls our internal method on the given arguments, ensuring that the
        correct number of values was passed in. The evaluate argument is kept
        only for compatibility with the Function class.
        """

        num_args = len(self.arg_names)
        if self.vararg is not None:
            num_args -= 1
        util.ensure_args(arg_values, num_args, self.vararg is None)

        return self.method(*arg_values)

class Macro(Callable):
    """
    A code-rewriting construct. A macro takes code and returns (expands) code
    dynamically at runtime. Arguments aren't evaluated before being inserted
    into the macro body.
    """

    def __init__(self, arg_symbols, body, name=None):

        # unroll arg symbols cons into a normal list so we can edit it
        unrolled = []
        for item in arg_symbols:
            if not isinstance(item, Symbol):
                raise errors.WrongArgumentTypeError.build(item, Symbol)
            unrolled.append(item)
        arg_symbols = unrolled

        # handle any variadic arg
        vararg = None
        if len(arg_symbols) > 1 and arg_symbols[-1].value == tokens.VARIADIC_ARG:
            vararg = arg_symbols[-2]
            del arg_symbols[-1]

        self.arg_symbols = arg_symbols
        self.vararg = vararg
        self.body = body

        Callable.__init__(self, name)

    def __str__(self):
        s = "<macro"

        if self.name is not None:
            s += " " + self.name

        s += " ("
        s += ' '.join(map(str, self.arg_symbols))

        if self.vararg is not None:
            s += " " + tokens.VARIADIC_ARG

        s += ")>"

        return s

    def __repr__(self):
        s = self.__class__.__name__ + "("
        s += repr(self.arg_symbols) + ", "
        s += repr(self.body)

        if self.name is not None:
            s += ", name=" + repr(self.name)

        s += ")"

        return s

    def __call__(self, evaluate, env, *arg_sexps):
        """
        Expand the macro's body in some environment using the given argument
        expressions.
        """

        num_args = len(self.arg_symbols)
        if self.vararg is not None:
            num_args -= 1
        util.ensure_args(arg_sexps, num_args, self.vararg is None)

        # map symbols to their replacement expressions in a new environment
        expand_env = Environment(env)
        for i, (symbol, value) in enumerate(
                itertools.izip(self.arg_symbols, arg_sexps)):
            # see if we're on the last argument and we have a variadic arg
            if self.vararg is not None and i == len(self.arg_symbols) - 1:
                # map it into the environment as the remaining arg values
                expand_env[self.vararg] = Cons.build(*arg_sexps[i:])

            # add the symbol normally otherwise
            else:
                expand_env[symbol] = value

        # evaluate our body in the created environment
        return evaluate(self.body, expand_env)

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

        # None means no parent, otherwise must be an environment
        assert parent is None or isinstance(parent, Environment)

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
        elif self.parent is not None:
            return self.parent.find(symbol)

        raise errors.SymbolNotFoundError.build(symbol)

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
