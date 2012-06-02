import collections
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
        v = self.value if hasattr(self, "value") else None
        return self.__class__.__name__ + "(" + repr(self.value) + ")"

    def __eq__(self, other):
        return isinstance(other, self.__class__) and self.value == other.value

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
            return TRUE
        elif token.lower() == tokens.FALSE:
            return FALSE

        # string (strips wrapping string tokens)
        elif token.startswith(tokens.STRING) and token.endswith(tokens.STRING):
            return String(token[len(tokens.STRING):-len(tokens.STRING)])

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
    def build_list(items):
        """Same as build, but takes a single list as its arguments instead."""
        return Cons.build(*items)

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

class Boolean(Atom):
    """
    Represents the base boolean type in our language. Evaluates to True and
    False for each type, respectively.
    """

    def __init__(self, value):
        Atom.__init__(self, value)

    def __str__(self):
        """Return the language token for true or false."""
        return tokens.TRUE if self.value else tokens.FALSE

    def __nonzero__(self):
        """Makes boolean expressions work and return instances of this class."""
        return self.value

    @staticmethod
    def build(value):
        return TRUE if value else FALSE

# singleton true and false values in the language
TRUE = Boolean(True)
FALSE = Boolean(False)

class String(Atom):
    """
    Functions exactly the same as a unicode string, but the string representation
    uses our language's tokens.
    """

    def __init__(self, value):
        Atom.__init__(self, value)

    def __unicode__(self):
        v = self.value.replace('"', tokens.ESCAPE_CHAR + '"')
        return tokens.STRING + unicode(v) + tokens.STRING

    def __str__(self):
        return unicode(self)

    def __repr__(self):
        return self.__class__.__name__ + "(" + repr(self.value) + ")"

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
        """A name can later be set for display purposes."""
        Atom.__init__(self, name)

    @staticmethod
    def build_argspec(evaluate, env, args):
        """
        Parses the given args according to the language specification and
        returns an ArgSpec object for them. Raises an error if parsing fails.
        """

        # the ArgSpec we'll return after filling it
        argspec = util.ArgSpec()

        # convert to a list for easy access/modification
        args = list(args)

        # consume the variadic arg first, if there is one
        if len(args) > 1:
            v = args[-1]
            vararg_symbol = args[-2]
            if isinstance(v, Symbol) and v.value == tokens.VARIADIC_ARG:
                # store the vararg symbol, then remove its cruft from the list
                argspec.variadic(vararg_symbol)
                del args[-2:]

        # consume optional args next
        while len(args) > 0 and Cons.is_list(args[-1]):
            # get the next argument in the list
            opt_arg = args.pop()

            # make sure we got a symbol and an expression for the optional arg
            if len(opt_arg) != 2 or not isinstance(opt_arg.car, Symbol):
                raise errors.WrongArgumentTypeError("optional arguments must " +
                        "consist of a single symbol and a single expression")

            # evaluate the expression and store the result in the spec
            symbol = opt_arg.car
            expression = opt_arg.cdr.car
            argspec.optional(symbol, evaluate(expression, env))

        # consume remaining required args
        for symbol in args:
            # optional args are no longer allowed
            if Cons.is_list(symbol):
                raise errors.WrongArgumentTypeError("optional arguments must " +
                        "come after required arguments and before any " +
                        "variadic argument.")
            # deal with arguments that aren't symbols
            elif not isinstance(symbol, Symbol):
                raise errors.WrongArgumentTypeError.build(symbol, Symbol)

            argspec.required(symbol)

        return argspec

    @staticmethod
    def build_string(kind, name, argspec):
        """
        Build a string to display a typical callable in the interpreter. kind is
        the object type to use, name is the (optional) name to give this
        specific instance of the object, and argspec is the ArgSpec object to
        use to build the arguments list.
        """

        s = "<" + str(kind)

        # set the function name if possible
        if name is not None:
            s += " " + str(name)

        s += " ("

        # compose a list of all arg symbols
        a = []
        for arg_type, arg in argspec:
            if arg_type == util.ArgSpec.REQUIRED:
                a.append(str(arg))
            elif arg_type == util.ArgSpec.OPTIONAL:
                arg, default = arg
                a.append("(" + str(arg) + " " + str(default) + ")")
            elif arg_type == util.ArgSpec.VARIADIC:
                a.append(str(arg))
                a.append(tokens.VARIADIC_ARG)
            else:
                raise ValueError("Unhandled arg type: " + arg_type)

        s += " ".join(a)
        s += ")>"

        return s

    def name(self, name):
        # set the name if it hasn't been stored yet
        if self.value is None:
            self.value = name

    def __eq__(self, other):
        """Callables are only equal if the other callable is this callable."""
        return isinstance(other, self.__class__) and other is self

class Function(Callable):
    """
    Represents a function in our language. Functions take some number of
    arguments, have a body, and are evaluated in some context.
    """

    def __init__(self, evaluate, parent, args, body, name=None):
        """
        Creates a function given a list of its arguments, its body, and
        its parent environment, i.e. the environment it was created and will be
        evaluated in (its closure). If the last argument is the variadic
        argument symbol, the preceding symbol is treated as a variadic arg.
        Optional arguments follow required arguments but precede any variadic
        arg, and consist of a two item list of symbol and expression, of which
        the expression is evaluated immediately.
        """

        # NOTE: arguments MUST come in the order: required, optional, variadic

        assert isinstance(parent, Environment)

        self.argspec = Callable.build_argspec(evaluate, parent, args)
        self.body = body
        self.parent = parent

        Callable.__init__(self, name)

    def __str__(self):
        return Callable.build_string("function", self.value, self.argspec)

    def __call__(self, evaluate, *arg_values):
        """
        Evaluate this function given a list of values to use for its arguments
        and return the result.
        """

        # create a new environment with the parent set as our parent environment
        env = Environment(self.parent)

        # fill it with the arg spec's values (throws an error if impossible)
        env.update(self.argspec.fill(arg_values, Cons.build_list))

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

        # parse the arg spec (no support for keyword args)
        args, vararg, _, defaults = inspect.getargspec(method)
        defaults = () if defaults is None else defaults

        self.argspec = util.ArgSpec()

        # get the counts for our respective arg types
        num_required = len(args) - len(defaults)
        num_optional = len(args) - num_required

        # add required arguments
        for arg in args[:num_required]:
            self.argspec.required(arg)

        # add optional arguments, which come after required ones
        for arg, default in itertools.izip(args[num_required:], defaults):
            self.argspec.optional(arg, default)

        # add the variadic arg if it exists
        if vararg is not None:
            self.argspec.variadic(vararg)

        Callable.__init__(self, name)

    def __str__(self):
        return Callable.build_string("primitive-function", self.value,
                self.argspec)

    def __call__(self, evaluate, *arg_values):
        """
        Calls our internal method on the given arguments, ensuring that the
        correct number of values was passed in. The evaluate argument is present
        only for method-parity with the Function class.
        """

        self.argspec.validate(arg_values)
        return self.method(*arg_values)

class Macro(Callable):
    """
    A code-rewriting construct. A macro takes code and returns (expands) code
    dynamically at runtime. Arguments aren't evaluated before being inserted
    into the macro body.
    """

    def __init__(self, evaluate, env, args, body, name=None):

        self.argspec = Callable.build_argspec(evaluate, env, args)
        self.body = body

        Callable.__init__(self, name)

    def __str__(self):
        return Callable.build_string("macro", self.value, self.argspec)

    def __call__(self, evaluate, env, *arg_sexps):
        """
        Expand the macro's body in some environment using the given argument
        expressions.
        """

        # map symbols to their replacement expressions in a new environment
        expand_env = Environment(env)
        expand_env.update(self.argspec.fill(arg_sexps, Cons.build_list))

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

    def update(self, other_dict):
        for key in other_dict:
            self[key] = other_dict[key]

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
