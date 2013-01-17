import inspect
import itertools

import argspec
import errors
import tokens
import util

class Atom:
    '''The base class all custom language constructs inherit from.'''

    def __init__(self, value):
        self.value = value

    def __str__(self):
        return unicode(self.value)

    def __repr__(self):
        v = self.value if hasattr(self, 'value') else None
        return self.__class__.__name__ + '(' + repr(self.value) + ')'

    def __eq__(self, other):
        return isinstance(other, self.__class__) and self.value == other.value

    @staticmethod
    def to_atom(token):
        '''
        Takes the given string token and returns the class representing that
        string in its most natural form (int, str, Symbol, etc.).
        '''

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
            return unicode(token[len(tokens.STRING):-len(tokens.STRING)])

        # the base case for all tokens is a symbol
        return Symbol(token)

class Cons:
    '''Represents a pair of elements.'''

    def __init__(self, car, cdr):
        self.car = car
        self.cdr = cdr

    @staticmethod
    def build(*items):
        '''Build a Cons sequence recursively from a nested list structure.'''

        result = NIL

        for item in reversed(items):
            if isinstance(item, (list, tuple)):
                result = Cons(Cons.build(*item), result)
            else:
                result = Cons(item, result)

        return result

    @staticmethod
    def build_list(items):
        '''Same as build, but takes a single list as its arguments instead.'''
        return Cons.build(*items)

    @staticmethod
    def is_list(e):
        '''Returns whether an element is a list or not (nil is a list).'''

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
            return u''

        if item.cdr is NIL:
            return util.to_string(item.car)

        if not isinstance(item.cdr, Cons):
            return util.to_string(item.car) + u' . ' + util.to_string(item.cdr)

        return util.to_string(item.car) + u' ' + self.__str_helper(item.cdr)

    def __str__(self):
        return u'(' + self.__str_helper(self) + ')'

    def __repr__(self):
        return (self.__class__.__name__ +
               u'(' + repr(self.car) + ', ' + repr(self.cdr) + ')')

    def __len__(self):
        # nil is the empty list
        if self is NIL:
            return 0
        return sum(1 for i in self)

    def __eq__(self, other):
        '''Compare recursively to another iterable.'''
        for si, oi in itertools.izip(self, other):
            if si != oi:
                return False
        return True

    def __iter__(self):
        '''
        Yield all the items in this cons sequence in turn. If the final item is
        NIL, it isn't yielded. If the final item is non-NIL, raises an error.
        '''

        item = self
        while 1:
            if item is NIL:
                # stop if the item is NIL
                return
            elif isinstance(item.cdr, Cons):
                yield item.car
                item = item.cdr
            else:
                raise errors.WrongArgumentTypeError('not a proper list: ' +
                        unicode(self))

    def __getitem__(self, index):
        '''Allow indexing into the list.'''
        index += len(self) if index < 0 else 0
        for i, item in enumerate(self):
            if i == index:
                return item
        raise IndexError(self.__class__.__name__.lower() +
                ' index out of range')

# the singleton 'nil' value, an empty Cons: we define it here so Cons can use it
NIL = Cons(None, None)

class Symbol(Atom):
    '''Symbols store other values, and evaluate to their stored values.'''

    def __init__(self, value):
        '''Symbols are stored and looked up by their string names.'''

        Atom.__init__(self, unicode(value))

    def __hash__(self):
        return hash(self.value)

    def __eq__(self, other):
        return isinstance(other, Symbol) and self.value == other.value

class Callable(Atom):
    '''A base class for object in our language that can be 'called'.'''

    def __init__(self, name=None, docstring=None):
        '''A name can later be set for display purposes.'''
        Atom.__init__(self, name)
        self.docstring = docstring

    @staticmethod
    def build_string(kind, name, spec):
        '''
        Build a string to display a typical callable in the interpreter. kind is
        the object type to use, name is the (optional) name to give this
        specific instance of the object, and spec is the ArgSpec object to
        use to build the arguments list.
        '''

        s = u'<' + unicode(kind)

        # set the function name if possible
        if name is not None:
            s += ' ' + unicode(name)

        s += ' ('

        # compose a list of all arg symbols
        a = []
        for arg_type, arg in spec:
            if arg_type == argspec.ArgSpec.REQUIRED:
                a.append(unicode(arg))
            elif arg_type == argspec.ArgSpec.OPTIONAL:
                arg, default = arg
                a.append('(' + unicode(arg) + ' ' + util.to_string(default) + ')')
            elif arg_type == argspec.ArgSpec.VARIADIC:
                a.append(unicode(arg))
                a.append(tokens.VARIADIC_ARG)
            else:
                raise ValueError('Unhandled arg type: ' + arg_type)

        s += ' '.join(a)
        s += ')>'

        return s

    def name(self, name):
        # set the name if it hasn't been stored yet
        if self.value is None:
            self.value = name

    def __eq__(self, other):
        '''Callables are only equal if the other callable is this callable.'''
        return isinstance(other, self.__class__) and other is self

class Function(Callable):
    '''
    Represents a function in our language. Functions take some number of
    arguments, have a body, and are evaluated in some context.
    '''

    def __init__(self, evaluate, parent, args, body, name=None):
        '''
        Creates a function given a list of its arguments, its body, and
        its parent environment, i.e. the environment it was created and will be
        evaluated in (its closure). If the last argument is the variadic
        argument symbol, the preceding symbol is treated as a variadic arg.
        Optional arguments follow required arguments but precede any variadic
        arg, and consist of a two item list of symbol and expression, of which
        the expression is evaluated immediately.
        '''

        # NOTE: arguments MUST come in the order: required, optional, variadic

        assert isinstance(parent, Environment)

        self.spec = argspec.ArgSpec.build(evaluate, parent, args)
        self.body = body
        self.parent = parent

        Callable.__init__(self, name)

    def __str__(self):
        return Callable.build_string('function', self.value, self.spec)

    def __call__(self, evaluate, *arg_values):
        '''
        Evaluate this function given a list of values to use for its arguments
        and return the result.
        '''

        # create a new environment with the parent set as our parent environment
        env = Environment(self.parent)

        # fill it with the arg spec's values (throws an error if impossible)
        env.update(self.spec.fill(arg_values, Cons.build_list))

        # evaluate our body using the new environment and return the result
        return evaluate(self.body, env)

class PrimitiveFunction(Function):
    '''
    Represents a base-level function that can't be broken down into an AST. One
    of the constructs that enables the language to function.
    '''

    def __init__(self, method, name=None):
        '''
        Create a primitive function that works much like a normal function,
        except that the method is a Python function that does work using the
        arguments given to __call__.
        '''

        self.method = method

        # parse the arg spec (no support for keyword args)
        args, vararg, _, defaults = inspect.getargspec(method)
        defaults = () if defaults is None else defaults

        self.spec = argspec.ArgSpec()

        # get the counts for our respective arg types
        num_required = len(args) - len(defaults)
        num_optional = len(args) - num_required

        # add required arguments
        for arg in args[:num_required]:
            self.spec.required(arg)

        # add optional arguments, which come after required ones
        for arg, default in itertools.izip(args[num_required:], defaults):
            self.spec.optional(arg, default)

        # add the variadic arg if it exists
        if vararg is not None:
            self.spec.variadic(vararg)

        Callable.__init__(self, name)

    def __str__(self):
        return Callable.build_string('primitive-function', self.value,
                self.spec)

    def __call__(self, evaluate, *arg_values):
        '''
        Calls our internal method on the given arguments, ensuring that the
        correct number of values was passed in. The evaluate argument is present
        only for method-parity with the Function class.
        '''

        self.spec.validate(arg_values)
        return self.method(*arg_values)

class Macro(Callable):
    '''
    A code-rewriting construct. A macro takes code and returns (expands) code
    dynamically at runtime. Arguments aren't evaluated before being inserted
    into the macro body.
    '''

    def __init__(self, evaluate, env, args, body, name=None):

        self.spec = argspec.ArgSpec.build(evaluate, env, args)
        self.body = body

        Callable.__init__(self, name)

    def __str__(self):
        return Callable.build_string('macro', self.value, self.spec)

    def __call__(self, evaluate, env, *arg_sexps):
        '''
        Expand the macro's body in some environment using the given argument
        expressions.
        '''

        # map symbols to their replacement expressions in a new environment
        expand_env = Environment(env)
        expand_env.update(self.spec.fill(arg_sexps, Cons.build_list))

        # evaluate our body in the created environment
        return evaluate(self.body, expand_env)

class Environment:
    '''
    A scope that holds variables mapped to their values. Allows us to easily
    package execution state.
    '''

    def __init__(self, parent):
        '''
        Create an environment with the given parent and any number of predefined
        variables.
        '''

        # None means no parent, otherwise must be an environment
        assert parent is None or isinstance(parent, Environment)

        # the environment that contains this environment
        self.parent = parent

        # where we keep our symbol name to value mappings
        self.items = {}

    def find(self, symbol):
        '''
        Attempts to locate a given symbol by name in the current environment,
        then in every environment up the parent chain if the symbol could not be
        found. If the symbol is not bound within the parent chain, raises an
        error.
        '''

        # make sure we're getting a symbol
        assert isinstance(symbol, Symbol)

        if symbol in self:
            return self[symbol]
        elif self.parent is not None:
            return self.parent.find(symbol)

        raise errors.SymbolNotFoundError.build(symbol)

    def put(self, symbol, value):
        '''Shortcut for setting symbols to values.'''
        return self.__setitem__(symbol, value)

    def update(self, other_dict):
        for key in other_dict:
            self[key] = other_dict[key]

    def __str__(self):
        return unicode(self.items)

    def __repr__(self):
        return (self.__class__.__name__ + u'(' +
                repr(self.parent) + ', ' + repr(self.items) + ')')

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
