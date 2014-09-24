from __future__ import unicode_literals

import collections
import inspect
import itertools
import re

import tokens
import util

class Object(object):
    '''The base class all custom language constructs inherit from.'''

    # the subclasses that Object's #parse method will attempt to parse from a
    # token in order of preference. order matters! see #parse for more details.
    PARSEABLE_SUBCLASSES = (
        Integer,
        Float,
        Boolean,
        String,
        Keyword,
        Symbol,
    )

    def __init__(self, value=None, can_modify_meta=True):
        self.value = value

        # an internal dict that can contain arbitrary metadata
        self.meta = {}

        # whether the metadata for this object is modifiable. disabled on
        # singleton objects like nil and the booleans.
        self.can_modify_meta = can_modify_meta

    def __getitem__(self, key):
        return self.meta[key]

    def __setitem__(self, key, value):
        # prevent 'set' access to unmodifiable metadata
        if not self.can_modify_meta:
            raise ValueError('metadata map locked!')
        self.meta[key] = value

    def __str__(self):
        '''Default to returning our value's unicode string.'''
        return unicode(self.value)

    def __unicode__(self):
        return str(self)

    def __repr__(self):
        v = self.value if hasattr(self, 'value') else None
        return self.__class__.__name__ + '(' + repr(self.value) + ')'

    def __eq__(self, other):
        '''Equal if we're the same type and our values are equal.'''
        return isinstance(other, self.__class__) and self.value == other.value

    def __hash__(self):
        '''Delegate to our value by default.'''
        return hash(self.value)

    @staticmethod
    def parse(token):
        '''
        Returns a new object based on the token value passed in. If the given
        token was invalid, None should be returned. Subclasses should override
        this method with their own specific implementation that returns an
        instance of their particular subclass.

        This particular instance takes the given token string and returns an
        instance of the Object subclass that best represents that token in its
        most natural form (Integer, String, Symbol, etc.).
        '''

        # the first instance of the subclasses that returns a non-None response
        # will be used, so order matters!
        for Subclass in Object.PARSEABLE_SUBCLASSES:
            result = Subclass.parse(token)
            if result is not None:
                return result

        return None

class Number(Object):
    '''A number in our language.'''

    def __init__(self, value):
        Object.__init__(self, value)

    def __eq__(self, other):
        '''
        Numbers can be equal to each other even if they're different types, as
        long as their values are equivalent.
        '''
        return isinstance(other, Number) and self.value == other.value

    # delegate the numeric magic methods to the values and cast the return value
    # to the appropriate type, given the classes being operated on.
    def __add__(self, other):
        return Number.choose(self, other)(self.value + other.value)
    def __sub__(self, other):
        return Number.choose(self, other)(self.value - other.value)
    def __mul__(self, other):
        return Number.choose(self, other)(self.value * other.value)
    def __div__(self, other):
        return Number.choose(self, other)(self.value / other.value)
    def __floordiv__(self, other):
        return Number.choose(self, other)(self.value // other.value)
    def __mod__(self, other):
        return Number.choose(self, other)(self.value % other.value)
    def __pow__(self, other):
        return Number.choose(self, other)(self.value ** other.value)
    def __lshift__(self, other):
        return Number.choose(self, other)(self.value << other.value)
    def __rshift__(self, other):
        return Number.choose(self, other)(self.value >> other.value)
    def __and__(self, other):
        return Number.choose(self, other)(self.value & other.value)
    def __or__(self, other):
        return Number.choose(self, other)(self.value | other.value)
    def __xor__(self, other):
        return Number.choose(self, other)(self.value ^ other.value)

    @staticmethod
    def choose(self, *subclasses):
        '''Given some Number subclasses as args, choose the 'winning' class.'''

        # since we currently have only two number types, we have to choose only
        # between Float and Integer. Floats pollute whatever they're mixed with,
        # so they always win if present.
        for subclass in subclasses:
            if isinstance(subclass, Float):
                return Float

        return Integer

    @staticmethod
    def parse(token):
        '''
        Attempt to return a specific number for the given string. Returns None
        if doing so is impossible.
        '''

        x = Integer.parse(token)
        if x is None:
            x = Float.parse(token)

        return x

class Integer(Number):
    '''A fixed-precision arbitrary-length number.'''

    REGEX = re.compile('^-?([0-9]+|0[xX][a-fA-F0-9]+|0[bB][01]+|0[oO][0-7]+)$')

    # characters mapped to base values. this works since we use the '0_' system
    # to represent numbers in different bases, where the second character always
    # uniquely identifies the base.
    BASES = {
        'b': 2, 'B': 2,
        'o': 8, 'O': 8,
        'x': 16, 'X': 16,
    }

    def __init__(self, value):
        Number.__init__(self, value)

    def __hash__(self):
        return self.value

    @staticmethod
    def parse(token):
        if Integer.REGEX.match(token):
            # get the base of the number (default 10)
            base = 10
            if len(token) > 2 and token[1] in Integer.BASES:
                base = Integer.BASES[token[1]]

            # parse the number using our base value
            return Integer(int(token, base))

        return None

class Float(Number):
    '''A rational number expressed to a fixed number of decimal places.'''

    REGEX = re.compile('^-?[0-9]+\.[0-9]+$')

    def __init__(self, value):
        Number.__init__(self, value)

    @staticmethod
    def parse(token):
        if Float.REGEX.match(token):
            return Float(float(token))
        return None

class Boolean(Object):
    def __init__(self, value):
        # disable metadata modification since this is a singleton class
        Object.__init__(self, value, can_modify_meta=False)

    def __eq__(self, other):
        # singletons can only equal themselves!
        return self is other

    def __nonzero__(self):
        '''Return our value directly since it's already a boolean.'''
        return self.value

    @staticmethod
    def parse(token):
        '''Return the singleton instance for the given token.'''
        if token == tokens.TRUE:
            return TRUE
        return FALSE

class String(Object):
    '''A string of characters.'''

    @staticmethod
    def parse(token):
        # strings have string tokens at the beginning and end
        if (len(token) >= 2 and
                token[0] == tokens.STRING_START and
                token[-1] == tokens.STRING_END):
            return String(token[1:-2])

        return None

class Cons(Object):
    '''Represents a pair of elements.'''

    def __init__(self, car, cdr):
        Object.__init__(self, None)

        self.car = car
        self.cdr = cdr

    def __str_helper(self, item):
        # nil has no contents
        if item is NIL:
            return ''

        if item.cdr is NIL:
            return unicode(item.car)

        if not isinstance(item.cdr, Cons):
            return unicode(item.car) + ' . ' + unicode(item.cdr)

        return unicode(item.car) + ' ' + self.__str_helper(item.cdr)

    def __str__(self):
        return '(' + self.__str_helper(self) + ')'

    def __repr__(self):
        return (self.__class__.__name__ +
               '(' + repr(self.car) + ', ' + repr(self.cdr) + ')')

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

class Symbol(Object):
    '''Symbols store other values, and evaluate to their stored values.'''

    REGEX = re.compile('^[a-zA-Z0-9\-_!?<>]+$')

    @staticmethod
    def parse(token):
        if Symbol.REGEX.match(token):
            return Symbol(token)
        return None

class Keyword(Object):
    '''
    A keyword is a named singleton that evaluates to itself. When created,
    they're stored in a global dict by the interpreter, and are re-allocated to
    new calls as singletons once created. Other than that, they are essentially
    identical to symbols.
    '''

    # keywords have exactly the same naming rules as symbols!
    REGEX = Symbol.REGEX

    def __init__(self, value, comp):
        # since all keywords are singletons, they can't have metadata
        Object.__init__(self, value, can_modify_meta=False)

    def __hash__(self):
        # this is alright since symbols can't contain the keyword token, so
        # their values should never overlap.
        return hash(tokens.KEYWORD + self.value)

    def __str__(self):
        return tokens.KEYWORD + self.value

    def __eq__(self, other):
        # singletons can only be equal when they are the same object!
        return self is other

    @staticmethod
    def parse(token):
        # keyword tokens differ from symbol tokens only in that they start
        # differently.
        word = token[1:]
        if token[0] == tokens.KEYWORD and Keyword.REGEX.match(word):
            return Keyword(word)
        return None

class Map(Object):
    '''
    Holds language constructs mapped to other language constructs. Items are
    ordered in insertion order. Overwriting an existing item leave the
    overwritten item in its original insertion position.
    '''

    def __init__(self):
        Object.__init__(self, collections.OrderedDict())

    def __hash__(self):
        '''It's too difficult to hash dicts, so we don't even try.'''
        raise TypeError('unhashable type: ' + self.__class__.__name__)

    def get(self, key):
        return self.value[key]

    def set(self, key, value):
        self.value[key] = value

class Vector(list):
    '''A static list of language constructs.'''

class Callable(Object):
    '''A base class for objects in our language that can be 'called'.'''

    def __eq__(self, other):
        '''Callables are only equal to themselves.'''
        return other is self

    @staticmethod
    def build_string(kind, name, spec):
        '''
        Build a string to display a typical callable in the interpreter. kind is
        the object type to use, name is the (optional) name to give this
        specific instance of the object, and spec is the ArgSpec object to
        use to build the arguments list.
        '''

        s = '<' + unicode(kind)

        # TODO: set the function name if possible
        # try:
        #     s += ' ' + unicode(self[Keyword.parse(':fn-name')])
        # except:
        #     pass

        s += ' ('

        # compose a list of all arg symbols
        a = []
        for arg_type, arg in spec:
            if arg_type == argspec.ArgSpec.REQUIRED:
                a.append(unicode(arg))
            elif arg_type == argspec.ArgSpec.OPTIONAL:
                arg, default = arg
                a.append('(' + unicode(arg) + ' ' + unicode(default) + ')')
            elif arg_type == argspec.ArgSpec.VARIADIC:
                a.append(unicode(arg))
                a.append(tokens.VARIADIC_ARG)
            else:
                raise ValueError('unhandled arg type: ' + arg_type)

        s += ' '.join(a)
        s += ')>'

        return s

class Function(Callable):
    '''
    Represents a function in our language. Functions take some number of
    arguments, have a body, and are evaluated in some context.
    '''

    # TODO: implement this!

class PrimitiveFunction(Function):
    '''
    Represents a base-level function that can't be broken down into an AST. One
    of the constructs that enables the language to function.
    '''

    # TODO: implement this!

class Macro(Callable):
    '''
    A code-rewriting construct. A macro takes code and returns (expands) code
    dynamically at runtime. Arguments aren't evaluated before being inserted
    into the macro body.
    '''

    # TODO: implement this!

class Environment(dict):
    '''
    A scope that holds variables mapped to their values. Allows us to easily
    package execution state.
    '''

    def __init__(self, parent=None):
        '''Create an environment with an optional parent.'''

        # None means no parent, otherwise must be an environment
        assert parent is None or isinstance(parent, Environment)

        # the environment that contains this environment
        self.parent = parent

        dict.__init__(self)

    def __getitem__(self, symbol):
        '''
        Attempts to locate a given symbol by name in the current environment,
        then in every environment up the parent chain if the symbol could not be
        found. If the symbol is not bound within the parent chain, raises an
        error.
        '''

        try:
            return self.get(symbol)
        except KeyError:
            if self.parent is not None:
                return self.parent[symbol]

        raise ValueError('symbol not found: ' + unicode(symbol))

# singletons
NIL = Cons(None, None)
TRUE = Boolean(True)
FALSE = Boolean(False)
