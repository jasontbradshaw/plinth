#!/usr/bin/env python

import math
import inspect

#
# exceptions
#

class ParserError(Exception):
    """Raised when parsing input fails."""

class OpenParenError(ParserError):
    """Raised when there are too few opening parenthesis."""

    def __init__(self):
        ParserError.__init__(self, "too few opening parenthesis")

class CloseParenError(ParserError):
    """Raised when there are too few closing parenthesis."""

    def __init__(self):
        ParserError.__init__(self, "too few closing parenthesis")

class SymbolNotFoundError(Exception):
    """Raised when a symbol could not be found in an environment chain."""

    def __init__(self, symbol):
        Exception.__init__(self, "could not find " + str(symbol))

class IncorrectArgumentCountError(Exception):
    """Raised when a function is called with the wrong number of arguments."""

    def __init__(self, expected, actual):
        Exception.__init__(self, "expected " + str(expected) +
                ", got " + str(actual))

class WrongArgumentTypeError(Exception):
    """Raised when an argument is of the wrong type."""
    def __init__(self, arg, expected_class):
        Exception.__init__(self, "wrong argument type for " + str(arg) +
                ": expected " + expected_class.__name__.lower() + ", got " +
                arg.__class__.__name__.lower())

class ApplicationError(Exception):
    """Raised when a function could not be applied correctly."""

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
        if token.lower() == Boolean.TRUE_TOKEN:
            return BoolTrue()
        elif token.lower() == Boolean.FALSE_TOKEN:
            return BoolFalse()

        # string
        elif token[0] == Tokens.STRING and token[-1] == token[0]:
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
        if s[0] == Tokens.STRING and s[-1] == s[0]:
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
        # replace literal values with escape sequences
        s = self.value
        s = s.replace("\\", "\\\\")
        s = s.replace("\"", "\\\"")
        s = s.replace("\a", "\\a")
        s = s.replace("\b", "\\b")
        s = s.replace("\f", "\\f")
        s = s.replace("\n", "\\n")
        s = s.replace("\r", "\\r")
        s = s.replace("\t", "\\t")
        s = s.replace("\v", "\\v")

        return Tokens.STRING + s + Tokens.STRING

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

    TRUE_TOKEN = "#t"
    FALSE_TOKEN = "#f"

    def __init__(self, value):
        Atom.__init__(self, bool(value))

    @staticmethod
    def to_boolean(item):
        """
        Transforms the given boolean primitive into BoolTrue or BoolFalse.
        """

        return BoolTrue() if item else BoolFalse()

class BoolTrue(Boolean):
    """
    Represents the 'true' Boolean value.
    """

    def __init__(self):
        Boolean.__init__(self, True)

    def __str__(self):
        return Boolean.TRUE_TOKEN

    def __repr__(self):
        return self.__class__.__name__ + "()"

class BoolFalse(Boolean):
    """
    Represents the 'false' Boolean value.
    """

    def __init__(self):
        Boolean.__init__(self, False)

    def __str__(self):
        return Boolean.FALSE_TOKEN

    def __repr__(self):
        return self.__class__.__name__ + "()"

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
                raise WrongArgumentTypeError(item, Symbol)

        self.arg_symbols = arg_symbols
        self.body = body
        self.parent = parent

        # create a normalized argument for a final vararg if there is one
        self.vararg = None
        if (len(self.arg_symbols) > 0 and
                self.arg_symbols[-1].value.endswith(Tokens.VARARG)):
            self.vararg = Symbol(self.arg_symbols[-1].value[:-len(Tokens.VARARG)])

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
        and return the result. Evaluates all arguments in the given environment
        to obtain their values before inserting them into the evaluation
        environment.
        """

        # ensure that we've got the correct number of argument values
        if self.vararg is not None:
            # we only check for the minimum number when variable
            if len(arg_values) < len(self.arg_symbols) - 1:
                raise IncorrectArgumentCountError(
                        len(self.arg_symbols) - 1, len(arg_values))
        else:
            # we ensure direct correspondence when not variable
            if len(arg_values) != len(self.arg_symbols):
                raise IncorrectArgumentCountError(
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

        # get our arguments with any variadic args
        args, vararg, _, _, = inspect.getargspec(method)

        # set the variadic argument (None if there wasn't one, else the arg)
        self.vararg = vararg

        # add the args and the only variadic arg (if there is one)
        self.arg_names = args
        if vararg is not None:
            self.arg_names.append(vararg + Tokens.VARARG)

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
                raise IncorrectArgumentCountError(
                        len(self.arg_count) - 1, len(arg_values))
        else:
            # we ensure direct correspondence when not variable
            if len(arg_values) != len(self.arg_names):
                raise IncorrectArgumentCountError(
                        self.arg_count, len(arg_values))

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
            raise SymbolNotFoundError(symbol)

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
# tokenizer
#

class Tokens:
    """
    A utility class for language tokens.
    """

    # base syntactic constructs
    OPEN_PAREN = "("
    CLOSE_PAREN = ")"
    QUOTE = "'"
    QUASIQUOTE = "`"
    UNQUOTE = ","
    WHITESPACE = frozenset([" ", "\t", "\n", "\r", "\f", "\v"])
    ESCAPE_CHAR = "\\"
    STRING = '"'
    COMMENT = ";"
    VARARG = "..."

    # special functions
    QUOTE_LONG = "quote"
    QUASIQUOTE_LONG = "quasiquote"
    UNQUOTE_LONG = "unquote"
    LAMBDA = "lambda"
    DEFINE = "define"
    IF = "if"

    # functional programming
    MAP = "map"
    APPLY = "apply"

    # math
    ADD = "+"
    SUBTRACT = "-"
    MULTIPLY = "*"
    DIVIDE = "/"
    POWER = "pow"
    SIN = "sin"
    COS = "cos"
    TAN = "tan"
    ARCTAN = "atan"
    ARCTAN2 = "atan2"

    # comparison
    IS = "is?"
    EQUAL = "="
    GREATER_THAN = ">"
    GREATER_THAN_OR_EQUAL = ">="
    LESS_THAN = "<"
    LESS_THAN_OR_EQUAL = "<="

    # logic
    AND = "and"
    OR = "or"
    NOT = "not"

    # type predicates
    BOOLEANP = "boolean?"
    LISTP = "list?"
    SYMBOLP = "symbol?"
    STRINGP = "string?"
    NUMBERP = "number?"
    INTEGERP = "integer?"
    FLOATP = "float?"
    FUNCTIONP = "function?"

    # list
    NTH = "nth"
    SLICE = "slice"
    LENGTH = "length"
    INSERT = "insert"
    LIST = "list"

    # used to de-sugar various syntactic elements
    DESUGAR = {
        QUOTE: QUOTE_LONG,
        UNQUOTE: UNQUOTE_LONG,
        QUASIQUOTE: QUASIQUOTE_LONG
    }

    def __init__(self):
        raise NotImplementedError("Can't instantiate the '" +
                self.__class__.__name__ + "' class!")

    @staticmethod
    def is_open_paren(token):
        return token == Tokens.OPEN_PAREN

    @staticmethod
    def is_close_paren(token):
        return token == Tokens.CLOSE_PAREN

    @staticmethod
    def is_quote(token):
        return token == Tokens.QUOTE

    @staticmethod
    def is_unquote(token):
        return token == Tokens.UNQUOTE

    @staticmethod
    def is_quasiquote(token):
        return token == Tokens.QUASIQUOTE

    @staticmethod
    def is_whitespace(token):
        # return whether all the characters in the token are whitespace
        for c in token:
            if c not in Tokens.WHITESPACE:
                return False
        return True

    @staticmethod
    def is_escape_char(token):
        return token == Tokens.ESCAPE_CHAR

    @staticmethod
    def is_string(token):
        return token == Tokens.STRING

    @staticmethod
    def is_comment(token):
        return token == Tokens.COMMENT

    @staticmethod
    def tokenize(source):
        """
        Given a string source, returns a generator that reads it character by
        character and yields all the tokens in sequence.
        """

        # buffer where uncommitted characters live
        buf = []

        def flush_fun(buf):
            """Returns the buffer contents as a string and clears the buffer."""

            # get the contents of the buffer as a string
            result = ''.join(buf)

            # uses __delslice__ method of the list so we modify original buffer
            # and not the local copy.
            del buf[:]

            # return the contents of the buffer
            return result

        # work around python's read-only closures
        flush = lambda: flush_fun(buf)

        # iterate over every character in the source string
        for c in source:

            # match escape characters, for having literal values in strings
            if Tokens.is_escape_char(c):
                if len(buf) > 0:
                    yield flush()
                yield c

            # add string delimiters
            elif Tokens.is_string(c):
                if len(buf) > 0:
                    yield flush()
                yield c

            # consume whitespace by collecting it in the buffer
            elif Tokens.is_whitespace(c):
                # flush out other characters before starting a whitespace buffer
                if len(buf) != 0 and not Tokens.is_whitespace(buf[-1]):
                    yield flush()

                # append the whitespace now that the buffer contains no other
                # characters.
                buf.append(c)

            # open parenthesis
            elif Tokens.is_open_paren(c):
                if len(buf) > 0:
                    yield flush()
                yield c

            # close parenthesis
            elif Tokens.is_close_paren(c):
                if len(buf) > 0:
                    yield flush()
                yield c

            # quotes, unquotes, and quasiquotes
            elif c in Tokens.DESUGAR:
                if len(buf) > 0:
                    yield flush()
                yield c

            # just a normal character, so collect it in the buffer
            else:
                # flush whitespace from the buffer before adding normal
                # characters.
                if len(buf) > 0 and Tokens.is_whitespace(buf[-1]):
                    yield flush()

                # append the character now that the buffer contains no
                # whitespace.
                buf.append(c)

        # do a final buffer flush to yield any remaining contents
        if len(buf) > 0:
            yield flush()

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

    def add_token_fun(stack, token):
        """Adds a token to the active scope on the stack."""

        # add the token to the top-most (active) scope of the stack
        stack[-1].append(Atom.atomize(token))

    def indent_fun(stack):
        """Adds an indent level to the ast when an indent marker is found."""

        # add a new level to last indent scope and push same list onto stack
        new_scope = List()
        stack[-1].append(new_scope)
        stack.append(new_scope)

    def dedent_fun(stack):
        """Reduces the indent level, changing the scope that receives tokens."""

        # remove current level of indentation from the stack
        stack.pop()

        if len(stack) < 1:
            raise OpenParenError()

    # work around python's read-only closures
    indent = lambda: indent_fun(stack)
    dedent = lambda: dedent_fun(stack)
    add_token = lambda token: add_token_fun(stack, token)

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
            if Tokens.is_escape_char(token) and not is_escaped:
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
        elif Tokens.is_whitespace(token):
            pass

        # open parenthesis indents
        elif Tokens.is_open_paren(token):
            indent()

        # close parenthesis dedents
        elif Tokens.is_close_paren(token):
            dedent()

        # quote, unquote, quasiquote (the only sugar in our language)
        elif token in Tokens.DESUGAR:
            # we mark the stack and position of the token for quick reference
            sugar_locations.append((stack[-1], len(stack[-1])))
            add_token(token)

        # mark strings
        elif Tokens.is_string(token):
            # mark us as being in a string, let the first case deal with rest
            string_buf.append(token)

        # just a normal token
        else:
            add_token(token)

    # ensure all strings were correctly closed
    if len(string_buf) > 0:
        raise ParserError("unclosed string")

    # check to see if we matched all closing parenthesis (first item is always
    # tokens list, and it never gets popped).
    if len(stack) > 1:
        raise CloseParenError()

    # process all the quote marks into quote functions. we process right-to-left
    # to allow for occurences of "''foo" and the like.
    for scope, i in reversed(sugar_locations):
        # quotes must have something to consume
        if i == len(scope) - 1:
            raise ParserError("invalid quote syntax")

        # have the sugar mark consume the item to its right and replace the
        # slots the two once filled with a new scope containing the desugared
        # function and its argument. we assign it as a list since slice
        # replacement unwraps the iterable it's given, and we need our desugared
        # function to exist within its own List.
        new_symbol = Symbol(Tokens.DESUGAR[scope[i].value])
        scope[i:i + 2] = [List(new_symbol, scope[i + 1])]

    # return the canonical abstract syntax tree
    return ast

def add(a, b, *rest):
    """Adds the all the given numbers together."""

    # gather all the arguments into one list
    nums = [a, b] + list(rest)

    # add all the arguments together while checking type
    total = 0
    for n in nums:
        if not isinstance(n, Number):
            raise WrongArgumentTypeError(n, Number)
        total += n.value

    return Number.to_number(total)

def sub(a, b):
    """Subtracts the second Number from the first Number."""

    if not isinstance(a, Number):
        raise WrongArgumentTypeError(a, Number)
    elif not isinstance(b, Number):
        raise WrongArgumentTypeError(b, Number)

    return Number.to_number(a.value - b.value)

def mul(a, b, *rest):
    """Multiplies all the given numbers together."""

    # gather all the arguments into one list
    nums = [a, b] + list(rest)

    # multiply all the arguments together while checking type
    product = 1
    for n in nums:
        if not isinstance(n, Number):
            raise WrongArgumentTypeError(n, Number)
        product *= n.value

        # stop multiplying if the product ever goes to zero
        if product == 0:
            break

    return Number.to_number(product)

def power(a, b):
    """Raises a to the power of b."""

    if not isinstance(a, Number):
        raise WrongArgumentTypeError(a, Number)
    elif not isinstance(b, Number):
        raise WrongArgumentTypeError(b, Number)

    return Number.to_number(a.value ** b.value)

def sin(a):
    """Takes the sin of a."""

    if not isinstance(a, Number):
        raise WrongArgumentTypeError(a, Number)

    return Number.to_number(math.sin(a.value))

def cos(a):
    """Takes the cosine of a."""

    if not isinstance(a, Number):
        raise WrongArgumentTypeError(a, Number)

    return Number.to_number(math.cos(a.value))

def tan(a):
    """Takes the tangent of a."""

    if not isinstance(a, Number):
        raise WrongArgumentTypeError(a, Number)

    return Number.to_number(math.tan(a.value))

def atan(a):
    """Takes the arctangent of a."""

    if not isinstance(a, Number):
        raise WrongArgumentTypeError(a, Number)

    return Number.to_number(math.atan(a.value))

def atan2(a):
    """Takes the second arctangent of a."""

    if not isinstance(a, Number):
        raise WrongArgumentTypeError(a, Number)

    return Number.to_number(math.atan2(a.value))

def div(a, b):
    """Subtracts the second Number from the first Number."""

    if not isinstance(a, Number):
        raise WrongArgumentTypeError(a, Number)
    elif not isinstance(b, Number):
        raise WrongArgumentTypeError(b, Number)

    return Number.to_number(a.value / b.value)

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

    if not isinstance(lst, List):
        raise WrongArgumentTypeError(lst, List)
    elif not isinstance(i, Integer):
        raise WrongArgumentTypeError(i, Integer)

    # throws a nice index error by itself, we don't need to wrap it
    return lst[i.value]

def slice_(start, end, lst):
    """
    Returns a new list containing the elements from start (inclusive) to end
    (exclusive) in the given list.
    """

    if not isinstance(lst, List):
        raise WrongArgumentTypeError(lst, List)
    elif not isinstance(start, Integer):
        raise WrongArgumentTypeError(start, Integer)
    elif not isinstance(end, Integer):
        raise WrongArgumentTypeError(end, Integer)

    return lst[start.value:end.value]

def length(lst):
    """
    Returns the length of a list.
    """

    if not isinstance(lst, List):
        raise WrongArgumentTypeError(lst, List)

    return Integer(len(lst))

def insert(i, item, lst):
    """
    Insert an item before the given position in the given list and return a new
    list with the item inserted at the specified position.
    """

    if not isinstance(i, Integer):
        raise WrongArgumentTypeError(i, Integer)
    elif not isinstance(lst, List):
        raise WrongArgumentTypeError(lst, List)

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

    # numbers are compared mathematically, regardless of type
    if isinstance(a, Number) and isinstance(b, Number):
        return Boolean.to_boolean(a.value == b.value)

    # things can't be equal if they're not the same class
    elif not (isinstance(a, b.__class__) and isinstance(b, a.__class__)):
        return BoolFalse()

    # we know both args are of the same class now, no need to check both

    # compare lists recursively
    elif isinstance(a, List):
        # must be of the same length
        if len(a) != len(b):
            return BoolFalse()

        # compare all items in the list
        for a_item, b_item in zip(a, b):
            if not equal(a_item, b_item).value:
                return BoolFalse()

        # if we made it to here, we were equal!
        return BoolTrue()

    # functions can never be equal, there are too many things to check
    elif isinstance(a, Function):
        return BoolFalse()

    # compare everything else by value (booleans, symbols, etc.)
    return Boolean.to_boolean(a.value == b.value)

def gt(a, b):
    """
    Compares two numbers using >.
    """

    if not isinstance(a, Number):
        raise WrongArgumentTypeError(a, Number)
    elif not isinstance(b, Number):
        raise WrongArgumentTypeError(b, Number)

    return Boolean.to_boolean(a.value > b.value)

def gte(a, b):
    """
    Compares two numbers using >=.
    """

    if not isinstance(a, Number):
        raise WrongArgumentTypeError(a, Number)
    elif not isinstance(b, Number):
        raise WrongArgumentTypeError(b, Number)

    return Boolean.to_boolean(a.value >= b.value)

def lt(a, b):
    """
    Compares two numbers using <.
    """

    if not isinstance(a, Number):
        raise WrongArgumentTypeError(a, Number)
    elif not isinstance(b, Number):
        raise WrongArgumentTypeError(b, Number)

    return Boolean.to_boolean(a.value < b.value)

def lte(a, b):
    """
    Compares two numbers using <=.
    """

    if not isinstance(a, Number):
        raise WrongArgumentTypeError(a, Number)
    elif not isinstance(b, Number):
        raise WrongArgumentTypeError(b, Number)

    return Boolean.to_boolean(a.value <= b.value)

def apply_(f, args):
    """
    Applies a function to some arguments and returns the result.
    """

    if not isinstance(f, Function):
        raise WrongArgumentTypeError(f, Function)
    elif not isinstance(args, List):
        raise WrongArgumentTypeError(args, List)

    return f(*args)

def not_(a):
    """
    Returns the opposite boolean of that passed in. All things that aren't #f
    are #t, so we return the opposite of that.
    """

    return Boolean.to_boolean(isinstance(a, BoolFalse))

# these functions serve as markers for whether the function being called is
# special. we check to see if the function for the symbol is one of these
# functions, and if so we evaluate it in whatever way it requires. this allows
# the user to define new symbols that point to these functions, but still have
# the functions work in the same way.
quote = PrimitiveFunction(lambda e: None)
lambda_ = PrimitiveFunction(lambda args, body: None)
define = PrimitiveFunction(lambda symbol, value: None)
if_ = PrimitiveFunction(lambda cond, success, failure: None)
and_ = PrimitiveFunction(lambda a, b, *rest: None)
or_ = PrimitiveFunction(lambda a, b, *rest: None)
list_ = PrimitiveFunction(lambda *items: None)

# the base environment for the interpreter
global_env = Environment(None)

# functions that need special treatment during evaluation
global_env[Symbol(Tokens.QUOTE_LONG)] = quote
global_env[Symbol(Tokens.LAMBDA)] = lambda_
global_env[Symbol(Tokens.DEFINE)] = define
global_env[Symbol(Tokens.IF)] = if_
global_env[Symbol(Tokens.AND)] = and_
global_env[Symbol(Tokens.OR)] = or_
global_env[Symbol(Tokens.LIST)] = list_

# adds a new primitive function to the gloval environment
add_prim = lambda t, f: global_env.__setitem__(Symbol(t), PrimitiveFunction(f))

# self-contained functions that need no special assistance
# logical
add_prim(Tokens.NOT, not_)

# math
add_prim(Tokens.ADD, add)
add_prim(Tokens.SUBTRACT, sub)
add_prim(Tokens.MULTIPLY, mul)
add_prim(Tokens.DIVIDE, div)
add_prim(Tokens.POWER, power)
add_prim(Tokens.SIN, sin)
add_prim(Tokens.COS, cos)
add_prim(Tokens.TAN, tan)
add_prim(Tokens.ARCTAN, atan)
add_prim(Tokens.ARCTAN2, atan2)

# functional programming
add_prim(Tokens.APPLY, apply_)

# comparison
add_prim(Tokens.IS, is_)
add_prim(Tokens.EQUAL, equal)
add_prim(Tokens.GREATER_THAN, gt)
add_prim(Tokens.GREATER_THAN_OR_EQUAL, gte)
add_prim(Tokens.LESS_THAN, lt)
add_prim(Tokens.LESS_THAN_OR_EQUAL, lte)

# types
add_prim(Tokens.BOOLEANP, booleanp)
add_prim(Tokens.LISTP, listp)
add_prim(Tokens.SYMBOLP, symbolp)
add_prim(Tokens.STRINGP, stringp)
add_prim(Tokens.NUMBERP, numberp)
add_prim(Tokens.INTEGERP, integerp)
add_prim(Tokens.FLOATP, floatp)
add_prim(Tokens.FUNCTIONP, functionp)

# list
add_prim(Tokens.NTH, nth)
add_prim(Tokens.SLICE, slice_)
add_prim(Tokens.LENGTH, length)
add_prim(Tokens.INSERT, insert)

def evaluate(item, env=global_env):
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
            raise ApplicationError("nothing to apply")

        # evaluate functions using their arguments
        function = evaluate(item[0])
        args = item[1:]

        # make sure our first item evaluated to a function
        if not isinstance(function, Function):
            raise ApplicationError("wrong type to apply: " + str(function))

        # quote
        if function is quote:
            if len(args) != 1:
                raise IncorrectArgumentCountError(1, len(args))

            # return the argument to quote unevaluated
            return args[0]

        # list
        elif function is list_:
            result = List()
            for item in args:
                result.append(evaluate(item, env))
            return result

        # function
        elif function is lambda_:
            if len(args) != 2:
                raise IncorrectArgumentCountError(2, len(args))

            arg_symbols = args[0]
            body = args[1]

            # return a function with the current environment as the parent
            return Function(arg_symbols, body, env)

        # define
        elif function is define:
            if len(args) != 2:
                raise IncorrectArgumentCountError(2, len(args))

            symbol = args[0]
            value = args[1]

            # make sure we're defining to a symbol
            if not isinstance(symbol, Symbol):
                raise WrongArgumentTypeError(symbol, Symbol)

            # evaluate the argument, map the symbol to the result in the current
            # environment, then return the evaluated value. this allows for
            # chains of definitions, or simultaneous variable assignments to the
            # same value.
            result = evaluate(value)
            env[symbol] = result
            return result

        # if
        elif function is if_:
            if len(args) != 3:
                raise IncorrectArgumentCountError(3, len(args))

            cond = args[0]
            success_clause = args[1]
            failure_clause = args[2]

            # every value is considered #t except for #f
            if isinstance(evaluate(cond), BoolFalse):
                return evaluate(failure_clause)
            return evaluate(success_clause)

        # logical and
        elif function is and_:
            if len(args) < 2:
                raise IncorrectArgumentCountError(2, len(args))

            # evaluate the arguments, returning the final one if none were #f,
            # otherwise the last evaluated item, #f.
            last_item = None
            for item in args:
                last_item = evaluate(item)
                if isinstance(last_item, BoolFalse):
                    break

            return last_item

        # logical or
        elif function is or_:
            if len(args) < 2:
                raise IncorrectArgumentCountError(2, len(args))

            # evaluate the arguments, returning the firt one that's not #f,
            last_item = None
            for item in args:
                last_item = evaluate(item)
                if not isinstance(last_item, BoolFalse):
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
            source = raw_input(prompt)

            # strip comments from the source (it's as if they don't exist)
            source = source.split(Tokens.COMMENT, 1)[0].strip()

            for result in parse(Tokens.tokenize(source)):
                print evaluate(result)

        except KeyboardInterrupt:
            # reset prompt on Ctrl+C
            print
        except EOFError:
            # exit on Ctrl+D
            print
            sys.exit()
        except Exception, e:
            # print all other problems and clear source
            traceback.print_exc()
