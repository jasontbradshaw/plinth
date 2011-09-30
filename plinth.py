#!/usr/bin/env python

import re

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
        Exception.__init__(self, "could not find symbol: " + str(symbol))

class IncorrectArgumentCountError(Exception):
    """Raised when a function is called with the wrong number of arguments."""

    def __init__(self, expected, actual):
        Exception.__init__(self, "incorrect number of arguments: expected " +
                    str(expected) + ", got " + str(actual))

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
        # return a reversed copy of ourself as a List
        return List(*[self[i] for i in xrange(len(self) - 1, -1, -1)])

    def append(self, item):
        self.items.append(item)

    def extend(self, other_items):
        self.items.extend(other_items)

    def pop(self, index=None):
        if index is None:
            index = len(self) - 1

        self.items.pop(index)

class Number(Atom):
    """
    Numbers can be added, subtracted, etc. and hold a single value.
    """

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

class Function(Atom):
    """
    Represents a function in our language. Functions take some number of
    arguments, have a body, and are evaluated in some context.
    """

    def __init__(self, arg_symbols, body, parent):
        """
        Creates a function given a list of its argument symbols, its body, and
        its parent environment, i.e. the environment it was created and will be
        evaluated in (its closure).
        """

        self.arg_symbols = tuple(arg_symbols)
        self.body = body
        self.parent = parent

    def __str__(self):
        return "<function (" + ' '.join(map(str, self.arg_symbols)) + ")>"

    def __repr__(self):
        return (self.__class__.__name__ + "(" +
                repr(self.arg_symbols) + ", " +
                repr(self.body) + ", " +
                repr(self.parent) + ")")

    def __call__(self, calling_env, *arg_values):
        """
        Evaluate this function given a list of values to use for its arguments
        and return the result. Evaluates all arguments in the given environment
        to obtain their values before inserting them into the evaluation
        environment.
        """

        # ensure that we've got the correct number of argument values
        if len(self.arg_symbols) != len(arg_values):
            raise IncorrectArgumentCountError(len(arg_symbols), len(arg_values))

        # create a new environment with the parent set as our parent enviroment
        env = Environment(self.parent)

        # put the arguments into the new environment, mapping by position
        for symbol, value in zip(self.arg_symbols, arg_values):
            env[symbol] = evaluate(value, calling_env)

        # evaluate our body using the new environment and return the result
        return evaluate(self.body, env)

class PrimitiveFunction(Function):
    """
    Represents a base-level function that can't be broken down into an AST. One
    of the constructs that enables the language to function.
    """

    # used to parse the actual and expected number of arguments from the error a
    # function raises when an incorrect number of arguments is given.
    ARGUMENT_REGEX = re.compile(
            "^.* takes exactly (\d+) arguments? \((\d+) given\)$")

    def __init__(self, method, evaluate_arguments=True):
        """
        Create a primitive function that works much like a normal function,
        except that the method is a Python function that does work using the
        arguments given to __call__. The second parameter specifies whether
        arguments should be evaluated before given to the method.
        """

        self.method = method
        self.evaluate_arguments = evaluate_arguments

    def __str__(self):
        return "<primitive-function>"

    def __repr__(self):
        return (self.__class__.__name__ + "(" +
                repr(self.args) + ", " +
                repr(self.body) + ")")

    def __call__(self, calling_env, *args):
        # evaluate our arguments if specified. since quote doesn't do this, we
        # allow inhibition to be specified when the primite function is created.
        if self.evaluate_arguments:
            ev = lambda x: evaluate(x, calling_env)
            args = map(ev, args)

        try:
            return self.method(*args)

        # if we had the wrong number of arguments, parse the numbers out and
        # raise them in an exception.
        except TypeError, te:
            # try to match the text for the argument error
            match = PrimitiveFunction.ARGUMENT_REGEX.match(te.message)

            if match is not None:
                # get actual and expected arg counts (group 0 is entire match)
                expected = match.group(1)
                actual = match.group(2)

                # raise the error with the parsed argument counts
                raise IncorrectArgumentCountError(expected, actual)

            # raise the original exception if it didn't match the regex
            raise te

class Tokens:
    """
    A utility class for language tokens.
    """

    OPEN_PAREN = "("
    CLOSE_PAREN = ")"
    QUOTE = "'"
    WHITESPACE = frozenset([" ", "\t", "\n", "\r", "\f", "\v"])
    ESCAPE_CHAR = "\\"
    STRING = '"'
    COMMENT = ";"

    # easy way to convert syntactic sugar to expanded forms
    DESUGAR = {
        "'": "quote"
    }

    def __init__(self):
        raise NotImplementedError("Can't instantiate the '" +
                self.__class__.__name__ + "' class!")

    @staticmethod
    def is_open_paren(c):
        return c == Tokens.OPEN_PAREN

    @staticmethod
    def is_close_paren(c):
        return c == Tokens.CLOSE_PAREN

    @staticmethod
    def is_quote(c):
        return c == Tokens.QUOTE

    @staticmethod
    def is_whitespace(c):
        return c in Tokens.WHITESPACE

    @staticmethod
    def is_escape_char(c):
        return c == Tokens.ESCAPE_CHAR

    @staticmethod
    def is_string(c):
        return c == Tokens.STRING

    @staticmethod
    def is_comment(c):
        return c == Tokens.COMMENT

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

            # quotes
            elif Tokens.is_quote(c):
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

    # return the canonical abstract syntax tree
    return ast

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
                print repr(result)
                print result

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
