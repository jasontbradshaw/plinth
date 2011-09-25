#!/usr/bin/env python

class ParserError(Exception):
    """Raised when parsing input fails."""

class OpenParenError(ParserError):
    """Raised when there are too few opening parenthesis."""

class CloseParenError(ParserError):
    """Raised when there are too few closing parenthesis."""

class Tokens:
    """
    A container class for language tokens. Tokens represent themselves, so there
    is no translation between the syntax representation of a token and the
    internal representation of a token.
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
        raise NotImplementedError("Can't instantiate the 'Tokens' class!")

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

class Atom:
    """
    Represents anything that's not a list: numbers, strings, symbols, etc.
    """

    def __init__(self, value):
        self.value = value

    def __str__(self):
        return str(self.value)

    def __repr__(self):
        return "Atom(" + repr(self.value) + ")"

class Cons:
    """
    Represents both dotted pairs (improper lists) and linked lists of many
    elements, which end with the empty list, nil.
    """

    def __init__(self, first, rest):
        # we can only make cons of Atom and Cons (or Nil, but it's both)
        assert isinstance(first, Atom)
        assert isinstance(rest, Atom) or isinstance(rest, Cons)

        self.first = first
        self.rest = rest

    def __firststr(self, inner):
        if isinstance(self.first, Cons):
            return self.first.__str__(inner)
        return str(self.first)

    def __reststr(self, inner):
        if isinstance(self.rest, Cons):
            return self.rest.__str__(inner)
        return str(self.rest)

    def __str__(self, inner=False):
        if isinstance(self.rest, Nil):
            # deal with the special case of (nil . nil)
            if isinstance(self.first, Nil):
                result = self.__firststr(True) + " . " + self.__reststr(True)
            else:
                result = self.__firststr(True)
        elif isinstance(self.rest, Atom):
            result = self.__firststr(True) + " . " + self.__reststr(True)
        else:
            result = self.__firststr(True) + " " + self.__reststr(True)

        return result if inner else "(" + result + ")"

    def __repr__(self):
        return "Cons(" + repr(self.first) + ", " + repr(self.rest) + ")"

    def __getitem__(self, index):
        """
        Recursively get the item at a given index in the cons chain.
        """

        # base case, return the first item
        if index == 0:
            return self.first

        # non-zero index, should we keep going?
        elif isinstance(self.rest, Cons) and not isinstance(self.rest, Nil):
            return self.rest[index - 1]

        # dotted-pair's final item is non-nil and index refers to it
        elif index == 1 and not isinstance(self.rest, Nil):
            return self.rest

        raise IndexError("Index out of range")

class Nil(Atom, Cons):
    """
    Represents the empty list.
    """

    def __init__(self):
        pass

    def __str__(self):
        return "nil"

    def __repr__(self):
        return "Nil()"

class Number(Atom):
    """
    Numbers can be added, subtracted, etc. and hold a single value.
    """

    def __init__(self, value):
        Atom.__init__(self, value)

class Integer(Number):
    """
    Integers represent numbers with no decimal part.
    """

    def __init__(self, value):
        Number.__init__(self, int(value))

    def __str__(self):
        return str(self.value)

    def __repr__(self):
        return "Integer(" + repr(self.value) + ")"

class Float(Number):
    """
    Floats represent floating-point numbers.
    """

    def __init__(self, value):
        Number.__init__(self, float(value))

    def __str__(self):
        # we use repr to get the entire float value, unrounded
        return repr(self.value)

    def __repr__(self):
        return "Float(" + repr(self.value) + ")"

class String(Atom):
    """
    Strings are immutable collections of character data. There's no such thing
    as a 'character' in our language, only single-element strings.
    """

    def __init__(self, value):
        # set the value to the non-delimted string value
        Atom.__init__(self, str(value)[1:-1])

    def __str__(self):
        return Tokens.STRING + str(self.value) + Tokens.STRING

    def __repr__(self):
        return "String(" + repr(self.value) + ")"

class Symbol(Atom):
    """
    Symbols store other values, and evaluate to their stored values.
    """

    def __init__(self, value):
        Atom.__init__(self, value)

    def __repr__(self):
        return "Symbol(" + repr(self.value) + ")"

class Boolean(Atom):
    """
    Represents a single Boolean value.
    """

    # the strings that represent true and false (upper/lower case are the same)
    TRUE_STRING = "#t"
    FALSE_STRING = "#f"

    # where the singleton instances of true and false are stored (get initialzed
    # by the atomizer on first use).
    TRUE = None
    FALSE = None

    def __init__(self, value):
        Atom.__init__(self, bool(value))

class BoolTrue(Boolean):
    """
    Represents the 'true' Boolean value.
    """

    def __init__(self):
        Boolean.__init__(self, True)

    def __str__(self):
        return Boolean.TRUE_STRING

    def __repr__(self):
        return "BoolTrue()"

class BoolFalse(Boolean):
    """
    Represents the 'false' Boolean value.
    """

    def __init__(self):
        Boolean.__init__(self, False)

    def __str__(self):
        return Boolean.FALSE_STRING

    def __repr__(self):
        return "BoolFalse()"

class Atomizer:
    """
    Turns a token into an atom.
    """

    @classmethod
    def atomize(token):
        """
        Takes the given string and returns an Atom representing that string in
        its most natural form (boolean, string, etc.).
        """

        # integer
        try:
            return Integer(token)
        except ValueError:
            pass

        # float
        try:
            return Float(Token)
        except ValueError:
            pass

        # boolean
        if token.lower() == Boolean.TRUE_STRING:
            # initialze singleton
            if Boolean.TRUE is None:
                Boolean.TRUE = BoolTrue()
            return Boolean.TRUE
        elif token.lower() == Boolean.FALSE_STRING:
            # initialze singleton
            if Boolean.FALSE is None:
                Boolean.FALSE = BoolFalse()
            return Boolean.FALSE

        # string
        elif token[0] == Tokens.STRING and token[-1] == token[0]:
            return String(token)

        # the base case for all tokens is a symbol
        return Symbol(token)

def tokenize(source):
    """
    Given a string source, returns a generator that reads it character by
    character and yields all the tokens in sequence.
    """

    # buffer where uncommitted characters live
    buf = []

    def flush_fun(buf):
        """Copy buffer contents into tokens and empty the buffer."""

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

            # append the whitespace now that the buffer contains no other chars
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
            # flush whitespace from the buffer before adding normal characters
            if len(buf) > 0 and Tokens.is_whitespace(buf[-1]):
                yield flush()

            # append the character now that the buffer contains no whitespace
            buf.append(c)

    # do a final buffer flush to yield any remaining contents
    if len(buf) > 0:
        yield flush()

def parse(token_source):
    """
    Given a token source, parses the token sequence into an abstract syntax
    tree.
    """

    # where the abstract syntax tree is held
    ast = []

    # stack where the active scope is kept. starts with the ast as the initial
    # active scope where tokens are added.
    stack = [ast]

    def add_token_fun(stack, token):
        """Adds a token to the active scope on the stack."""

        # add the token to the top-most (active) scope of the stack
        stack[-1].append(token)

    def indent_fun(stack):
        """Adds an indent level to the ast when an indent marker is found."""

        # add a new level to last indent scope and push same list onto stack
        new_scope = []
        stack[-1].append(new_scope)
        stack.append(new_scope)

    def dedent_fun(stack):
        """Reduces the indent level, changing the scope that receives tokens."""

        # remove current level of indentation from the stack
        stack.pop()

        if len(stack) < 1:
            raise OpenParenError("Too few opening parenthesis.")

    # work around python's read-only closures
    indent = lambda: indent_fun(stack)
    dedent = lambda: dedent_fun(stack)
    add_token = lambda token: add_token_fun(stack, token)

    # we keep a buffer of string parts so we can concatenate all the parts of
    # the string together at once, and so we can check whether we're in a string
    # and whether tokens are escaped.
    string_buf = []

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
            # will be appended literally.
            if Tokens.is_escape_char(token):
                pass

            # if the token preceding this token is an escape char, this token
            # gets appended to the string literally. this will never exceed
            # bounds since by this point, the string contains AT LEAST a string
            # token and one other token, guaranteeing that there's an index two
            # back from the end.
            elif Tokens.is_escape_char(string_buf[-2]):
                pass

            # end the string and flush if we found an unescaped string token
            # that matched the initial string token kind. this allows us to
            # define several different string delimiting tokens.
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
        raise ParserError("Unclosed string.")

    # check to see if we matched all closing parenthesis (first item is always
    # tokens list, and it never gets popped).
    if len(stack) > 1:
        raise CloseParenError("Too few closing parenthesis.")

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
            print parse(tokenize(source))
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
