#!/usr/bin/env python

import re

class ParserError(Exception):
    """Raised when parsing input fails."""

class OpenParenError(ParserError):
    """Raised when there are too few opening parenthesis."""

class CloseParenError(ParserError):
    """Raised when there are too few closing parenthesis."""

class Symbol:
    def __init__(self, symbol_string):
        self.value = symbol_string

    def __str__(self):
        return self.value

    def __repr__(self):
        return "(SYMBOL " + str(self) + ")"

class Integer:
    def __init__(self, integer_string):
        self.value = int(integer_string)

    def __str__(self):
        return str(self.value)

    def __repr__(self):
        return "(INTEGER " + str(self) + ")"

class Float:
    def __init__(self, float_string):
        self.value = float(float_string)

    def __str__(self):
        return str(self.value)

    def __repr__(self):
        return "(FLOAT " + str(self) + ")"

class Tokens:
    """
    A container class for language tokens.
    """

    OPEN_PAREN = "("
    CLOSE_PAREN = ")"
    QUOTE = "'"
    WHITESPACE = "\s"
    ESCAPE_CHAR = "\\"
    STRING = "\"|'"

    @classmethod
    def init():
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
        return True if re.match(Tokens.WHITESPACE, c) else False

    @staticmethod
    def is_escape_char(c):
        return c == Token.ESCAPE_CHAR

    @staticmethod
    def is_string(c):
        return True if re.match(Tokens.STRING, c) else False

def parse(source):
    """
    Given a string source, reads it character by character and generates a parse
    tree for it.
    """

    # committed tokens
    tokens = []

    # buffer where uncommitted characters live
    buf = []

    # stack that keeps track of scopes, where first scope is always tokens list
    stack = [tokens]

    def flush_fun(stack, buf):
        """Copy buffer contents into latest scope and empty the buffer."""

        if len(stack) < 1:
            raise OpenParenError("Too few opening parenthesis.")

        # don't flush if buffer is empty
        if len(buf) > 0:
            # append to current indentation level and clear buffer
            stack[-1].append(''.join(buf))

            # uses __delslice__ method of the list so we modify original buffer
            # and not the local copy.
            del buf[:]

    def indent_fun(stack, buf):
        """Add another level to tokens when an indentation marker is found."""

        flush_fun(stack, buf)

        # add a new level to last indent scope and push same list onto stack
        new_scope = []
        stack[-1].append(new_scope)
        stack.append(new_scope)

    def dedent_fun(stack, buf):
        """Reduce indentation level."""

        # flush the buffer and remove current level from the stack
        flush_fun(stack, buf)
        stack.pop()

        if len(stack) < 1:
            raise OpenParenError("Too few opening parenthesis.")

    # work around python's read-only closures
    flush = lambda: flush_fun(stack, buf)
    indent = lambda: indent_fun(stack, buf)
    dedent = lambda: dedent_fun(stack, buf)

    # iterate over every character in the source string
    for c in source:

        # skip whitespace, flushing the buffer if necessary
        if Tokens.is_whitespace(c):
            flush()

        # open parenthesis
        elif Tokens.is_open_paren(c):
            indent()

        # close parenthesis
        elif Tokens.is_close_paren(c):
            dedent()

        # quotes are special tokens
        elif Tokens.is_quote(c):
            buf.append(c)
            flush()

        # just a normal character
        else:
            buf.append(c)

    # do a final buffer flush to catch any remaining contents
    flush()

    # check to see if we matched all closing parenthesis (first item is always
    # tokens list, and it never gets popped).
    if len(stack) > 1:
        raise CloseParenError("Too few closing parenthesis.")

    # TODO: turn tokens into language constructs (Integer, Float, etc.)

    return tokens

if __name__ == "__main__":
    import sys
    import traceback

    source = ""

    standard_prompt = "> "
    continue_prompt = ": "
    prompt = standard_prompt

    while 1:
        try:
            # get input from user and try to parse and print it
            source += " " + raw_input(prompt)
            print parse(source)

            # reset after successful print
            prompt = standard_prompt
            source = ""
        except CloseParenError:
            # if we fail because of this error, get more input with a new prompt
            prompt = continue_prompt
        except KeyboardInterrupt:
            # reset prompt on Ctrl+C
            prompt = standard_prompt
            source = ""
            print
        except EOFError:
            # exit on Ctrl+D
            print
            sys.exit()
        except Exception, e:
            # print all other problems
            traceback.print_exc()


