#!/usr/bin/env python

import re

class ParserError(Exception):
    """Raised when parsing input fails."""

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

    @classmethod
    def init():
        raise NotImplementedError("Can't instantiate the 'Tokens' class!")

    @staticmethod
    def is_open_paren(s):
        return s == Tokens.OPEN_PAREN

    @staticmethod
    def is_close_paren(s):
        return s == Tokens.CLOSE_PAREN

    @staticmethod
    def is_quote(s):
        return s == Tokens.QUOTE

    @staticmethod
    def is_whitespace(s):
        return True if re.match(Tokens.WHITESPACE, s) else False

def parse(source):
    """
    Given a string source, reads it character by character and generates a parse
    tree for it.
    """

    # committed tokens
    tokens = []

    # buffer where uncommitted characters live
    buf = []

    # stack that keeps track of scopes (where first scope is always tokens list)
    stack = [tokens]

    def flush_raw(stack, buf):
        """Copy buffer contents into latest scope and empty the buffer."""

        # don't flush if buffer is empty
        if len(buf) > 0:
            # append to current indentation level and clear buffer
            stack[-1].append(''.join(buf))

            # uses __delslice__ method of the list so we modify original buffer
            # and not the local copy.
            del buf[:]

    def indent_raw(stack, buf):
        """Add another level to tokens when an indentation marker is found."""

        flush_raw(stack, buf)

        # add a new level to last indent scope and push same list onto stack
        new_scope = []
        stack[-1].append(new_scope)
        stack.append(new_scope)

    def dedent_raw(stack, buf):
        """Reduce indentation level."""

        # flush the buffer and remove current level from the stack
        flush_raw(stack, buf)
        stack.pop()

    # work around python's read-only closures
    flush = lambda: flush_raw(stack, buf)
    indent = lambda: indent_raw(stack, buf)
    dedent = lambda: dedent_raw(stack, buf)

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

        # just a normal character
        else:
            buf.append(c)

    # TODO: turn tokens into language constructs (Integer, Float, etc.)

    return tokens

if __name__ == "__main__":
    while 1:
        i = raw_input("> ")
        print parse(i)

