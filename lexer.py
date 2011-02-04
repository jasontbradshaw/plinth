#!/usr/bin/env python

import re

OPEN_PAREN = "("
CLOSE_PAREN = ")"
QUOTE = "'"

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

class ParserError(Exception):
    """ Raised when parsing input fails. """

def parse(s):
    return lex(tokenize(s))

def tokenize(s):
    """
    Breaks the given string into a list of strings using our language
    conventions.
    """

    # this is simple and fast. thanks Norvig!
    return s.replace(OPEN_PAREN, " " + OPEN_PAREN + " ").replace(
            CLOSE_PAREN, " " + CLOSE_PAREN + " ").split()

def lex(tokens):
    """
    Turns a given tokenized expression into a list of symbols.
    """

    if len(tokens) == 0:
        raise ValueError("Invalid input.")

    token = tokens.pop(0)
    if token == OPEN_PAREN:
        e = []
        while len(tokens) > 0 and tokens[0] != CLOSE_PAREN:
            e.append(lex(tokens))

        # pop off ')' if it exists
        if len(tokens) > 0:
            tokens.pop(0)
        else:
            raise ParserError("Expecting closing parenthesis in expression.")

        # return the lexed input stream
        return e
    elif token == CLOSE_PAREN:
        raise ParserError("Found unexpected closing parenthesis in expression.")
    else:
        return atomize(token)

def atomize(symbol):
    """
    Turns a given symbol into an atom and returns it.
    """

    # attempt to return an int
    try:
        return Integer(symbol)
    except ValueError:
        pass

    # attempt to return a float
    try:
        return Float(symbol)
    except ValueError:
        pass

    # otherwise, just return a symbol
    return Symbol(symbol)

if __name__ == "__main__":
    import sys
    import traceback
    from pprint import pprint

    print "Test Parser:"

    while 1:
        try:
            s = raw_input("> ")
            try:
                pprint(parse(s))
            except Exception, e:
                print e
                traceback.print_exc()
        except KeyboardInterrupt:
            print
            continue
        except EOFError:
            print
            break
