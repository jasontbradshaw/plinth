#!/usr/bin/env python

import re

class Symbol:
    def __init__(self, symbol_string):
        self.value = symbol_string

    def __str__(self):
        return self.value

    def __repr__(self):
        return str(self)

class LexingError(Exception):
    """ Raised when parsing input fails. """

def parse(s):
    return lex(tokenize(s))

def tokenize(s):
    """
    Breaks the given string into a list of strings using our language
    conventions.
    """

    # this is simple and fast. thanks Norvig!
    return s.replace("(", " ( ").replace(")", " ) ").split()

def lex(tokens):
    """
    Turns a given tokenized expression into a list of symbols.
    """

    if len(tokens) == 0:
        raise ValueError("Invalid input.")

    token = tokens.pop(0)
    if token == "(":
        e = []
        while len(tokens) > 0 and tokens[0] != ")":
            e.append(lex(tokens))

        # pop off ')'
        if len(tokens) > 0:
            tokens.pop(0)
        else:
            raise LexingError("Expecting closing parenthesis in expression.")

        return e
    elif token == ")":
        raise LexingError("Found unexpected closing parenthesis in expression.")
    else:
        return atomize(token)

def atomize(symbol):
    """
    Turns a given symbol into an atom and returns it.
    """

    # attempt to return an int
    try:
        return int(symbol)
    except ValueError:
        pass

    # attempt to return a float
    try:
        return float(symbol)
    except ValueError:
        pass

    # otherwise, return a symbol
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
