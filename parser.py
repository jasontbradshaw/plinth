#!/usr/bin/env python

import re

class ParserError(Exception):
    """Raised when parsing input fails."""

class OpenParenError(ParserError):
    """Raised when there are too few opening parenthesis."""

class CloseParenError(ParserError):
    """Raised when there are too few closing parenthesis."""

class Tokens:
    """
    A container class for language tokens.
    """

    OPEN_PAREN = "("
    CLOSE_PAREN = ")"
    QUOTE = "'"
    WHITESPACE = "\s"
    ESCAPE_CHAR = "\\"
    STRING = '"'

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
        return bool(re.match(Tokens.WHITESPACE, c))

    @staticmethod
    def is_escape_char(c):
        return c == Tokens.ESCAPE_CHAR

    @staticmethod
    def is_string(c):
        return c == Tokens.STRING

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

        # flush first so we clear the buffer
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
    in_string = False
    is_escaped = False
    for c in source:

        # deal with strings first to avoid triggering other language constructs
        if in_string:
            # every character inside a string gets inserted literally
            buf.append(c)

            # treat escaped characters as literal
            if is_escaped:
                # reset the escape marker after we've appended the escaped char
                is_escaped = False

            # only allow for escape character inside strings
            elif Tokens.is_escape_char(c):
                is_escaped = True

            # end the string and flush if we found an unescaped string token
            elif Tokens.is_string(c):
                flush()
                in_string = False

        # skip whitespace, flushing the buffer if necessary
        elif Tokens.is_whitespace(c):
            flush()

        # open parenthesis
        elif Tokens.is_open_paren(c):
            indent()

        # close parenthesis
        elif Tokens.is_close_paren(c):
            dedent()

        # quotes are special tokens
        elif Tokens.is_quote(c):
            flush()
            buf.append(c)
            flush()

        # mark strings
        elif Tokens.is_string(c):
            # mark us as being in a string, let the first case deal with rest
            buf.append(c)
            in_string = True

        # just a normal character
        else:
            buf.append(c)

    # do a final buffer flush to catch any remaining contents
    flush()

    # ensure all strings were correctly closed
    if in_string:
        raise ParserError("Unclosed string.")

    # check to see if we matched all closing parenthesis (first item is always
    # tokens list, and it never gets popped).
    if len(stack) > 1:
        raise CloseParenError("Too few closing parenthesis.")

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
            if source != " ":
                print parse(source)

            # give a new line if user entered nothing
            else:
                source = ""
                continue

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
            # print all other problems and clear source
            traceback.print_exc()
            source = ""
            prompt = standard_prompt
