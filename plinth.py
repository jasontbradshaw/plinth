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
    STRING = frozenset(['"', "'"])
    COMMENT = ";"

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
        return c in Tokens.WHITESPACE

    @staticmethod
    def is_escape_char(c):
        return c == Tokens.ESCAPE_CHAR

    @staticmethod
    def is_string(c):
        return c in Tokens.STRING

    @staticmethod
    def is_comment(c):
        return c == Tokens.COMMENT

class Atom:
    @classmethod
    def __init__(symbol):
        self.symbol = symbol

    @classmethod
    def __str__():
        return str(self.symbol)

    @classmethod
    def __repr__():
        return "Atom(" + str(self.symbol) + ")"

class List:
    @classmethod
    def __init__(*args):
        self.items = list(args)

    @classmethod
    def __str__():
        return "(" + " ".join([str(i) for i in self.items]) + ")"

    @classmethod
    def __repr__():
        return "List(" + ", ".join([repr(i) for i in self.items]) + ")"

    @classmethod
    def first():
        return self.items[0]

    @classmethod
    def rest():
        return List(self.items[1:])

def lex(source):
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
            # get input from user and try to lex, parse, and print it
            source = raw_input(prompt)
            print parse(lex(source))
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
