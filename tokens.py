# the tokens module contains all the variables for matching the various tokens,
# as well as functions for determining whether a string is some token. also
# contains a function for generating a stream of tokens from some string input.

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

# repl
READ = "read"
EVAL = "eval"
PRINT = "print"

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

def is_open_paren(token):
    return token == OPEN_PAREN

def is_close_paren(token):
    return token == CLOSE_PAREN

def is_quote(token):
    return token == QUOTE

def is_unquote(token):
    return token == UNQUOTE

def is_quasiquote(token):
    return token == QUASIQUOTE

def is_whitespace(token):
    # return whether all the characters in the token are whitespace
    for c in token:
        if c not in WHITESPACE:
            return False
    return True

def is_escape_char(token):
    return token == ESCAPE_CHAR

def is_string(token):
    return token == STRING

def is_comment(token):
    return token == COMMENT

def tokenize(source):
    """
    Given a string source, returns a generator that reads it character by
    character and yields all the tokens in sequence.
    """

    # buffer where uncommitted characters live
    buf = []

    def flush():
        """Returns the buffer contents as a string and clears the buffer."""

        # get the contents of the buffer as a string
        result = ''.join(buf)

        # uses __delslice__ method of the list so we modify original buffer
        # and not the local copy.
        del buf[:]

        # return the contents of the buffer
        return result

    # iterate over every character in the source string
    for c in source:

        # match escape characters, for having literal values in strings
        if is_escape_char(c):
            if len(buf) > 0:
                yield flush()
            yield c

        # add string delimiters
        elif is_string(c):
            if len(buf) > 0:
                yield flush()
            yield c

        # consume whitespace by collecting it in the buffer
        elif is_whitespace(c):
            # flush out other characters before starting a whitespace buffer
            if len(buf) != 0 and not is_whitespace(buf[-1]):
                yield flush()

            # append the whitespace now that the buffer contains no other
            # characters.
            buf.append(c)

        # open parenthesis
        elif is_open_paren(c):
            if len(buf) > 0:
                yield flush()
            yield c

        # close parenthesis
        elif is_close_paren(c):
            if len(buf) > 0:
                yield flush()
            yield c

        # quotes, unquotes, and quasiquotes
        elif c in DESUGAR:
            if len(buf) > 0:
                yield flush()
            yield c

        # just a normal character, so collect it in the buffer
        else:
            # flush whitespace from the buffer before adding normal
            # characters.
            if len(buf) > 0 and is_whitespace(buf[-1]):
                yield flush()

            # append the character now that the buffer contains no
            # whitespace.
            buf.append(c)

    # do a final buffer flush to yield any remaining contents
    if len(buf) > 0:
        yield flush()
