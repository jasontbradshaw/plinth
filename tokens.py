# the tokens module contains all the variables for matching the various tokens.
# also contains the tokenize function for generating a stream of tokens from
# some string input.

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
LINE_SEPARATORS = frozenset(["\r", "\n"])
VARARG = "..."

# true/false/nil
TRUE = "#t"
FALSE = "#f"
NIL = "nil"

# repl
READ = "read"
PARSE = "parse"
EVAL = "eval"
PRINT = "print"
LOAD = "load"

# special functions
QUOTE_LONG = "quote"
QUASIQUOTE_LONG = "quasiquote"
UNQUOTE_LONG = "unquote"
LAMBDA = "lambda"
DEFINE = "define"
COND = "cond"

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
GREATER_THAN_EQUAL = ">="
LESS_THAN = "<"
LESS_THAN_EQUAL = "<="

# logic
AND = "and"
OR = "or"
NOT = "not"

# type predicates
BOOLEANP = "boolean?"
CONSP = "cons?"
LISTP = "list?"
SYMBOLP = "symbol?"
STRINGP = "string?"
NUMBERP = "number?"
INTEGERP = "integer?"
FLOATP = "float?"
COMPLEXP = "complex?"
FUNCTIONP = "function?"

# cons
CONS = "cons"
CAR = "car"
CDR = "cdr"

# used to de-sugar various syntactic elements
SUGAR = {
    QUOTE: QUOTE_LONG,
    UNQUOTE: UNQUOTE_LONG,
    QUASIQUOTE: QUASIQUOTE_LONG
}

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

        # collect all comment characters as one token
        if len(buf) > 0 and buf[0] == COMMENT:
            # end the comment once a line separator is encountered
            if c not in LINE_SEPARATORS:
                buf.append(c)
                continue

            # yield the comment as one token, add the character to the new buf
            yield flush()
            buf.append(c)

        # add the first comment character to the buffer
        elif c == COMMENT:
            # clear the buffer of non-comment contents
            if len(buf) > 0:
                yield flush()

            buf.append(c)

        # match escape characters, for having literal values in strings
        elif c == ESCAPE_CHAR:
            if len(buf) > 0:
                yield flush()
            yield c

        # add string delimiters
        elif c == STRING:
            if len(buf) > 0:
                yield flush()
            yield c

        # consume whitespace by collecting it in the buffer
        elif c in WHITESPACE:
            # flush out other characters before starting a whitespace buffer
            if len(buf) != 0 and buf[-1] not in WHITESPACE:
                yield flush()

            # append the whitespace now that the buffer contains no other chars
            buf.append(c)

        # open parenthesis
        elif c == OPEN_PAREN:
            if len(buf) > 0:
                yield flush()
            yield c

        # close parenthesis
        elif c == CLOSE_PAREN:
            if len(buf) > 0:
                yield flush()
            yield c

        # quotes, unquotes, and quasiquotes
        elif c in SUGAR:
            if len(buf) > 0:
                yield flush()
            yield c

        # just a normal character, so collect it in the buffer
        else:
            # flush whitespace from the buffer before adding normal characters
            if len(buf) > 0 and buf[-1] in WHITESPACE:
                yield flush()

            # append the character now that the buffer contains no
            # whitespace.
            buf.append(c)

    # do a final buffer flush to yield any remaining contents
    if len(buf) > 0:
        yield flush()
