from __future__ import unicode_literals

import collections

# the tokens module contains all the variables for matching the various tokens.
# also contains the tokenize function for generating a stream of tokens from
# some string input.

# data structure delimiters
LIST_START = '('
LIST_END = ')'
VECTOR_START = '['
VECTOR_END = ']'
MAP_START = '{'
MAP_END = '}'
SET_START = '|'
SET_END = '|'

DATA = frozenset([
    LIST_START,
    LIST_END,
    VECTOR_START,
    VECTOR_END,
    MAP_START,
    MAP_END,
    SET_START,
    SET_END,
])

# prefix syntax
QUOTE = "'"
QUASIQUOTE = '`'
UNQUOTE = '~'
UNQUOTE_SPLICING = '^'
KEYWORD = ':'

# postfix syntax
VARIADIC_ARG = '...'

WHITESPACE = frozenset([
    ' ',
    '\t',
    '\n',
    '\r',
    '\f',
    '\v',
    ',', # commas are whitespace, too!
])

ESCAPE_CHAR = '\\'
STRING_START = '"'
STRING_END = '"'
COMMENT = ';'

#
# functions
#

# general
FUNCTION = 'fn'
MACRO = 'macro'
MACRO_EXPAND = 'expand'
DEFINE = 'def'
COND = 'cond'
TYPE = 'type'
KEYWORD_LONG = 'keyword'

# boolean symbols
TRUE = '#t'
FALSE = '#f'

# repl
READ = 'read'
PARSE = 'parse'
EVAL = 'eval'
LOAD = 'load'

# expanded sugar
QUOTE_LONG = 'quote'
QUASIQUOTE_LONG = 'quasiquote'
UNQUOTE_LONG = 'unquote'
UNQUOTE_SPLICING_LONG = 'unquote-splicing'

# math
ADD = '+'
SUBTRACT = '-'
MULTIPLY = '*'
DIVIDE = '/'
MODULUS = '%'
POWER = 'pow'

# comparison
IS = 'is?'
EQUAL = '='
GREATER_THAN = '>'

# logic
AND = 'and'
OR = 'or'
NOT = 'not'

# cons
CONS = 'cons'
CAR = 'car'
CDR = 'cdr'

# used to de-sugar various syntactic elements
CONSUMERS = {
    QUOTE: QUOTE_LONG,
    UNQUOTE: UNQUOTE_LONG,
    QUASIQUOTE: QUASIQUOTE_LONG,
    UNQUOTE_SPLICING: UNQUOTE_SPLICING_LONG
}

# how we yield token data from the tokenize method
Token = collections.namedtuple('Token', ['value', 'line', 'column'])

def tokenize(source):
    '''
    Given a string source, returns a generator that reads it character by
    character and yields all the tokens in sequence as Token namedtuples of
    (value, line, column). Both line and column are one-based.  Newlines are not
    emitted.
    '''

    # buffer where uncommitted characters live
    buf = []

    # ugly hack to let us modify these within the flush function
    buf_column = [-1]
    buf_line = [-1]

    def flush(line=buf_line, column=buf_column):
        '''Returns the buffer contents as a string and clears the buffer.'''

        # get the contents of the buffer as a string
        result = Token(''.join(buf), buf_line[0], buf_column[0])

        # uses __delslice__ method of the list so we modify original buffer
        # and not the local copy.
        del buf[:]

        # reset the buffer's start character index
        buf_column[0] = -1

        # return the contents and metadata of the buffer
        return result

    # iterate over every character on every line in the source string
    for line_index, line in enumerate(source.splitlines()):

        # flush the buffer if it's non-empty
        if len(buf) > 0: yield flush()

        # set the new line index
        buf_line[0] = line_index + 1

        # there's no such thing as a multi-line token (besides a comment)
        assert len(buf) == 0

        # iterate over each character
        for column, c in enumerate(line):
            # collect all comment characters as one token
            if len(buf) > 0 and buf[0] == COMMENT:
                buf.append(c)
                continue

            # add the first comment character to the buffer
            elif c == COMMENT:
                # clear the buffer of non-comment contents
                if len(buf) > 0: yield flush()
                buf.append(c)

            # match escape characters, for having literal values in strings
            elif c == ESCAPE_CHAR:
                if len(buf) > 0: yield flush()
                yield Token(c, buf_line[0], column + 1)

            # add string delimiters
            elif c == STRING_START or c == STRING_END:
                if len(buf) > 0: yield flush()
                yield Token(c, buf_line[0], column + 1)

            # consume whitespace by collecting it in the buffer
            elif c in WHITESPACE:
                # flush non-whitespace chars before starting a whitespace buffer
                if len(buf) > 0 and buf[-1] not in WHITESPACE:
                    yield flush()

                # append the whitespace now that the buffer is empty
                buf.append(c)

            # various indentation structures
            elif c in DATA or c in CONSUMERS:
                if len(buf) > 0: yield flush()
                yield Token(c, buf_line[0], column + 1)

            # tokens that consume their next argument
            elif c in CONSUMERS:
                if len(buf) > 0: yield flush()
                yield Token(c, buf_line[0], column + 1)

            # a generic character, so collect it in the buffer
            else:
                # flush whitespace from the buffer before adding generic characters
                if len(buf) > 0 and buf[-1] in WHITESPACE:
                    yield flush()

                # append the char now that the buffer contains no whitespace
                buf.append(c)

            # if something got added to the buffer, mark where it started if we
            # haven't already done so.
            if len(buf) > 0 and buf_column[0] < 0:
                buf_column[0] = column + 1

    # do a final buffer flush to yield any remaining contents
    if len(buf) > 0: yield flush()
