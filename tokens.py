# the tokens module contains all the variables for matching the various tokens.
# also contains the tokenize function for generating a stream of tokens from
# some string input.

#
# syntactic constructs
#

OPEN_PAREN = u'('
CLOSE_PAREN = u')'
WHITESPACE = frozenset([u' ', u'\t', u'\n', u'\r', u'\f', u'\v'])
ESCAPE_CHAR = u'\\'
STRING = u'"'
COMMENT = u';'
LINE_SEPARATORS = frozenset([u'\r', u'\n'])
VARIADIC_ARG = u'...'

#
# functions
#

# general
LAMBDA = u'lambda'
MACRO = u'macro'
MACRO_EXPAND = u'expand'
GENERATE_SYMBOL = u'gensym'
DEFINE = u'define'
COND = u'cond'
TYPE = u'type'

# boolean symbols
TRUE = u'#t'
FALSE = u'#f'

# repl
READ = u'read'
PARSE = u'parse'
EVAL = u'eval'
LOAD = u'load'

# quoting
QUOTE = u'quote'

# math
ADD = u'+'
SUBTRACT = u'-'
MULTIPLY = u'*'
DIVIDE = u'/'
MODULUS = u'%'
POWER = u'pow'

# comparison
IS = u'is?'
LISTP = u'list?'
EQUAL = u'='
GREATER_THAN = u'>'

# logic
AND = u'and'
OR = u'or'
NOT = u'not'

# cons
CONS = u'cons'
CAR = u'car'
CDR = u'cdr'

def tokenize(source):
    '''
    Given a string source, returns a generator that reads it character by
    character and yields all the tokens in sequence.
    '''

    # buffer where uncommitted characters live
    buf = []

    def flush():
        '''Returns the buffer contents as a string and clears the buffer.'''

        # get the contents of the buffer as a string
        result = u''.join(buf)

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
