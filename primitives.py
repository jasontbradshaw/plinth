import numbers

import lang
import tokens
import util

# used to get generate guaranteed-unique symbol names
GENSYM_COUNTER = util.ThreadSafeCounter()

def add(a, b, *rest):
    '''Adds the all the given numbers together.'''

    util.ensure_type(numbers.Number, a, b)

    # add all the arguments together while checking type
    total = a + b
    for n in rest:
        util.ensure_type(numbers.Number, n)
        total += n

    return total

def sub(a, b, *rest):
    '''Subtracts the given numbers in sequence.'''

    util.ensure_type(numbers.Number, a, b)

    # subtract all the arguments in sequence while checking type
    difference = a - b
    for n in rest:
        util.ensure_type(numbers.Number, n)
        difference -= n

    return difference

def mul(a, b, *rest):
    '''Multiplies all the given numbers together.'''

    util.ensure_type(numbers.Number, a, b)

    # multiply all the arguments together while checking type
    product = a * b
    for n in rest:
        # stop multiplying if the product ever goes to zero
        if product == 0:
            break

        util.ensure_type(numbers.Number, n)
        product *= n

    return product

def div(a, b, *rest):
    '''Divides the given numbers in sequence.'''

    util.ensure_type(numbers.Number, a, b)

    # divide all the arguments in sequence while checking type
    quotient = a / b
    for n in rest:
        util.ensure_type(numbers.Number, n)
        quotient /= n

    return quotient

def mod(a, b):
    '''Takes the modulus of a number.'''
    util.ensure_type(numbers.Number, a, b)
    return a % b

def power(a, b):
    '''Raises a to the power of b.'''
    util.ensure_type(numbers.Number, a, b)
    return a ** b

def type_(e):
    '''Returns the type of an element as a string. Returns 'nil' for NIL.'''

    if isinstance(e, lang.Symbol):
        return u'symbol'
    elif isinstance(e, basestring):
        return u'string'
    elif isinstance(e, bool):
        return u'boolean'
    elif isinstance(e, (int, long)):
        return u'integer'
    elif isinstance(e, float):
        return u'float'
    elif isinstance(e, complex):
        return u'complex'
    elif e is lang.NIL:
        return u'nil'
    elif isinstance(e, lang.Cons):
        return u'cons'
    elif isinstance(e, lang.Function):
        return u'function'
    elif isinstance(e, lang.Macro):
        return u'macro'

    # shouldn't ever get this far
    raise errors.WrongArgumentTypeError('unsupported type: ' +
            e.__class__.__name__.lower())

def is_(a, b):
    '''Returns true if the two items refer to the same object in memory.'''
    return a is b

def equal(a, b):
    '''
    Returns true if two constructs are congruent. For example, numbers are
    compared mathematically, cons are compared by structure and equivalent
    contents, etc.
    '''

    # the same item is equal to itself
    if a is b:
        return True

    # things can't be equal if they're not the same class
    elif not (isinstance(a, b.__class__) and isinstance(b, a.__class__)):
        return False

    # compare everything else by value (numbers, Cons, symbols, etc.)
    return a == b

def gt(a, b):
    '''Compare two numbers using '>'.'''
    util.ensure_type(numbers.Number, a, b)
    return (a > b)

def not_(a):
    '''
    Returns the opposite boolean of that passed in. All things that aren't #f
    are #t, so we return whether a is False.
    '''

    return a is False

def cons(a, b):
    '''Pair two items.'''
    return lang.Cons(a, b)

def car(e):
    '''Return the first element of a pair.'''
    util.ensure_type(lang.Cons, e)

    # nil isn't allowed to be indexed into, since it has no car or cdr
    if e is lang.NIL:
        raise errors.WrongArgumentTypeError('wrong argument type for car: ' +
                'expected pair, got ' + str(e))

    return e.car

def cdr(e):
    '''Return the second element of a pair.'''
    util.ensure_type(lang.Cons, e)

    if e is lang.NIL:
        raise errors.WrongArgumentTypeError('wrong argument type for cdr: ' +
                'expected pair, got ' + str(e))

    return e.cdr

def read(prompt):
    '''Print the prompt, read input from stdin, and return it as a string.'''
    util.ensure_type(basestring, prompt)
    return unicode(raw_input(prompt))

def parse_(s):
    '''Parse a string into a list of the S-expressions it describes.'''
    util.ensure_type(basestring, s)
    return lang.Cons.build(*parse(tokens.tokenize(s)))

def gensym(prefix='SYM__'):
    '''
    Generate a unique symbol with the given prefix in its name. Generated
    symbols have names that contain syntax elements, and hence can't be entered
    via the reader.
    '''
    util.ensure_type(basestring, prefix)
    return lang.Symbol(prefix + tokens.OPEN_PAREN + str(GENSYM_COUNTER()) +
            tokens.CLOSE_PAREN)

# these functions serve as markers for whether the function being called is
# special. we check to see if the function for the symbol is one of these
# functions, and if so we evaluate it in whatever way it requires. this allows
# the user to define new symbols that point to these functions, but still have
# the functions work in the same way.
quote = lang.PrimitiveFunction(lambda e: _, name=tokens.QUOTE_LONG)
unquote = lang.PrimitiveFunction(lambda e: _, name=tokens.UNQUOTE_LONG)
quasiquote = lang.PrimitiveFunction(lambda e: _, name=tokens.QUASIQUOTE_LONG)
lambda_ = lang.PrimitiveFunction(lambda args, body: _, name=tokens.LAMBDA)
macro = lang.PrimitiveFunction(lambda args, body: _, name=tokens.MACRO)
expand = lang.PrimitiveFunction(lambda macro, *args: _, name=tokens.MACRO_EXPAND)
define = lang.PrimitiveFunction(lambda symbol, value: _, name=tokens.DEFINE)
cond = lang.PrimitiveFunction(lambda *e: _, name=tokens.COND)
and_ = lang.PrimitiveFunction(lambda a, b, *rest: _, name=tokens.AND)
or_ = lang.PrimitiveFunction(lambda a, b, *rest: _, name=tokens.OR)
eval_ = lang.PrimitiveFunction(lambda sexp: _, name=tokens.EVAL)
load = lang.PrimitiveFunction(lambda fname: _, name=tokens.LOAD)
