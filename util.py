import collections
import threading

import errors
import tokens

def ensure_type(required_class, item, *rest):
    '''
    Raises a WrongArgumentTypeError if all the items aren't instances of the
    required class/classes tuple.
    '''

    if not isinstance(item, required_class):
        raise errors.WrongArgumentTypeError.build(item, required_class)

    for thing in rest:
        if not isinstance(thing, required_class):
            raise errors.WrongArgumentTypeError.build(thing, required_class)

def ensure_args(supplied_args, num_required=0, num_optional=0, is_variadic=False):
    '''
    Enforces the argument format specified by the keyword arguments.  This
    format is: required arguments first, optional arguments next, and a single
    optional variadic arg last.

    num_required defaults to 0, num_optional defaults to 0, and is_variadic
    defaults to  False.

    Raises an IncorrectArgumentCountError if the args don't match the spec.
    '''

    # get the various counts we need to determine if the number of args is good
    min_args = num_required
    max_args = float('inf') if is_variadic else num_required + num_optional

    # determine whether the arg spec was met by the supplied arg list
    num_supplied = len(supplied_args)
    if num_supplied < min_args or num_supplied > max_args:
        raise errors.IncorrectArgumentCountError.build(min_args, max_args,
                num_supplied, is_variadic=is_variadic)

def file_char_iter(f):
    '''Iterate over an open file one character at a time.'''
    for line in f:
        for c in line:
            yield c

def to_string(x):
    '''Convert an atom to a string as it appears in our language.'''

    if isinstance(x, bool):
        return tokens.TRUE if x else tokens.FALSE
    elif isinstance(x, basestring):
        # TODO: escape properly
        return tokens.STRING + x + tokens.STRING

    return unicode(x)

class ThreadSafeCounter:
    '''When called, returns increasing ints in order.'''

    def __init__(self, count=0):
        self.count = count
        self.lock = threading.Lock()

    def __call__(self):
        with self.lock:
            c = self.count
            self.count += 1
            return c
