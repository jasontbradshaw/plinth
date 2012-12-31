import collections
import threading

import errors

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

class ArgSpec:
    '''Holds all the arguments to a function in an easily accessible manner.'''

    # markers for the tuples yielded in __iter__
    REQUIRED = 'required'
    OPTIONAL = 'optional'
    VARIADIC = 'variadic'

    def __init__(self, required=None, optional=None, variadic=None):
        self.__required = [] if required is None else list(required)
        self.__optional = [] if optional is None else list(optional)
        self.__variadic = variadic

    def required(self, arg):
        self.__required.append(arg)
        return self

    def optional(self, arg, default):
        self.__optional.append((arg, default))
        return self

    def variadic(self, arg):
        self.__variadic = arg
        return self

    def validate(self, arg_values):
        '''
        Validate the given arg_values against this spec. Raises an
        IncorrectArgumentCountError if the values can't fill the spec.
        '''

        # get argument counts
        min_args = len(self.__required)
        is_variadic = self.__variadic is not None
        max_args = float('inf') if is_variadic else min_args + len(self.__optional)

        # enforce
        num_args = len(arg_values)
        if num_args < min_args or num_args > max_args:
            raise errors.IncorrectArgumentCountError.build(
                    min_args, max_args, num_args, is_variadic=is_variadic)

    def fill(self, arg_values, vararg_fun=tuple):
        '''
        Return an ordered dict of arguments mapped to arg_values. If the
        arguments can't be filled using the given arg_values, an
        IncorrectArgumentCountError is raised.

        If vararg_fun is specified, the variadic args are passed to it before
        being assigned to the variadic arg name in the returned dict. If left
        undefined, the tuple built-in function is used.
        '''

        # make sure we can meet the internal spec
        self.validate(arg_values)

        arg_dict = collections.OrderedDict()

        arg_stack = list(reversed(arg_values))

        # consume required args
        for arg in self.__required:
            arg_dict[arg] = arg_stack.pop()

        # fill optional args if they were supplied
        for arg, default in self.__optional:
            v = default if len(arg_stack) == 0 else arg_stack.pop()
            arg_dict[arg] = v

        # add the rest to the variadic arg
        if self.__variadic is not None:
            arg_dict[self.__variadic] = vararg_fun(a for a in reversed(arg_stack))

        return arg_dict

    def __iter__(self):
        '''
        Yields all args in sequece, preceded by a marker to signal what type of
        arg it is. Optional arguments are yielded as a tuple of name and
        default.
        '''

        for arg in self.__required:
            yield ArgSpec.REQUIRED, arg

        for arg_default in self.__optional:
            yield ArgSpec.OPTIONAL, arg_default

        if self.__variadic is not None:
            yield ArgSpec.VARIADIC, self.__variadic

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
