import collections

import lang
import tokens

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

    @staticmethod
    def build(evaluate, env, args):
        '''
        Parses the given args according to the language specification and
        returns an ArgSpec object for them. Raises an error if parsing fails.
        '''

        # the ArgSpec we'll return after filling it
        spec = ArgSpec()

        # convert to a list for easy access/modification
        args = list(args)

        # consume the variadic arg first, if there is one
        if len(args) > 1:
            v = args[-1]
            vararg_symbol = args[-2]
            if isinstance(v, lang.Symbol) and v.value == tokens.VARIADIC_ARG:
                # store the vararg symbol, then remove its cruft from the list
                spec.variadic(vararg_symbol)
                del args[-2:]

        # consume optional args next
        while len(args) > 0 and lang.Cons.is_list(args[-1]):
            # get the next argument in the list
            opt_arg = args.pop()

            # make sure we got a symbol and an expression for the optional arg
            if len(opt_arg) != 2 or not isinstance(opt_arg.car, lang.Symbol):
                raise errors.WrongArgumentTypeError('optional arguments must ' +
                        'consist of a single symbol and a single expression')

            # evaluate the expression and store the result in the spec
            symbol = opt_arg.car
            expression = opt_arg.cdr.car
            spec.optional(symbol, evaluate(expression, env))

        # consume remaining required args
        for symbol in args:
            # optional args are no longer allowed
            if lang.Cons.is_list(symbol):
                raise errors.WrongArgumentTypeError('optional arguments must ' +
                        'come after required arguments and before any ' +
                        'variadic argument.')
            # deal with arguments that aren't symbols
            elif not isinstance(symbol, lang.Symbol):
                raise errors.WrongArgumentTypeError.build(symbol, lang.Symbol)

            spec.required(symbol)

        return spec

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
