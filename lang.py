import collections
import inspect
import itertools

import argspec
import errors
import parser
import tokens
import util

class Atom:
    '''The base class all custom language constructs inherit from.'''

    def __init__(self, value):
        self.value = value
        self.metadata = collections.OrderedDict()

    def __setitem__(self, meta_attr, value):
        '''
        Set a metadata attribute, but only allow setting it once. Metadata keys
        and values may only be strings.
        '''
        util.ensure_type(basestring, meta_attr, value)
        if meta_attr not in self.metadata:
            self.metadata[meta_attr] = value

    def __getitem__(self, meta_attr):
        '''Get a metadata attribute string.'''
        return self.metadata[meta_attr]

    def __contains__(self, meta_attr):
        '''See if the given attribute is in our metadata.'''
        return meta_attr in self.metadata

    def __str__(self):
        return unicode(self.value)

    def __repr__(self):
        v = self.value if hasattr(self, 'value') else None
        return self.__class__.__name__ + '(' + repr(self.value) + ')'

    def __eq__(self, other):
        return isinstance(other, self.__class__) and self.value == other.value

    @staticmethod
    def to_atom(token):
        '''
        Takes the given string token and returns the class representing that
        string in its most natural form (int, str, Symbol, etc.).
        '''

        # number types: complex are invalid floats, and floats are invalid ints
        try:
            # automatically returns 'long' if necessary
            return int(token)
        except:
            try:
                return float(token)
            except:
                try:
                    return complex(token)
                except:
                    pass

        # boolean
        if token.lower() == tokens.TRUE:
            return True
        elif token.lower() == tokens.FALSE:
            return False

        # string (strips wrapping string tokens)
        elif token.startswith(tokens.STRING) and token.endswith(tokens.STRING):
            return unicode(token[len(tokens.STRING):-len(tokens.STRING)])

        # the base case for all tokens is a symbol
        return Symbol(token)

class Cons:
    '''Represents a pair of elements.'''

    def __init__(self, car, cdr):
        self.car = car
        self.cdr = cdr

    @staticmethod
    def build(*items):
        '''Build a Cons sequence recursively from a nested list structure.'''

        result = NIL

        for item in reversed(items):
            if isinstance(item, (list, tuple)):
                result = Cons(Cons.build(*item), result)
            else:
                result = Cons(item, result)

        return result

    @staticmethod
    def is_list(e):
        '''Returns whether an element is a list or not (nil is a list).'''

        # nil is a list
        if e is NIL:
            return True

        # non-cons can't be lists
        if not isinstance(e, Cons):
            return False

        # only cons that len() works on are lists (throws an exception otherwise)
        try:
            return bool(len(e)) or True
        except errors.WrongArgumentTypeError:
            return False

    def __str_helper(self, item):
        # nil has no contents
        if item is NIL:
            return u''

        if item.cdr is NIL:
            return util.to_string(item.car)

        if not isinstance(item.cdr, Cons):
            return util.to_string(item.car) + u' . ' + util.to_string(item.cdr)

        return util.to_string(item.car) + u' ' + self.__str_helper(item.cdr)

    def __str__(self):
        return u'(' + self.__str_helper(self) + ')'

    def __repr__(self):
        return (self.__class__.__name__ +
               u'(' + repr(self.car) + ', ' + repr(self.cdr) + ')')

    def __len__(self):
        # nil is the empty list
        if self is NIL:
            return 0
        return sum(1 for i in self)

    def __eq__(self, other):
        '''Compare recursively to another iterable.'''
        for si, oi in itertools.izip(self, other):
            if si != oi:
                return False
        return True

    def __iter__(self):
        '''
        Yield all the items in this cons sequence in turn. If the final item is
        NIL, it isn't yielded. If the final item is non-NIL, raises an error.
        '''

        item = self
        while 1:
            if item is NIL:
                # stop if the item is NIL
                return
            elif isinstance(item.cdr, Cons):
                yield item.car
                item = item.cdr
            else:
                raise errors.WrongArgumentTypeError('not a proper list: ' +
                        unicode(self))

    def __getitem__(self, index):
        '''Allow indexing into the list.'''
        if isinstance(index, slice):
            if index.stop < len(self):
                raise IndexError(self.__class__.__name__.lower() +
                        ' does not support slice end indexes')
            s = self
            for i in xrange(index.start):
                if s is NIL:
                    break
                s = s.cdr
            return s
        else:
            index += len(self) if index < 0 else 0
            for i, item in enumerate(self):
                if i == index:
                    return item
            raise IndexError(self.__class__.__name__.lower() +
                    ' index out of range')

# the singleton 'nil' value, an empty Cons: we define it here so Cons can use it
NIL = Cons(None, None)

class Symbol(Atom):
    '''Symbols store other values, and evaluate to their stored values.'''

    def __init__(self, value):
        '''Symbols are stored and looked up by their string names.'''
        Atom.__init__(self, unicode(value))

    def __hash__(self):
        '''Important since we use Symbols as keys in Environments.'''
        return hash(self.value)

class Environment(dict):
    '''
    A scope that holds variables mapped to their values. Allows us to easily
    package execution state.
    '''

    def __init__(self, parent):
        '''
        Create an environment with the given parent and any number of predefined
        variables.
        '''

        # None means no parent, otherwise must be an environment
        assert parent is None or isinstance(parent, Environment)

        # the environment that contains this environment
        self.parent = parent

        dict.__init__(self)

    def __getitem__(self, symbol):
        '''
        Attempts to locate a given symbol by name in the current environment,
        then in every environment up the parent chain if the symbol could not be
        found. If the symbol is not bound within the parent chain, raises an
        error.
        '''

        if symbol in self:
            return self.get(symbol)
        elif self.parent is not None:
            return self.parent[symbol]

        raise errors.SymbolNotFoundError.build(symbol)

class Evaluator:
    '''
    A class that controls the evaluation of a Callable. When called with the
    parent environment, argspec, body, and arguments of some target function,
    returns a generator that yields (and accepts) results until a return value
    is obtained.
    '''

    # actions to be taken for yielded items
    EVALUATE = 'evaluate'
    RETURN = 'return'

    @staticmethod
    def build_evaluate(sexp, env=None):
        '''If None, env defaults to the parent frame's calling environment.'''
        return (Evaluator.EVALUATE, sexp, env)

    @staticmethod
    def build_return(sexp):
        return (Evaluator.RETURN, sexp, None)

    def __call__(self, parent, spec, body, args):
        '''
        Return the generator result of the user-defined evaluate method after
        validating the arguments.
        '''
        spec.validate(args)
        return self.evaluate(parent, spec, body, args)

    def evaluate(self, parent, spec, body, args):
        '''
        Takes a parent environment, an argspec, a body, and arguments, then
        yields actions that need to be taken before a result can be obtained.
        The EVALUATE action specifies that the yielded result should be
        evaluated, possibly in some environment, then returned to the generator.
        The RETURN action specifies that the result is final and needs no more
        evaluation. Arguments have already been validated by the time this
        method receives them.
        '''
        raise NotImplementedError('evaluate must be implemented as a generator.')

class UserFunctionEvaluator(Evaluator):
    '''Handles user-defined function evaluation.'''

    def evaluate(self, parent, spec, body, args):
        # evaluate all arguments left-to-right
        evaluated_args = []
        for arg in args:
            evaluated_arg = yield Evaluator.build_evaluate(arg)
            evaluated_args.append(evaluated_arg)

        # create a new environment filled with the evaluated arguments
        env = Environment(parent)
        env.update(spec.fill(evaluated_args))

        # get the result, then return it
        result = yield Evaluator.build_evaluate(body, env)
        yield Evaluator.build_return(result)

class UserMacroEvaluator(Evaluator):
    '''Handles user-defined macro evaluation.'''

    def evaluate(self, parent, spec, body, args):
        # create a new environment filled with the unevaluated arguments
        env = Environment(parent)
        env.update(spec.fill(args))

        # evaluate the body to get the interpolated macro body
        macro_body = yield Evaluator.build_evaluate(body, env)

        # evaluate the macro body in the original parent to get the full result
        result = yield Evaluator.build_evaluate(macro_body, parent)
        yield Evaluator.build_return(result)

class PythonMethodEvaluator(Evaluator):
    '''Evaluates Python methods as if they were native functions.'''

    def evaluate(self, parent, spec, body, args):
        # evaluate all arguments left-to-right
        evaluated_args = []
        for arg in args:
            evaluated_arg = yield Evaluator.build_evaluate(arg)
            evaluated_args.append(evaluated_arg)

        # return the result of calling the method on the evaluated arguments
        yield Evaluator.build_return(body(*evaluated_args))

class QuoteEvaluator(Evaluator):
    '''Return the only argument unevaluated.'''
    def evaluate(self, parent, spec, body, args):
        yield Evaluator.build_return(args[0])

class QuasiquoteEvaluator(Evaluator):
    '''Selectively evaluate a quoted S-expression.'''

    # symbols for the special tokens
    QUASIQUOTE_SYMBOL = Symbol(tokens.QUASIQUOTE_LONG)
    UNQUOTE_SYMBOL = Symbol(tokens.UNQUOTE_LONG)
    UNQUOTE_SPLICING_SYMBOL = Symbol(tokens.UNQUOTE_SPLICING_LONG)

    # symbols that cause special evaluation to happen
    SYMBOLS = frozenset((
        QUASIQUOTE_SYMBOL,
        UNQUOTE_SYMBOL,
        UNQUOTE_SPLICING_SYMBOL
    ))

    def evaluate(self, parent, spec, body, args):
        raise NotImplementedError('quasiquote is not yet implemented!')

class CallableEvaluator(Evaluator):
    '''Build a new user-defined callable.'''

    def __init__(self, callable_class):
        # the type of Callable object to create
        self.callable_class = callable_class

    def evaluate(self, parent, spec, body, args):
        # get the child callable's arguments and body
        new_args = args[0]
        new_body = args[1]

        parsed_args = argspec.ArgSpec.parse(new_args)

        # evaluate all the optional argument's defaults
        optional_arg_values = {}
        for symbol, sexp in parsed_args[argspec.ArgSpec.OPTIONAL]:
            value = yield Evaluator.build_evaluate(sexp)
            optional_arg_values[symbol] = value

        # build a new function and return it
        new_spec = argspec.ArgSpec.build(parsed_args, optional_arg_values)
        yield Evaluator.build_return(
                self.callable_class(parent, new_spec, new_body))

class LambdaEvaluator(CallableEvaluator):
    '''Build a new user-defined function.'''
    def __init__(self):
        CallableEvaluator.__init__(self, Function)

class MacroEvaluator(CallableEvaluator):
    '''Build a new user-defined macro.'''
    def __init__(self):
        CallableEvaluator.__init__(self, Macro)

class ExpandEvaluator(Evaluator):
    '''Expand a user-defined macro and return its S-expression.'''
    def evaluate(self, parent, spec, body, args):
        raise NotImplementedError()

class DefineEvaluator(Evaluator):
    '''Define a symbol in an environment and return the defined value.'''
    def evaluate(self, parent, spec, body, args):
        symbol = args[0]

        util.ensure_type(Symbol, symbol)

        value = yield Evaluator.build_evaluate(args[1])

        # define the symbol in the parent environment
        parent[symbol] = value

        # return the value to allow for chained definitions
        yield Evaluator.build_return(value)

class CondEvaluator(Evaluator):
    '''
    Evaluate a conditional expression. The arguments are lists of tuples where
    the first item is a conditional and the second is the result to evaluate and
    return if the conditional evaluates to True. If no conditional expression
    evaluates to True, returns nil.
    '''

    def evaluate(self, parent, spec, body, args):
        for tup in args:
            # if tup is not an iterable, len() raises an error for us
            if len(tup) != 2:
                # make sure each is a list of exactly two expressions
                s = 'expected 2 expressions, got ' + str(len(tup))
                raise errors.IncorrectArgumentCountError(s)

            # first and second list items are condition and result
            condition = tup[0]
            result = tup[1]

            # evaluate and return the result if the condition passes
            condition_result = yield Evaluator.build_evaluate(condition)
            if condition_result:
                result = yield Evaluator.build_evaluate(result)
                yield Evaluator.build_return(result)

        # if no result is returned, return nil
        yield Evaluator.build_return(NIL)

class AndEvaluator(Evaluator):
    '''
    Evaluate Boolean 'and'. Returns False if any of the items evaluates to False,
    otherwise short-circuits and returns the final item.
    '''
    def evaluate(self, parent, spec, body, args):
        last_item = None
        for item in args:
            last_item = yield Evaluator.build_evaluate(item)
            if last_item is False:
                break

        yield Evaluator.build_return(last_item)

class OrEvaluator(Evaluator):
    '''
    Evaluate Boolean 'or'. Returns the first argument that doesn't evaluate
    to False, otherwise returns False.
    '''
    def evaluate(self, parent, spec, body, args):
        last_item = None
        for item in args:
            last_item = yield Evaluator.build_evaluate(item)
            if last_item is not False:
                break

        yield Evaluator.build_return(last_item)

class EvalEvaluator(Evaluator):
    '''Evaluate an S-expression and return the result.'''
    def evaluate(self, parent, spec, body, args):
        sexp = yield Evaluator.build_evaluate(args[0])
        result = yield Evaluator.build_evaluate(sexp)
        yield Evaluator.build_return(result)

class LoadEvaluator(Evaluator):
    '''
    Interpret a file as a list of S-expressions and evaluate them into the
    current environment.
    '''
    def evaluate(self, parent, spec, body, args):
        fname = args[0]
        util.ensure_type(basestring, fname)

        # evaluate every expression in the file in sequence, top to bottom
        with open(os.path.abspath(fname), 'r') as f:
            for sexp in parser.parse(tokens.tokenize(util.file_char_iter(f))):
                yield Evaluator.build_evaluate(sexp)

        # return that we were successful
        yield Evaluator.build_return(True)

class Callable(Atom):
    '''A base class for object in our language that can be 'called'.'''

    def __init__(self, evaluator_class):
        Atom.__init__(self, None)

        # make sure these are present for get_evaluator()
        self.parent = None
        self.body = None
        self.spec = None

        # build our evaluator
        self.evaluator = evaluator_class()

    def __call__(self, args):
        '''Return an evaluator generator specific to this object instance.'''
        return self.evaluator(self.parent, self.spec, self.body, args)

    def __eq__(self, other):
        '''Callables are only equal if the other callable is this callable.'''
        return other is self

    @staticmethod
    def build_string(kind, spec, name=None):
        '''
        Build a string to display a typical callable in the interpreter. kind is
        the object type to use, and spec is the ArgSpec object to use to build
        the arguments list.
        '''

        s = u'<' + unicode(kind)

        # set the function name if possible
        if name is not None:
            s += ' ' + unicode(name)

        s += ' ('

        # compose a list of all arg symbols
        a = []
        for arg_type, arg in spec:
            if arg_type == argspec.ArgSpec.REQUIRED:
                a.append(unicode(arg))
            elif arg_type == argspec.ArgSpec.OPTIONAL:
                arg, default = arg
                a.append('(' + unicode(arg) + ' ' + util.to_string(default) + ')')
            elif arg_type == argspec.ArgSpec.VARIADIC:
                a.append(unicode(arg))
                a.append(tokens.VARIADIC_ARG)
            else:
                raise ValueError('Unhandled arg type: ' + arg_type)

        s += ' '.join(a)
        s += ')>'

        return s

class Function(Callable):
    '''
    Represents a function in our language. Functions take some number of
    arguments, have a body, and are evaluated in some context.
    '''

    def __init__(self, parent, spec, body):
        Callable.__init__(self, UserFunctionEvaluator)

        self.parent = parent
        self.spec = spec
        self.body = body

    def __str__(self):
        return Callable.build_string('function', self.spec,
                self['name'] if 'name' in self else None)

class Macro(Callable):
    '''
    A code-rewriting construct. A macro takes code and returns (expands) code
    dynamically at runtime. Arguments aren't evaluated before being inserted
    into the macro body.
    '''

    def __init__(self, parent, spec, body):
        Callable.__init__(self, UserMacroEvaluator)

        self.parent = parent
        self.spec = spec
        self.body = body

    def __str__(self):
        return Callable.build_string('macro', self.spec,
                self['name'] if 'name' in self else None)

class PrimitiveFunction(Function):
    '''
    Represents a base-level function that can't be broken down into an AST. One
    of the constructs that enables the language to function.
    '''

    def __init__(self, parent, method, evaluator_class):
        '''
        Create a primitive function that works much like a normal function,
        except that the body is a Python method and evaluation is controlled by
        the specified class.
        '''

        Callable.__init__(self, evaluator_class)

        self.parent = parent
        self.spec = argspec.ArgSpec()

        # body is a Python method
        self.body = method

        # parse the arg spec (no support for keyword args)
        args, vararg, _, defaults = inspect.getargspec(method)
        defaults = () if defaults is None else defaults

        # get the counts for our respective arg types
        num_required = len(args) - len(defaults)
        num_optional = len(args) - num_required

        # add required arguments
        for arg in args[:num_required]:
            self.spec.required(arg)

        # add optional arguments, which come after required ones
        for arg, default in itertools.izip(args[num_required:], defaults):
            self.spec.optional(arg, default)

        # add the variadic arg if it exists
        if vararg is not None:
            self.spec.variadic(vararg)

    def __str__(self):
        return Callable.build_string('primitive-function', self.spec,
                self['name'] if 'name' in self else None)
