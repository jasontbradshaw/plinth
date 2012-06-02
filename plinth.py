#!/usr/bin/env python

import math
import numbers
import os

import errors
import lang
import tokens
import util

# used to get generate guaranteed-unique symbol names
GENSYM_COUNTER = util.ThreadSafeCounter()

def add(a, b, *rest):
    """Adds the all the given numbers together."""

    util.ensure_type(numbers.Number, a, b)

    # add all the arguments together while checking type
    total = a + b
    for n in rest:
        util.ensure_type(numbers.Number, n)
        total += n

    return total

def sub(a, b, *rest):
    """Subtracts the given numbers in sequence."""

    util.ensure_type(numbers.Number, a, b)

    # subtract all the arguments in sequence while checking type
    difference = a - b
    for n in rest:
        util.ensure_type(numbers.Number, n)
        difference -= n

    return difference

def mul(a, b, *rest):
    """Multiplies all the given numbers together."""

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
    """Divides the given numbers in sequence."""

    util.ensure_type(numbers.Number, a, b)

    # divide all the arguments in sequence while checking type
    quotient = a / b
    for n in rest:
        util.ensure_type(numbers.Number, n)
        quotient /= n

    return quotient

def power(a, b):
    """Raises a to the power of b."""
    util.ensure_type(numbers.Number, a, b)
    return a ** b

def sin(a):
    """Takes the sine of a."""
    util.ensure_type(numbers.Number, a)
    return math.sin(a)

def cos(a):
    """Takes the cosine of a."""
    util.ensure_type(numbers.Number, a)
    return math.cos(a)

def tan(a):
    """Takes the tangent of a."""
    util.ensure_type(numbers.Number, a)
    return math.tan(a)

def atan(a):
    """Takes the arctangent of a."""
    util.ensure_type(numbers.Number, a)
    return math.atan(a)

def atan2(a):
    """Takes the second arctangent of a."""
    util.ensure_type(numbers.Number, a)
    return math.atan2(a)

def type_(e):
    """Returns the type of an element as a string. Returns 'nil' for NIL."""

    if isinstance(e, lang.Symbol):
        return lang.String("symbol")
    elif isinstance(e, lang.String):
        return lang.String("string")
    elif isinstance(e, lang.Boolean):
        return lang.String("boolean")
    elif isinstance(e, (int, long)):
        return lang.String("integer")
    elif isinstance(e, float):
        return lang.String("float")
    elif isinstance(e, complex):
        return lang.String("complex")
    elif e is lang.NIL:
        return lang.String("nil")
    elif isinstance(e, lang.Cons):
        return lang.String("cons")
    elif isinstance(e, lang.Function):
        return lang.String("function")
    elif isinstance(e, lang.Macro):
        return lang.String("macro")

    # shouldn't ever get this far
    raise errors.WrongArgumentTypeError("unsupported type: " +
            e.__class__.__name__.lower())

def is_(a, b):
    """Returns true if the two items refer to the same object in memory."""
    return lang.Boolean.build(a is b)

def equal(a, b):
    """
    Returns true if two constructs are congruent. For example, numbers are
    compared mathematically, cons are compared by structure and equivalent
    contents, etc.
    """

    # the same item is equal to itself
    if a is b:
        return lang.TRUE

    # things can't be equal if they're not the same class
    elif not (isinstance(a, b.__class__) and isinstance(b, a.__class__)):
        return lang.FALSE

    # compare everything else by value (numbers, Cons, symbols, etc.)
    return lang.Boolean.build(a == b)

def gt(a, b):
    """Compare two numbers using '>'."""
    util.ensure_type(numbers.Number, a, b)
    return lang.Boolean.build(a > b)

def gte(a, b):
    """Compare two numbers using '>='."""
    util.ensure_type(numbers.Number, a, b)
    return lang.Boolean.build(a >= b)

def lt(a, b):
    """Compare two numbers using '<'."""
    util.ensure_type(numbers.Number, a, b)
    return lang.Boolean.build(a < b)

def lte(a, b):
    """Compare two numbers using '<='."""
    util.ensure_type(numbers.Number, a, b)
    return lang.Boolean.build(a <= b)

def not_(a):
    """
    Returns the opposite boolean of that passed in. All things that aren't #f
    are #t, so we return whether a is False.
    """

    return lang.Boolean.build(a is lang.FALSE)

def cons(a, b):
    """Pair two items."""
    return lang.Cons(a, b)

def car(e):
    """Return the first element of a pair."""
    util.ensure_type(lang.Cons, e)

    # nil isn't allowed to be indexed into, since it has no car or cdr
    if e is lang.NIL:
        raise errors.WrongArgumentTypeError("wrong argument type for car: " +
                "expected pair, got " + str(e))

    return e.car

def cdr(e):
    """Return the second element of a pair."""
    util.ensure_type(lang.Cons, e)

    if e is lang.NIL:
        raise errors.WrongArgumentTypeError("wrong argument type for cdr: " +
                "expected pair, got " + str(e))

    return e.cdr

def read(prompt):
    """Print the prompt, read input from stdin, and return it as a string."""
    util.ensure_type(basestring, prompt)
    return String(raw_input(prompt))

def parse_(s):
    """Parse a string into a list of the S-expressions it describes."""
    util.ensure_type(basestring, s)
    return lang.Cons.build(*parse(tokens.tokenize(s)))

def gensym(prefix):
    """
    Generate a unique symbol with the given prefix in its name. Generated
    symbols have names that contain syntax elements, and hence can't be entered
    via the reader.
    """
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

# the base environment for the interpreter
GLOBAL_ENV = lang.Environment(None)

# functions that need special treatment during evaluation
GLOBAL_ENV[lang.Symbol(tokens.QUOTE_LONG)] = quote
GLOBAL_ENV[lang.Symbol(tokens.QUASIQUOTE_LONG)] = quasiquote
GLOBAL_ENV[lang.Symbol(tokens.LAMBDA)] = lambda_
GLOBAL_ENV[lang.Symbol(tokens.MACRO)] = macro
GLOBAL_ENV[lang.Symbol(tokens.MACRO_EXPAND)] = expand
GLOBAL_ENV[lang.Symbol(tokens.DEFINE)] = define
GLOBAL_ENV[lang.Symbol(tokens.COND)] = cond
GLOBAL_ENV[lang.Symbol(tokens.AND)] = and_
GLOBAL_ENV[lang.Symbol(tokens.OR)] = or_
GLOBAL_ENV[lang.Symbol(tokens.EVAL)] = eval_
GLOBAL_ENV[lang.Symbol(tokens.LOAD)] = load

# adds a new primitive function to the global environment
ap = lambda t, f: GLOBAL_ENV.put(lang.Symbol(t), lang.PrimitiveFunction(f, t))

# repl
ap(tokens.READ, read)
ap(tokens.PARSE, parse_)

# logical
ap(tokens.NOT, not_)

# math
ap(tokens.ADD, add)
ap(tokens.SUBTRACT, sub)
ap(tokens.MULTIPLY, mul)
ap(tokens.DIVIDE, div)
ap(tokens.POWER, power)
ap(tokens.SIN, sin)
ap(tokens.COS, cos)
ap(tokens.TAN, tan)
ap(tokens.ARCTAN, atan)
ap(tokens.ARCTAN2, atan2)

# comparison
ap(tokens.IS, is_)
ap(tokens.EQUAL, equal)
ap(tokens.GREATER_THAN, gt)
ap(tokens.GREATER_THAN_EQUAL, gte)
ap(tokens.LESS_THAN, lt)
ap(tokens.LESS_THAN_EQUAL, lte)

# cons
ap(tokens.CONS, cons)
ap(tokens.CAR, car)
ap(tokens.CDR, cdr)
ap(tokens.LISTP, lang.Cons.is_list)

# meta
ap(tokens.GENERATE_SYMBOL, gensym)
ap(tokens.TYPE, type_)

def parse(token_source):
    """
    Given a token source, parses the token sequence into an abstract syntax
    tree built from the base elements of the language.
    """

    # where the abstract syntax tree is held
    ast = []

    # stack where the active scope is kept. starts with the ast as the initial
    # active scope where tokens are added.
    stack = [ast]

    def add_token(token):
        """Adds a token to the active scope on the stack."""

        # add the token to the top-most (active) scope of the stack
        stack[-1].append(lang.Atom.to_atom(token))

    def indent():
        """Adds an indent level to the ast when an indent marker is found."""

        # add a new level to last indent scope and push same list onto stack
        new_scope = []
        stack[-1].append(new_scope)
        stack.append(new_scope)

    def dedent():
        """Reduces the indent level, changing the scope that receives tokens."""

        # remove current level of indentation from the stack
        stack.pop()

        if len(stack) < 1:
            raise errors.OpenParenError.build()

    # we keep a buffer of string parts so we can concatenate all the parts of
    # the string together at once, and so we can check whether we're in a string
    # and whether tokens are escaped.
    string_buf = []
    is_escaped = False

    # we store the locations and indexes where we added sugary tokens so we can
    # quickly post-process them when done parsing.
    sugar_locations = []

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
            # will be appended literally. we make sure we're not currently
            # escaped so we can escape the escape character itself.
            if token == tokens.ESCAPE_CHAR and not is_escaped:
                is_escaped = True

            # if the token preceding this token is an escape char, this token
            # gets appended to the string literally and we switch off escaping.
            elif is_escaped:
                is_escaped = False

            # end the string and flush if we found an unescaped string token
            elif token == tokens.STRING:
                # add the entire string as one token and clear the string buffer
                add_token(''.join(string_buf))

                # clear the string buffer in-place
                del string_buf[:]

        # skip whitespace and comments
        elif token[0] in tokens.WHITESPACE or token.startswith(tokens.COMMENT):
            pass

        # open parenthesis indents
        elif token == tokens.OPEN_PAREN:
            indent()

        # close parenthesis dedents
        elif token == tokens.CLOSE_PAREN:
            dedent()

        # quote, unquote, quasiquote (the only sugar in our language)
        elif token in tokens.SUGAR:
            # we mark the stack and position of the token for quick reference
            sugar_locations.append((stack[-1], len(stack[-1])))
            add_token(token)

        # mark strings
        elif token == tokens.STRING:
            # mark us as being in a string, let the first case deal with rest
            string_buf.append(token)

        # just a normal token
        else:
            add_token(token)

    # ensure all strings were correctly closed
    if len(string_buf) > 0:
        raise errors.ParserError("unclosed string")

    # check to see if we matched all closing parenthesis (first item is always
    # tokens list, and it never gets popped).
    if len(stack) > 1:
        raise errors.CloseParenError.build()

    # process all the quote marks into quote functions. we process right-to-left
    # to allow for occurences of "''foo" and the like.
    for scope, i in reversed(sugar_locations):
        # quotes must have something to consume
        if i == len(scope) - 1:
            raise errors.ParserError("invalid quote syntax")

        # have the sugar mark consume the item to its right and replace the
        # slots the two once filled with a new scope containing the desugared
        # function and its argument.
        new_symbol = lang.Symbol(tokens.SUGAR[scope[i].value])
        new_item = scope[i + 1]

        scope[i] = [new_symbol, new_item]
        del scope[i + 1]

    # return the canonical abstract syntax tree as a Cons list
    return lang.Cons.build(*ast)

def quasiquote_evaluate(sexp, env, level=0):
    """
    Traverses a nested list of S-expressions and evaluates unquoted or spliced
    sections before returning the traversed structure. Handles quasiquote
    nesting.
    """

    # NOTE: this is the only place that the 'unquote' and
    # 'unquote-splicing' tokens are treated as valid. they simply
    # resolve to undefined symbols everywhere else.

    assert level >= 0

    # don't do anything fancy with non-lists
    if not lang.Cons.is_list(sexp):
        return sexp

    # evaluate unquoted expressions (if any) when given a list
    result = []
    for item in sexp:
        if (lang.Cons.is_list(item) and len(item) > 0 and
                isinstance(item.car, lang.Symbol)):
            # quasiquote
            if item.car.value == tokens.QUASIQUOTE_LONG:
                # further nesting always preserves the quasiquote expression
                util.ensure_args(item.cdr, num_required=1)
                result.append(quasiquote_evaluate(item, env, level + 1))

            # unquote
            elif item.car.value == tokens.UNQUOTE_LONG:
                util.ensure_args(item.cdr, num_required=1)

                if level == 0:
                    # evaluate item directly if we're fully unquoted
                    a = evaluate(item.cdr.car, env)
                else:
                    # otherwise, nest a deeper quasiquote and leave this alone
                    a = quasiquote_evaluate(item, env, level - 1)

                result.append(a)

            # unquote-splicing
            elif item.car.value == tokens.UNQUOTE_SPLICING_LONG:
                util.ensure_args(item.cdr, num_required=1)
                if level == 0:
                    result.extend(evaluate(item.cdr.car, env))
                else:
                    result.append(quasiquote_evaluate(item, env, level - 1))

            # normal lists
            else:
                # qq-evaluate the item at the current level of nesting, add it
                result.append(quasiquote_evaluate(item, env, level))
        else:
            # not a list, length is 0, or first item isn't a symbol
            result.append(quasiquote_evaluate(item, env, level))

    # return the semi-evaluated arguments as a list
    return lang.Cons.build(*result)

def evaluate(sexp, env):
    """
    Given an Atom or list, evaluates it using the given environment
    (global by default) and returns the result as represented in our language
    constructs.
    """

    # symbol
    if isinstance(sexp, lang.Symbol):
        # look it up in the environment for its value
        return env.find(sexp)

    # atom (not a literal list)
    elif not lang.Cons.is_list(sexp):
        # it's a generic atom and evaluates to itself
        return sexp

    # list
    else:
        # we can't evaluate functions that have nothing in them
        if len(sexp) == 0:
            raise errors.ApplicationError("nothing to apply")

        # evaluate functions using their arguments
        function = evaluate(sexp.car, env)
        args = sexp.cdr

        # make sure our first item evaluated to a function
        if not isinstance(function, lang.Callable):
            raise errors.ApplicationError("wrong type to apply: " +
                    str(function))

        # quote
        if function is quote:
            # return the argument unevaluated
            util.ensure_args(args, num_required=1)
            return args.car

        # quasiquote
        elif function is quasiquote:
            util.ensure_args(args, num_required=1)
            return quasiquote_evaluate(args.car, env)

        # function
        elif function is lambda_:
            util.ensure_args(args, num_required=2)

            arg_symbols = args.car
            body = args.cdr.car

            # return a function with the current environment as the parent
            return lang.Function(evaluate, env, arg_symbols, body)

        # macro
        elif function is macro:
            util.ensure_args(args, num_required=2)

            arg_symbols = args.car
            body = args.cdr.car

            # return a macro with the given symbols and body
            return lang.Macro(evaluate, env, arg_symbols, body)

        # macro expand
        elif function is expand:
            util.ensure_args(args, num_required=1, is_variadic=True)

            # evaluate to get the macro and its arguments
            m = evaluate(args.car, env)
            arg_expressions = [evaluate(arg, env) for arg in args.cdr]

            # make sure we got a macro
            util.ensure_type(lang.Macro, m)

            return m(evaluate, env, *arg_expressions)

        # define
        elif function is define:
            util.ensure_args(args, num_required=2)

            symbol = args.car
            value = args.cdr.car

            # make sure we're defining to a symbol
            util.ensure_type(lang.Symbol, symbol)

            # evaluate the argument, map the symbol to the result in the current
            # environment, then return the evaluated value. this allows for
            # chains of definitions, or simultaneous variable assignments to the
            # same value.
            result = evaluate(value, env)
            env[symbol] = result

            # set the function or macro name if possible
            if isinstance(result, lang.Callable):
                result.name(symbol.value)

            return result

        # cond
        elif function is cond:
            for tup in args:
                # if e is not a list, len() raises an error for us
                if len(tup) != 2:
                    # make sure each is a list of exactly two expressions
                    s = "expected 2 expressions, got " + str(len(tup))
                    raise errors.IncorrectArgumentCountError(s)

                # first and second list items are condition and result
                condition = tup.car
                result = tup.cdr.car

                # evaluate and return the result if condition is true
                if evaluate(condition, env):
                    return evaluate(result, env)

            # if no result is returned, result is undefined
            raise errors.ApplicationError("at least one condition must " +
                    "evaluate to " + lang.TRUE)

        # logical and
        elif function is and_:
            util.ensure_args(args, num_required=2, is_variadic=True)

            # evaluate the arguments, returning the final one if none were #f,
            # otherwise the last evaluated item, #f.
            last_item = None
            for item in args:
                last_item = evaluate(item, env)
                if last_item is lang.FALSE:
                    break

            return last_item

        # logical or
        elif function is or_:
            util.ensure_args(args, num_required=2, is_variadic=True)

            # evaluate the arguments, returning the first one that's not #f,
            last_item = None
            for item in args:
                last_item = evaluate(item, env)
                if not last_item is lang.FALSE:
                    break

            return last_item

        # eval
        elif function is eval_:
            util.ensure_args(args, num_required=1)

            # evaluate the given s-expression and return it
            return evaluate(evaluate(args.car, env), env)

        # load
        elif function is load:
            util.ensure_args(args, num_required=1)
            util.ensure_type(lang.String, args.car)

            # evaluate every expression in the file in sequence, top to bottom
            with open(os.path.abspath(args.car.value), "r") as f:
                for result in parse(tokens.tokenize(util.file_char_iter(f))):
                    evaluate(result, env)

            # return that we were successful
            return lang.TRUE

        # evaluate macros
        elif isinstance(function, lang.Macro):
            # evaluate the expanded form of the macro in the current environment
            return evaluate(function(evaluate, env, *args), env)

        else:
            # evaluate args and call the function with them
            return function(evaluate, *[evaluate(arg, env) for arg in args])

if __name__ == "__main__":
    import sys
    import traceback

    source = ""

    standard_prompt = "> "
    continue_prompt = ": "
    prompt = standard_prompt

    print "plinth 0.2"
    print "-----------"

    # load all provided files into the global environment on interpreter start
    for fname in sys.argv[1:]:
        # evaluate every expression in the file in sequence, top to bottom
        with open(os.path.abspath(fname), "r") as f:
            for result in parse(tokens.tokenize(util.file_char_iter(f))):
                evaluate(result, GLOBAL_ENV)
        print "loaded '" + os.path.abspath(fname) + "'"

    while 1:
        try:
            # get input from user and try to tokenize, parse, and print it
            source += raw_input(prompt)

            # evaluate every entered expression sequentially
            for result in parse(tokens.tokenize(source)):
                print evaluate(result, GLOBAL_ENV)

            # reset the source and prompt on a successful evaluation
            source = ""
            prompt = standard_prompt

        except errors.ParserError:
            # allow the user to finish entering a correct expression
            prompt = continue_prompt
            source += os.linesep

        except KeyboardInterrupt:
            # reset input on Ctrl+C
            prompt = standard_prompt
            source = ""
            print
        except EOFError:
            # exit on Ctrl+D
            print
            sys.exit()
        except Exception, e:
            # print all other problems and clear source
            traceback.print_exc()

            # reset the source and prompt for the next parse
            source = ""
            prompt = standard_prompt
