#!/usr/bin/env python

import os
import sys
import traceback

import errors
import lang
import primitives
import tokens
import util

def parse(token_source):
    '''
    Given a token source, parses the token sequence into an abstract syntax
    tree built from the base elements of the language.
    '''

    # where the abstract syntax tree is held
    ast = []

    # stack where the active scope is kept. starts with the ast as the initial
    # active scope where tokens are added.
    stack = [ast]

    def add_token(token):
        '''Adds a token to the active scope on the stack.'''

        # add the token to the top-most (active) scope of the stack
        stack[-1].append(lang.Atom.to_atom(token))

    def indent():
        '''Adds an indent level to the ast when an indent marker is found.'''

        # add a new level to last indent scope and push same list onto stack
        new_scope = []
        stack[-1].append(new_scope)
        stack.append(new_scope)

    def dedent():
        '''Reduces the indent level, changing the scope that receives tokens.'''

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
        raise errors.ParserError('unclosed string')

    # check to see if we matched all closing parenthesis (first item is always
    # tokens list, and it never gets popped).
    if len(stack) > 1:
        raise errors.CloseParenError.build()

    # process all the quote marks into quote functions. we process right-to-left
    # to allow for occurences of "''foo" and the like.
    for scope, i in reversed(sugar_locations):
        # quotes must have something to consume
        if i == len(scope) - 1:
            raise errors.ParserError('invalid quote syntax')

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
    '''
    Traverses a nested list of S-expressions and evaluates unquoted or spliced
    sections before returning the traversed structure. Handles quasiquote
    nesting.
    '''

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
    '''
    Given an Atom or list, evaluates it using the given environment
    (global by default) and returns the result as represented in our language
    constructs.
    '''

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
            raise errors.ApplicationError('nothing to apply')

        # evaluate functions using their arguments
        function = evaluate(sexp.car, env)
        args = sexp.cdr

        # make sure our first item evaluated to a function
        if not isinstance(function, lang.Callable):
            raise errors.ApplicationError('wrong type to apply: ' +
                    str(function))

        # quote
        if function is primitives.quote:
            # return the argument unevaluated
            util.ensure_args(args, num_required=1)
            return args.car

        # quasiquote
        elif function is primitives.quasiquote:
            util.ensure_args(args, num_required=1)
            return quasiquote_evaluate(args.car, env)

        # function
        elif function is primitives.lambda_:
            util.ensure_args(args, num_required=2)

            arg_symbols = args.car
            body = args.cdr.car

            # return a function with the current environment as the parent
            return lang.Function(evaluate, env, arg_symbols, body)

        # macro
        elif function is primitives.macro:
            util.ensure_args(args, num_required=2)

            arg_symbols = args.car
            body = args.cdr.car

            # return a macro with the given symbols and body
            return lang.Macro(evaluate, env, arg_symbols, body)

        # macro expand
        elif function is primitives.expand:
            util.ensure_args(args, num_required=1, is_variadic=True)

            # evaluate to get the macro and its arguments
            m = evaluate(args.car, env)
            arg_expressions = [evaluate(arg, env) for arg in args.cdr]

            # make sure we got a macro
            util.ensure_type(lang.Macro, m)

            return m(evaluate, env, *arg_expressions)

        # define
        elif function is primitives.define:
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
        elif function is primitives.cond:
            for tup in args:
                # if e is not a list, len() raises an error for us
                if len(tup) != 2:
                    # make sure each is a list of exactly two expressions
                    s = 'expected 2 expressions, got ' + str(len(tup))
                    raise errors.IncorrectArgumentCountError(s)

                # first and second list items are condition and result
                condition = tup.car
                result = tup.cdr.car

                # evaluate and return the result if condition is true
                if evaluate(condition, env):
                    return evaluate(result, env)

            # if no result is returned, result is undefined
            raise errors.ApplicationError('at least one condition must ' +
                    'evaluate to ' + lang.TRUE)

        # logical and
        elif function is primitives.and_:
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
        elif function is primitives.or_:
            util.ensure_args(args, num_required=2, is_variadic=True)

            # evaluate the arguments, returning the first one that's not #f,
            last_item = None
            for item in args:
                last_item = evaluate(item, env)
                if not last_item is lang.FALSE:
                    break

            return last_item

        # eval
        elif function is primitives.eval_:
            util.ensure_args(args, num_required=1)

            # evaluate the given s-expression and return it
            return evaluate(evaluate(args.car, env), env)

        # load
        elif function is primitives.load:
            util.ensure_args(args, num_required=1)
            util.ensure_type(lang.String, args.car)

            # evaluate every expression in the file in sequence, top to bottom
            with open(os.path.abspath(args.car.value), 'r') as f:
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

class Interpreter(object):
    '''Implements the standard plinth interpreter.'''

    def __init__(self, standard_prompt='> ', continue_prompt=': '):
        self.standard_prompt = standard_prompt
        self.continue_prompt = continue_prompt

        # create the global environment for the interpreter
        self.env = lang.Environment(None)

        # bind functions that need special treatment during evaluation
        self.env[lang.Symbol(tokens.QUOTE_LONG)] = primitives.quote
        self.env[lang.Symbol(tokens.QUASIQUOTE_LONG)] = primitives.quasiquote
        self.env[lang.Symbol(tokens.LAMBDA)] = primitives.lambda_
        self.env[lang.Symbol(tokens.MACRO)] = primitives.macro
        self.env[lang.Symbol(tokens.MACRO_EXPAND)] = primitives.expand
        self.env[lang.Symbol(tokens.DEFINE)] = primitives.define
        self.env[lang.Symbol(tokens.COND)] = primitives.cond
        self.env[lang.Symbol(tokens.AND)] = primitives.and_
        self.env[lang.Symbol(tokens.OR)] = primitives.or_
        self.env[lang.Symbol(tokens.EVAL)] = primitives.eval_
        self.env[lang.Symbol(tokens.LOAD)] = primitives.load

        # repl
        self.bind_prim(tokens.READ, primitives.read)
        self.bind_prim(tokens.PARSE, primitives.parse_)

        # logical
        self.bind_prim(tokens.NOT, primitives.not_)

        # math
        self.bind_prim(tokens.ADD, primitives.add)
        self.bind_prim(tokens.SUBTRACT, primitives.sub)
        self.bind_prim(tokens.MULTIPLY, primitives.mul)
        self.bind_prim(tokens.DIVIDE, primitives.div)
        self.bind_prim(tokens.POWER, primitives.power)
        self.bind_prim(tokens.SIN, primitives.sin)
        self.bind_prim(tokens.COS, primitives.cos)
        self.bind_prim(tokens.TAN, primitives.tan)
        self.bind_prim(tokens.ARCTAN, primitives.atan)
        self.bind_prim(tokens.ARCTAN2, primitives.atan2)

        # comparison
        self.bind_prim(tokens.IS, primitives.is_)
        self.bind_prim(tokens.EQUAL, primitives.equal)
        self.bind_prim(tokens.GREATER_THAN, primitives.gt)
        self.bind_prim(tokens.GREATER_THAN_EQUAL, primitives.gte)
        self.bind_prim(tokens.LESS_THAN, primitives.lt)
        self.bind_prim(tokens.LESS_THAN_EQUAL, primitives.lte)

        # cons
        self.bind_prim(tokens.CONS, primitives.cons)
        self.bind_prim(tokens.CAR, primitives.car)
        self.bind_prim(tokens.CDR, primitives.cdr)
        self.bind_prim(tokens.LISTP, lang.Cons.is_list)

        # meta
        self.bind_prim(tokens.GENERATE_SYMBOL, primitives.gensym)
        self.bind_prim(tokens.TYPE, primitives.type_)

    def bind_prim(self, token, function):
        '''Binds a primitive function to a token in the global environment.'''
        if self.env is not None:
            s = lang.Symbol(token)
            f = lang.PrimitiveFunction(function, token)
            self.env.put(s, f)

    def repl(self):
        '''
        The standard interpreter loop.

        Reads input, parses it, evaluates it, and prints the result to the
        console.

        Continues to run until the user exits by sending an EOF, then returns.
        '''

        # the current source code that's been entered
        source = ''

        # the current state of the prompt
        prompt = self.standard_prompt

        print 'plinth 0.2'
        print '-----------'

        # load all provided files into the global environment on interpreter start
        for fname in sys.argv[1:]:
            # evaluate every expression in the file in sequence, top to bottom
            with open(os.path.abspath(fname), 'r') as f:
                for result in parse(tokens.tokenize(util.file_char_iter(f))):
                    evaluate(result, self.env)
            print "loaded '" + os.path.abspath(fname) + "'"

        while 1:
            try:
                # get input from user and try to tokenize, parse, and print it
                source += raw_input(prompt)

                # evaluate every entered expression sequentially
                for result in parse(tokens.tokenize(source)):
                    print evaluate(result, self.env)

                # reset the source and prompt on a successful evaluation
                source = ''
                prompt = self.standard_prompt

            except errors.ParserError:
                # allow the user to finish entering a correct expression
                prompt = continue_prompt
                source += os.linesep

            except KeyboardInterrupt:
                # reset input on Ctrl+C
                prompt = self.standard_prompt
                source = ''
                print
            except EOFError:
                # exit on Ctrl+D
                print
                return
            except Exception, e:
                # print all other problems and clear source
                traceback.print_exc()

                # reset the source and prompt for the next parse
                source = ''
                prompt = self.standard_prompt


if __name__ == '__main__':
    i = Interpreter()

    # run until the user quits
    i.repl()

    # exit with success
    sys.exit(0)
