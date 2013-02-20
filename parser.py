import errors
import lang
import tokens

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
