#!/usr/bin/env python

import re

def lex(text):
    """
    Lexes a given string into a list of strings, paying special attention to
    actual strings within the input.  If an un-parenthesized expression is
    given, it is returned as a bare atom.  Parenthesized expressions are
    returned as lists of strings and other lists.
    """

    # used for consuming whitespace while parsing
    whitespace_regex = re.compile("\s")

    # result acts as a stack that holds the strings grouped by nested parens.
    # result will only ever contain one item, the first level of indenting
    # encountered.
    result = []

    # the current level of indentation, used to append chars to correct level
    indent = 0

    # the non-indenting characters we find. these are kept in a buffer until
    # we indent or dedent, and then are added to the current indent level all
    # at once, for efficiency.
    s = []

    # whether we're currently in the middle of parsing a string
    in_string = False

    # the last character seen, None to begin with
    prev_c = None
    for c in text:
        # prevent parsing parens when inside a string (also ignores escaped
        # characters).
        if c == '"' and prev_c != "\\":
            in_string = not in_string
            s.append(c)

        # we only indent/dedent if not in the middle of parsing a string
        elif c == "(" and not in_string:
            # recurse into current level of nesting
            cur = result
            for i in xrange(indent):
                cur = cur[-1]

            # add our buffered string onto the previous level, then clear it
            # for the next.
            if len(s) > 0:
                cur.append(''.join(s))
                s = []

            # append a new level of nesting to our list
            cur.append([])

            # increase the indent level so we can get back to this level later
            indent += 1

        elif c == ")" and not in_string:
            # append remaining string buffer before dedenting
            if len(s) > 0:
                # recurse into the current level of nesting
                cur = result
                for i in xrange(indent):
                    cur = cur[-1]

                cur.append(''.join(s))
                s = []

            # we finished with one level, so dedent back to the previous one
            indent -= 1

        # append non-whitespace characters to the buffer list. whitespace is a
        # delimiter for expressions, hence is special.
        elif not whitespace_regex.match(c):
            # append the current string character to the buffer list.
            s.append(c)

        # we separate expressions by spaces
        elif whitespace_regex.match(c) and len(s) > 0:
            # recurse into the current level of nesting
            cur = result
            for i in xrange(indent):
                cur = cur[-1]

            cur.append(''.join(s))
            s = []

        # save the previous character. used to determine if c is escaped
        prev_c = c

    # if we entered an expression without any parenthesis, the buffer won't be
    # empty and we should append its contents as an atom.
    if len(s) > 0:
        result.append(''.join(s))

    # if we entered some combination of lists and atoms, that's an error
    if len(result) > 1:
        raise ValueError("SYNTAX ERROR: May only enter one atom or expression.")
    # if we didn't match all our parenthesis, that's an error
    elif indent != 0:
        raise ValueError("SYNTAX ERROR: Unmatched parenthesis.")

    # this returns the first and only message found.  result is a list simply
    # because it makes adding new levels of indentation simpler as it avoids
    # the 'if result is None' corner case that would come up when trying to
    # append the first '('.
    return result[0]

if __name__ == "__main__":
    import sys
    import traceback
    from pprint import pprint

    while 1:
        try:
            s = raw_input("> ")
            try:
                pprint(lex(s))
            except Exception, e:
                print e
                traceback.print_exc()
        except KeyboardInterrupt:
            print
            continue
        except EOFError:
            print
            break
