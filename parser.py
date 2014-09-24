from __future__ import unicode_literals

import lang
import readers
import tokens

def parse(token_source):
    '''
    Given a token source, parses the token sequence into an abstract syntax
    tree built from the base elements of the language, then returns the tree.
    '''

    current_reader = None

    for t in token_source:

      # if we have a current reader, feed it the input
      if current_reader:
        result = current_reader.process(t)
        if result is not None:
          # unregister the current reader
          current_reader = None

        continue

      # try to 
      current_reader = readers.Reader.build(t)
      if current_reader:
        continue
