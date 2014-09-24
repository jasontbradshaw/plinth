from __future__ import unicode_literals

import tokens
import lang

class Reader(object):
  '''Receives tokens as input and processes them into an AST.'''

  # all subclasses mapped to their trigger character
  READERS = {
    VectorReader.CHAR: VectorReader,
    MapReader.CHAR: MapReader,
    SetReader.CHAR: SetReader,
    QuoteReader.CHAR: QuoteReader,
    QuasiquoteReader.CHAR: QuasiquoteReader,
    UnquoteReader.CHAR: UnquoteReader,
    UnquoteSplicingReader.CHAR: UnquoteSplicingReader,
  }

  # subclasses should override this property on their own classes. it should be
  # a string with a single character in it.
  CHAR = None

  def process(self, token):
    '''
    Receive the next token. Returns None until we've successfully parsed the
    received sequence, at which point it should return the parsed language
    construct.
    '''

    raise NotImplementedError()

  @staticmethod
  def build(token):
    '''
    Given a token, create a reader from it and return it. Does not feed the
    reader the first token.
    '''
    char = token[0]
    if char in Reader.READERS:
      return Reader.READERS[char]()

    return None

class VectorReader(Reader):
  CHAR = tokens.VECTOR_START

  def process(self, token):
    raise NotImplementedError()

class MapReader(Reader):
  CHAR = tokens.MAP_START

  def process(self, token):
    raise NotImplementedError()

class SetReader(Reader):
  CHAR = tokens.SET_START

  def process(self, token):
    raise NotImplementedError()

class QuoteReader(Reader):
  CHAR = tokens.QUOTE

  def process(self, token):
    raise NotImplementedError()

class QuasiquoteReader(Reader):
  CHAR = tokens.QUASIQUOTE

  def process(self, token):
    raise NotImplementedError()

class UnquoteReader(Reader):
  CHAR = tokens.UNQUOTE

  def process(self, token):
    raise NotImplementedError()
