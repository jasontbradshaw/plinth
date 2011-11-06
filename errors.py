
class ParserError(Exception):
    """Raised when parsing input fails."""

class OpenParenError(ParserError):
    """Raised when there are too few opening parenthesis."""

    def __init__(self):
        ParserError.__init__(self, "too few opening parenthesis")

class CloseParenError(ParserError):
    """Raised when there are too few closing parenthesis."""

    def __init__(self):
        ParserError.__init__(self, "too few closing parenthesis")

class SymbolNotFoundError(Exception):
    """Raised when a symbol could not be found in an environment chain."""

    def __init__(self, symbol):
        Exception.__init__(self, "could not find " + str(symbol))

class IncorrectArgumentCountError(Exception):
    """Raised when a function is called with the wrong number of arguments."""

    def __init__(self, expected, actual):
        Exception.__init__(self, "expected " + str(expected) +
                ", got " + str(actual))

class WrongArgumentTypeError(Exception):
    """Raised when an argument is of the wrong type."""
    def __init__(self, arg, expected_class):
        Exception.__init__(self, "wrong argument type for " + str(arg) +
                ": expected " + expected_class.__name__.lower() + ", got " +
                arg.__class__.__name__.lower())

class ApplicationError(Exception):
    """Raised when a function could not be applied correctly."""

