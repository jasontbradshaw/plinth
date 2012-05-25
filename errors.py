
class ParserError(Exception):
    """Raised when parsing input fails."""

class OpenParenError(ParserError):
    """Raised when there are too few opening parenthesis."""

    @staticmethod
    def build():
        return OpenParenError("too few opening parenthesis")

class CloseParenError(ParserError):
    """Raised when there are too few closing parenthesis."""

    @staticmethod
    def build():
        return CloseParenError("too few closing parenthesis")

class SymbolNotFoundError(Exception):
    """Raised when a symbol could not be found in an environment chain."""

    @staticmethod
    def build(symbol):
        return SymbolNotFoundError("could not find symbol " + str(symbol))

class IncorrectArgumentCountError(Exception):
    """Raised when a function is called with the wrong number of arguments."""

    @staticmethod
    def build(expected, actual):
        return IncorrectArgumentCountError("expected " + str(expected) +
                ", got " + str(actual))

class WrongArgumentTypeError(Exception):
    """Raised when an argument is of the wrong type."""

    @staticmethod
    def build(arg, expected_class):
        expected = ""
        if hasattr(expected_class, "__name__"):
            expected = expected_class.__name__ + ","
        else:
            # support multiple expected classes
            expected = "one of "
            expected += ", ".join(map(lambda x: x.__name__, expected_class))
            expected += ";"

        return WrongArgumentTypeError("wrong argument type for " + str(arg) +
                ": expected " + expected.lower() + " got " +
                arg.__class__.__name__.lower())

class ApplicationError(Exception):
    """Raised when a function could not be applied correctly."""
