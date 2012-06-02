
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
    def build(min_expected, max_expected, num_actual, is_variadic=False):

        assert max_expected >= min_expected

        s = "expected "

        if min_expected != max_expected and not is_variadic:
            s += "between " + str(min_expected) + " "
            s += "and " + str(max_expected) + ", "
        elif is_variadic:
            s += "at least " + str(min_expected) + ", "
        else:
            s += str(max_expected) + ", "

        s += "got " + str(num_actual)

        return IncorrectArgumentCountError(s)

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
