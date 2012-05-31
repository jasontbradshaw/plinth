import threading

import errors

def ensure_type(required_class, item, *rest):
    """
    Raises a WrongArgumentTypeError if all the items aren't instances of the
    required class/classes tuple.
    """

    if not isinstance(item, required_class):
        raise errors.WrongArgumentTypeError.build(item, required_class)

    for thing in rest:
        if not isinstance(thing, required_class):
            raise errors.WrongArgumentTypeError.build(thing, required_class)

def ensure_args(arg_list, count, exact=True):
    """
    Ensures that an argument list contains a number of arguments. When exact is
    True (the default), ensures that the count is exactly that provided. When
    exact is False, ensures that the count is at least the number provided.
    """

    if exact:
        if len(arg_list) != count:
            raise errors.IncorrectArgumentCountError.build(count, len(arg_list))
    else:
        if len(arg_list) < count:
            raise errors.IncorrectArgumentCountError.build(count, len(arg_list))

class ThreadSafeCounter:
    """When called, returns increasing ints in order."""

    def __init__(self, count=0):
        self.count = count
        self.lock = threading.Lock()

    def __call__(self):
        with self.lock:
            c = self.count
            self.count += 1
            return c
