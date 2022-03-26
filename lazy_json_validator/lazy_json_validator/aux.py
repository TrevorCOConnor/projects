# Standard Modules
from functools import reduce, partial
from typing import TypeVar, Iterable, Optional, Callable
import operator as op


A = TypeVar('A')
B = TypeVar('B')
C = TypeVar('C')


def compose(*functions):
    return reduce(lambda f, g: lambda x: f(g(x)), functions, lambda x: x)


def filter_out_empty_lists(lists: Iterable[list[A]]) -> list[list[A]]:
    filtered_list = list(filter(
        compose(op.not_, partial(op.eq, 0), len), lists
    ))

    return filtered_list


def lift_optional(
    func: Callable[[A, B], C]
) -> Callable[[Optional[A], Optional[B]], Optional[C]]:
    """ Lifts a function to work in the Optional space. """
    def inner(x: Optional[A], y: Optional[B]):
        if (x is None) or (y is None):
            return None
        else:
            return func(x, y)

    return inner
