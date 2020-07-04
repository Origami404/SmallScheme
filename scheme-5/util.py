
# Regex

from re import compile as regex


# Typing

from typing import List, Dict, Callable, Generator, Iterable, TypeVar, Union, Tuple, Any, Literal

T, X, Y = TypeVar('T'), TypeVar('X'), TypeVar('Y')

# small tools 

from functools import reduce

def make_list(data: Any) -> list:
    if type(data) == type([]):
        return data
    else: return [data]

def lmap(f: Callable[[X], Y], iter: Iterable[X]) -> List[Y]:
    return list(map(f, iter))

def bind_tail(f, *args) -> Callable:
    return lambda x: f(x, *args)

def not_none(iter) -> Iterable:
    return filter(lambda x: x != None, iter)

def lnmap(f: Callable[[X], Y], iter: Iterable[X]) -> List[Y]:
    return list(not_none(map(f, iter)))

def union(dict1: Dict[X, Y], dict2: Dict[X, Y]) -> Dict[X, Y]:
    ret = {}
    ret.update(dict1)
    ret.update(dict2)
    return ret

def concat(list1: List[X], list2: List[X]) -> List[X]:
    return list1 + list2


# output

from pprint import PrettyPrinter
fprint = PrettyPrinter(2).pprint

