
# Regex

from re import L, compile as regex


# Typing

from typing import List, Dict, Callable, Generator, Iterable, Optional, TypeVar, Union, Tuple, Any, Literal, overload
# 常用的用来表示泛型类型的字母, 方便后面泛型函数的定义
T, X, Y = TypeVar('T', covariant=True), TypeVar('X'), TypeVar('Y')

# small tools 

from functools import reduce

for_each = map

# 把 map 返回的 Iterable 变成 list
# 可以看做是即刻求值版 map
def lmap(f: Callable[[X], Y], iter: Iterable[X]) -> List[Y]:
    return list(map(f, iter))

# 将 f 的第2, 3, ...个参数绑定起来; 即将 f(x, ...) 变成 f(x)
def bind_tail(f: Callable[..., Y], *f_args) -> Callable[[X], Y]:
    return lambda x: f(x, *f_args)

# 筛选掉为 None 的元素
def not_none(iter: Iterable[Optional[T]]) -> Iterable[T]:
    return filter(lambda x: x != None, iter)

# 即刻求值 + 筛选非 None 的 map
def lnmap(f: Callable[[X], Optional[Y]], iter: Iterable[X]) -> List[Y]:
    return list(not_none(map(f, iter)))

# 合并两个字典
def union(dict1: Dict[X, Y], dict2: Dict[X, Y]) -> Dict[X, Y]:
    ret = {}
    ret.update(dict1)
    ret.update(dict2)
    return ret

# 合并两个列表
def concat(list1: List[X], list2: List[X]) -> List[X]:
    return list1 + list2


# output

# 对字典的漂亮输出
from pprint import PrettyPrinter
fprint = PrettyPrinter(2).pprint

__all__ = [
    'regex',

    'List', 'Dict', 'Callable', 'Generator', 'Iterable', 
    'TypeVar', 'Union', 'Tuple', 'Any', 'Literal',
    'T', 'X', 'Y',

    'reduce', 'lmap', 'bind_tail', 'not_none', 
    'lnmap', 'union', 'concat', 'fprint'
]