from abc import ABC
from typing import Generic, Type, Optional
from . import *

# eval 时的环境, 其实应该就相当于标准库的 ChainMap ?
class Environment(Generic[T]):
    def __init__(self, father: 'Environment'=None, inital={}) -> None:
        self.father = father
        self.binds = inital

    def bind(self, name: str, var: T) -> 'Environment[T]':
        self.binds[name] = var
        return self
    
    def get(self, name: str) -> T:
        if name not in self.binds:
            if self.father:
                return self.father.get(name)
            raise RuntimeError(f'Unbound name: {name}')
        return self.binds[name]
    
    def has(self, name: str) -> bool:
        try:
            self.get(name)
        except RuntimeError:
            return False
        return True
    
    def update(self, binds: Dict[str, T]) -> 'Environment[T]':
        self.binds = union(self.binds, binds)
        return self

# 一些辅助的断言
def assuming_len_in(l: List[Any], len_min: Optional[int], len_max: Optional[int], message: str='Bad syntax') -> None:
    if len_min and len(l) < len_min:
        raise RuntimeError(message)
    if len_max and len(l) > len_max:
        raise RuntimeError(message)

def not_empty(l: List[Any], message: str='Bad syntax') -> None:
    assuming_len_in(l, 1, None, message)

def assuming_len(l: List[Any], length: int, message: str='Bad syntax') -> None:
    assuming_len_in(l, length, length, message)


# typing definition
class SchValue(ABC):
    def __init__(self) -> None:
        pass

class AtomValue(Generic[T], SchValue):
    def __init__(self, data: T) -> None:
        self._data = data
    
    def data(self) -> T:
        return self._data

class Number(AtomValue[int]):    pass
class String(AtomValue[str]):    pass
class Symbol(AtomValue[str]):    pass
class Character(AtomValue[str]): pass
class Boolean(AtomValue[bool]):  pass

EvalEnv = Environment[SchValue]

class Procedure(SchValue):
    def __init__(self, func: Callable[[List[SchValue]], SchValue], signature: List[Type['ValueType']]=None) -> None:
        self.func = func
        self.signature = signature
    
    def call(self, operands: List[SchValue]) -> SchValue:
        if self.signature:
            # Do type check here
            pass
        func = self.func
        return func(operands)

class Pair(SchValue):
    def __init__(self, car: SchValue, cdr: SchValue) -> None:
        self.car = car
        self.cdr = cdr
    
class Nil(SchValue):
    def __init__(self) -> None:
        pass
        # raise RuntimeError('You can not construct a nil')
nil: Nil = Nil()

ValueType = Union[Number, String, Boolean, Symbol, Procedure, Pair, Nil]

inital_marco = [
'''
(define-syntax define
    (syntax-rule ()
        (define (name args ...)) (define-var name (lambda args ...)) 
        (define name var)        (define-var name var              )))
'''
]
