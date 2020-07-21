from abc import ABC, abstractmethod
from os import EX_PROTOCOL, sendfile, terminal_size
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
    
    @abstractmethod
    def to_str(self) -> str:
        raise RuntimeError('Not impl')

    def as_value(self, cls: Type['ValueType']) -> 'ValueType':
        # 不需要再检查了, 在基本过程开头会有检查的
        return self # type: ignore

class AtomValue(Generic[T], SchValue):
    def __init__(self, data: T) -> None:
        self._data = data
    
    def data(self) -> T:
        return self._data
    
    def to_str(self) -> str:
        return str(self._data)

class Number(AtomValue[int]):    pass
class String(AtomValue[str]):    pass
class Symbol(AtomValue[str]):    pass
class Character(AtomValue[str]): pass
class Boolean(AtomValue[bool]):  pass

EvalEnv = Environment[SchValue]

class Procedure(SchValue):
    def __init__(self, func: Callable[[List[SchValue]], SchValue]) -> None:
        self.func = func
    
    def call(self, operands: List[SchValue]) -> SchValue:
        func = self.func
        return func(operands)
    
    def to_str(self) -> str:
        return str(self.func)

class Nil(SchValue):
    def __init__(self) -> None:
        pass
    def to_str(self) -> str:
        return '\'()'

nil: Nil = Nil()

class Pair(SchValue):
    def __init__(self, car: SchValue, cdr: SchValue) -> None:
        self.car = car
        self.cdr = cdr
    
    def to_list(self) -> List[SchValue]:
        ret = [self.car]
        if isinstance(self.cdr, Pair):
            ret.extend(self.cdr.to_list())
        return ret

    def to_str(self) -> str:
        l = self.to_list()
        front, last = l[:-1], l[-1]
        front_str = ' '.join([o.to_str() for o in front])
        if last == nil:
            return f'({front_str})'
        else:
            return f'({front_str} . {last.to_str()})'
        


ValueType = TypeVar('ValueType', Number, String, Boolean, Symbol, Procedure, Pair, Nil)

# 基本过程

def assuming_type(operands: List[SchValue], types: List[Type[SchValue]], variadic: bool=False) -> None:
    expect_len = len(types)
    given_len = len(operands)
    if not variadic and expect_len != given_len:
        raise RuntimeError(f'Too many or too less operands: expect: {expect_len}, given: {given_len}')
    if variadic and given_len < expect_len:
        raise RuntimeError(f'Too less operands: expect more than: {expect_len}, given: {given_len}')

    for idx, (op, ty) in enumerate(zip(operands, types)):
        if not isinstance(op, ty):
            raise TypeError(f'Unmatch type in operand {idx}')

# 手动类型检查装饰器   
def operands_type(types: List[Type[SchValue]], variadic: bool=False):
    def decorator(func: Callable[[List[SchValue]], SchValue]):
        def wrapper(operands: List[SchValue]):
            assuming_type(operands, types, variadic)
            return func(operands)
        return wrapper
    return decorator

@operands_type([SchValue])
def display(operands: List[SchValue]) -> SchValue:
    print(operands[0].to_str(), end='')
    return Boolean(False)

@operands_type([])
def newline(operands: List[SchValue]) -> SchValue:
    print('')
    return Boolean(False)

@operands_type([SchValue, SchValue])
def make_pair(operands: List[SchValue]) -> Pair:
    return Pair(operands[0], operands[1])

@operands_type([Pair])
def car(operands: List[SchValue]) -> SchValue:
    return operands[0].car # type: ignore

@operands_type([Pair])
def cdr(operands: List[SchValue]) -> SchValue:
    return operands[0].cdr # type: ignore

@operands_type([], variadic=True)
def make_list(operands: List[SchValue]) -> Union[Pair, Nil]:
    if len(operands) == 0:
        return nil
    return Pair(operands[0], make_list(operands[1:]))

@operands_type([SchValue])
def is_nil(operands: List[SchValue]) -> Boolean:
    if isinstance(operands[0], Nil):
        return Boolean(True)
    return Boolean(False)

def inital_environment() -> EvalEnv:
    primitive_func: Dict[str, Callable[[List[SchValue]], SchValue]] = {
        'display': display, 
        'newline': newline,
        'make-pair': make_pair,
        'cons': make_pair,
        'car': car,
        'cdr': cdr,
        'make-list': make_list,
        'null?': lambda x: isinstance(x[0], Nil)
    }

    primitive_value: Dict[str, SchValue] = {
        'nil': nil, 
        'else': Boolean(True)
    }

    env = EvalEnv(None)
    for k, v in primitive_func.items():
        env.bind(k, Procedure(v))
    
    for k, v in primitive_value.items():
        env.bind(k, v)
    
    return env
    

