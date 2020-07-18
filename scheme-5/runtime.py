from re import S
from typing import NewType, Generic
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


# typing definition
Number = int
String = str
Bool   = bool
Symbol = str
Procedure = Callable[[List['SchValue'], Environment], 'SchValue']
Pair = Tuple['SchValue', 'SchValue']
Nil = None

SchList = Union[Pair, Nil]

TypeLiteral = Literal[
    'Number', 'String', 'Bool', 'Symbol', 
    'Procedure', 'Pair', 'Nil',
    'SchList'
]
ValueType = Union[Number, String, Bool, Symbol, Procedure, Pair, Nil, SchList]
SchValue = ValueType
# class SchValue:
#     def __init__(self, typing: TypeLiteral, value: ValueType, mutable: bool=False):
#         self.typing = typing
#         self.value = value
#         self.mutable = mutable
    
#     def is_type(self, typing: TypeLiteral) -> bool:
#         return self.typing == typing

#     def change(self, value: 'SchValue'):
#         if self.typing == 'Bool':
#             if value.is_type('Bool') and value.get() == False:
#                 self.value = False
#             self.value = True
#         else:
#             if self.typing != value.typing:
#                 raise RuntimeError('Type do not match!')
#             self.value = value.get()
    
#     def get(self) -> ValueType:
#         return self.value


def eqv(a: SchValue, b: SchValue) -> Bool:
    return a == b

def make_pair(first: SchValue, second: SchValue) -> Pair:
    return (first, second)

def car(pair: Pair) -> SchValue:
    return pair[0]

def cdr(pair: Pair) -> SchValue:
    return pair[1]

def make_sch_list(vals: Pair) -> SchList:
    if vals == []:
        return None
    return (vals[0], make_sch_list(vals))

def list_ref(l: Pair, idx: Number) -> SchValue:
    if idx == 0:
        return car(l)
    return list_ref(l, idx - 1)

inital_marco = [
'''
(define-syntax define
    (syntax-rule ()
        (define (name args ...)) (define-var name (lambda args ...)) 
        (define name var)        (define-var name var              )))
'''

]