from re import S
from typing import NewType
from . import *
from .pre_eval import Environment

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