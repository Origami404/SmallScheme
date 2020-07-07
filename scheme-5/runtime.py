from . import *
from .pre_eval import Environment

# typing definition
Number = int
String = str
Bool   = bool
Symbol = str
Procedure = Callable[[List['SchValue'], Environment], 'SchValue']
Pair = Tuple['SchValue', 'SchValue']

class SchValue:
    def __init__(self):
        return


