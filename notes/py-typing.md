> Note Recall that the use of a type alias declares two types to be equivalent to one another. Doing Alias = Original will make the static type checker treat Alias as being exactly equivalent to Original in all cases. This is useful when you want to simplify complex type signatures.
In contrast, NewType declares one type to be a subtype of another. Doing Derived = NewType('Derived', Original) will make the static type checker treat Derived as a subclass of Original, which means a value of type Original cannot be used in places where a value of type Derived is expected. This is useful when you want to prevent logic errors with minimal runtime cost.

> It is possible to declare the return type of a callable without specifying the call signature by substituting a literal ellipsis for the list of arguments in the type hint: Callable[..., ReturnType].

## Generic Function
```python
from typing import Sequence, TypeVar

T = TypeVar('T')      # Declare type variable

def first(l: Sequence[T]) -> T:   # Generic function
    return l[0]
```

## Generic Type

```python
from typing import TypeVar, Generic
from logging import Logger

T = TypeVar('T')
class LoggedVar(Generic[T]): # Devired from a ABC 'Generic'
    def __init__(self, value: T, name: str, logger: Logger) -> None:
        self.name = name
        self.logger = logger
        self.value = value

    def set(self, new: T) -> None:
        self.log('Set ' + repr(self.value))
        self.value = new

    def get(self) -> T:
        self.log('Get ' + repr(self.value))
        return self.value

    def log(self, message: str) -> None:
        self.logger.info('%s: %s', self.name, message)
```

**Each type variable argument to Generic must be distinct.** This is thus invalid:
```py
from typing import TypeVar, Generic
...

T = TypeVar('T')

class Pair(Generic[T, T]):   # INVALID
    ...
```

[Nominal vs structural subtyping](https://docs.python.org/3/library/typing.html#nominal-vs-structural-subtyping)

## Literal

typing.Literal
A type that can be used to indicate to type checkers that the corresponding variable or function parameter has a value equivalent to the provided literal (or one of several literals). For example:

def validate_simple(data: Any) -> Literal[True]:  # always returns True
    ...

MODE = Literal['r', 'rb', 'w', 'wb']
def open_helper(file: str, mode: MODE) -> str:
    ...

open_helper('/some/path', 'r')  # Passes type check
open_helper('/other/path', 'typo')  # Error in type checker
Literal[...] cannot be subclassed. At runtime, an arbitrary value is allowed as type argument to Literal[...], but type checkers may impose restrictions. See PEP 586 for more details about literal types.

**New in version 3.8.**