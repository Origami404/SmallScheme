# partial generic function specialization

Here is a small sample which is very like an issue in Python's typing ([559](https://github.com/python/typing/issues/599#issuecomment-586007066))

```python
T = TypeVar('T')

@overload
def as_list(val: List[T]) -> List[T]: ...

@overload
def as_list(val: T) -> List[T]: ...

def as_list(val):
    return val if isinstance(val, list) else [val]
```

It works fine until I use a variable with nondeterministic `Union` type (such as function's paraparameters):

```py
x = as_list(2)   # x: List[int] # Not a `Union` type, fine. 
y = as_list([2]) # y: List[int] # fine.

z: Union[int, List[int]] = 2 
zl = as_list(zl) # zl: List[int] # Deterministic `Union`, fine.

def f(a: Union[T, List[T]]) -> List[T]:
    # Shouldn't `a` be with a type 'List[T]'?
    a_as_list = as_list(a) # a: Union[List[T], List[List[T]]] 
    ...
    return ...
```

In C++, I could use partial template specialization and complier will select the right function for me, but how can I do (or ask the interrupter to do) this in Python with typing?

```cpp
template<typename T>
List<T> as_list(T x);

template<typename T>
List<T> as_list(List<T> x);
```