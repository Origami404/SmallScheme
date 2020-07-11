from typing import TypeVar, overload, List

# Overload 

T = TypeVar('T')

# 重载必须提供签名, 调整顺序
@overload
def f(n: List[T]) -> List[T] :...
@overload 
def f(n: T) -> List[T] :...

def f(n):
    return n if isinstance(n, list) else [n]

if __name__ == "__main__":
    x = f(1)
    y = f('2')
    z = f(['2', 3])