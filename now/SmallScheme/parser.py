from os import sendfile
from typing import Generic, Iterator, Sized, overload, Type
from . import *
from .lexer import IterBuffer, TokenLiteral, Token
from abc import ABC, abstractmethod

class AstNode:
    def __init__(self, dot: bool=False, ellipsis: bool=False) -> None:
        self.dot, self.ellipsis = dot, ellipsis

    def is_dot(self) -> bool: return self.dot
    def is_ellipsis(self) -> bool: return self.ellipsis

    @abstractmethod
    def to_scheme(self) -> str:
        raise RuntimeError('Unsupported operation')

    @abstractmethod
    def to_dict(self) -> Dict[str, Any]:
        raise RuntimeError('Unsupported operation')

    def as_node(self, node_class: Type['LeafNodeType']) -> 'LeafNodeType':
        if isinstance(self, node_class):
            return self # type: ignore
        raise TypeError('Not an special node')


class AtomNode(AstNode, Generic[T]):
    def __init__(self, data: T, dot: bool=False, ellipsis: bool=False) -> None:
        super().__init__(dot, ellipsis)
        self._data = data

    def is_data(self, data: T) -> bool:
        return self._data == data
    
    def data(self) -> T:
        return self._data
    
    def to_scheme(self) -> str:
        return str(self._data)
    
    def to_dict(self) -> Dict[str, Any]:
        return self._data

class IdentifierNode(AtomNode[str]): pass
class NumberNode(AtomNode[int]):     pass
class StringNode(AtomNode[str]):     pass
class BooleanNode(AtomNode[bool]):   pass
class CharacterNode(AtomNode[str]):  pass

class ExprListNode(AstNode):
    def __init__(self, sons: List[AstNode]) -> None:
        super().__init__()
        self.sons = sons

    def as_list(self) -> List[AstNode]:
        return self.sons

    def split(self) -> Tuple[AstNode, List[AstNode]]:
        return self.sons[0], self.sons[1:]

    def first_is(self, identifier: str) -> bool:
        first_ast = self.sons[0]
        return isinstance(first_ast, IdentifierNode) and first_ast.is_data(identifier)

    def is_dot(self) -> bool:
        if len(self.sons) == 0:
            return False
        return self.sons[-1].is_dot()
    
    def is_ellipsis(self) -> bool:
        if len(self.sons) == 0:
            return False
        return self.sons[-1].is_ellipsis()
    
    def is_quote(self) -> bool:
        return self.first_is('quote')

    def to_scheme(self) -> str:
        return f'({" ".join([o.to_scheme() for o in self.sons])})'

    def to_dict(self) -> Dict[str, Any]:
        return {
            '0-type': 'ExprList',
            '1-sons': [o.to_dict() for o in self.sons]
        }

LeafNodeType = TypeVar('LeafNodeType', IdentifierNode, NumberNode, StringNode, BooleanNode, CharacterNode, ExprListNode)


# 接收一个解析BNF <x> 的 Parser, 然后把它一直解析到 <end_token> 为止. 也就是说解析 <x>* <end_token>
# 当 end_token 为单个 TokenLiteral 时,                         返回解析出来的 List[AstNode]
# 当 end_token 是一堆 TokenLiteral 时(一个 List[TokenLiteral]), 返回元组 (List[AstNode], Token); 其中第二个返回值为停止时的 Token
@overload 
def pluralize(token_buffer: IterBuffer, parser: Callable[[IterBuffer], AstNode], end_token: TokenLiteral) -> List[AstNode]: ...
@overload
def pluralize(token_buffer: IterBuffer, parser: Callable[[IterBuffer], AstNode], end_token: List[TokenLiteral]) -> Tuple[TokenLiteral, List[AstNode]]: ...
def pluralize(token_buffer, parser, end_token):
    results = []
    ends = make_list(end_token)
    need_return_end = type(end_token) == type([])

    while True:
        token, _ = token_buffer.now()
        
        if token in ends:
            next(token_buffer)
            return (results, token) if need_return_end else results

        else:
            results.append(parser(token_buffer))

def parse_dot(token: IterBuffer) -> AstNode:
    next_dot = parse(token)
    next_dot.dot = True
    return next_dot

def parse(token_buffer: IterBuffer) -> AstNode:
    token, data = next(token_buffer)
    
    if token == 'LeftBracket':
        return ExprListNode(pluralize(token_buffer, parse, 'RightBracket'))

    if token == 'Number':    return NumberNode(data)
    if token == 'String':    return StringNode(data)
    if token == 'Character': return CharacterNode(data)
    if token == 'Boolean':   return BooleanNode(data)

    if token == 'Identifier':
        id_node = IdentifierNode(data)
        # 特判一个紧跟着...的Identifier, 方便后面marco pattern判断
        if token_buffer.now() == ('Identifier', '...'):
            next(token_buffer) # eat it
            id_node.ellipsis = True
        return id_node
    
    if token == 'Dot':
        next_dot = parse(token_buffer)
        next_dot.dot = True
        return next_dot
    
    if token == 'Quote':
        quote_node = IdentifierNode('quote')
        quoted_node = parse(token_buffer)
        return ExprListNode([quote_node, quoted_node])
    
    raise RuntimeError('Invaild Token')

# parse 的入口
def parse_program(token_buffer: IterBuffer) -> AstNode:
    begin_node = IdentifierNode('begin')
    begin_body: List[AstNode] = []
    try:
        while True:
            begin_body.append(parse(token_buffer))
    except StopIteration:
        return ExprListNode(concat([begin_node], begin_body))