from typing import Sized
from . import *
from .lexer import IterBuffer, TokenLiteral, Token

# 可能的AST的类型
AstLiteral = Literal[
    'Dot',          # 附加类型, 如果一个 Identifier 跟在 . 的后面, 它就会有这个类型
    'Ellipsis',     # 附加类型, 如果一个 Identifier 在 ... 的前面, 它就会有这个类型
    'ExprList',     # 主类型, 一个(<expr>*). sons 内存放的就是子节点(们)
    'Identifier',   # 主类型, 一个 Identifier, 包括所有的 keyword.
    'Quote',        # 主类型, 只有 ` 才会被赋予这个类型

    # 主类型, 分别是对应的字面量. sons[0]存放了它们对应的Python值(int, str, bool, str)
    'Number', 'String', 'Boolean', 'Character', 

    'Program'       # 主类型, 适用于整个程序或者define/lambda/let的body那种不带括号的一堆表达式, 即<expr>*
]

class AstNode:
    def __init__(self, types: Union[AstLiteral, List[AstLiteral]], sons: Union[List['AstNode'], T]):
        self.types = make_list(types)
        self.sons = make_list(sons)
    
    def __getitem__(self, key: int) -> Union['AstNode', T]:
        return self.sons[key]
    
    def is_type(self, type: AstLiteral) -> bool:
        return type in self.types
    
    def in_type(self, types: List[AstLiteral]) -> bool:
        return len(set(types).union(set(self.types))) != 0

    def data(self) -> T:
        if isinstance(self.sons[0], AstNode):
            raise RuntimeError(f'Not an atom node: {self.sons}')
        return self.sons[0]

    def to_dict(self) -> Dict[Literal['types', 'sons'], Any]:
        sons_dict = None
        if not isinstance(self.sons[0], AstNode):
            sons_dict = self.sons[0]
        else: sons_dict = list(map(AstNode.to_dict, self.sons))

        return {
            'types': self.types,
            'sons': sons_dict
        }
    
    def __str__(self):
        return str(self.to_dict())
    __repr__ = __str__

    def __len__(self):
        return len(self.sons)

    # 转换为 Scheme 程序
    # 或者应该挑出来写成一个 unparse ?
    def to_scheme(self) -> str:
        if self.is_type('ExprList'):
            return f'({" ".join([s.to_scheme() for s in self.sons])})'
        elif self.is_type('Program'):
            return "\n".join([s.to_scheme() for s in self.sons])
        elif self.is_type('Quote'):
            return f"'{self.sons[0].to_scheme()}"
        else:
            return str(self.data())

# 接收一个解析BNF <x> 的 Parser, 然后把它一直解析到 <end_token> 为止. 也就是说解析 <x>* <end_token>
# 当 end_token 为单个 TokenLiteral 时,                         返回解析出来的 List[AstNode]
# 当 end_token 是一堆 TokenLiteral 时(一个 List[TokenLiteral]), 返回元组 (List[AstNode], Token); 其中第二个返回值为停止时的 Token
def pluralize(token_buffer: IterBuffer, parser: Callable[[IterBuffer], AstNode], end_token: Union[TokenLiteral, List[TokenLiteral]]) -> Union[List[AstNode], Tuple[List[AstNode], Token]]:
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

def parse(token_buffer: IterBuffer) -> AstNode:
    token, data = next(token_buffer)
    next_dot = False

    # 判断紧跟着一个 . 的expr; 语法 (<expr>+ . <expr>) 在 quote 里用来构造一个不以()结尾的列表
    # 把 'Dot' 附加类型附到后面的 <expr> 节点里
    def make_ast(types: Union[List[AstLiteral], AstLiteral], sons) -> AstNode:
        if next_dot:
            types = make_list(types) + ['Dot']
        return AstNode(types, sons)
    # print(f'Now token: ({token}, {data})')

    # expression list
    if token == 'LeftBracket':
        return make_ast('ExprList', pluralize(token_buffer, parse, 'RightBracket')) 

    # atoms
    elif token == 'Quote':
        return make_ast('Quote', parse(token_buffer))
    elif token == 'Number':
        return make_ast('Number', int(data))
    elif token == 'String':
        return make_ast('String', str(data))
    elif token == 'Boolean':
        return make_ast('Boolean', data == '#t')
    elif token == 'Character':
        return make_ast('Character', data)
    elif token == 'Identifier':
        # 特判一个紧跟着...的Identifier, 方便后面marco pattern判断
        if token_buffer.now() == ('Identifier', '...'):
            next(token_buffer) # eat ...
            return make_ast(['Identifier', 'Ellipsis'], str(data))
        else:
            return make_ast('Identifier', str(data))
    elif token == 'Dot':
        next_dot = True
    
    raise RuntimeError('Unknown Token')

# parse 的入口
def parse_program(token_buffer: IterBuffer) -> AstNode:
    # AstNode('Program')
    sub_exprs: List[AstNode] = []
    try:
        while True:
            sub_exprs.append(parse(token_buffer))
    except StopIteration:
        pass
    return AstNode('Program', sub_exprs)