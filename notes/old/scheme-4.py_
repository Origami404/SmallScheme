from re import compile as regex
from typing import List, Dict, Callable, Generator, Iterable, TypeVar, Union, Tuple
from functools import reduce
T, X, Y = TypeVar('T'), TypeVar('X'), TypeVar('Y') 

# ========================= Lexer ======================================

def tokenize(string: str) -> Iterable[Tuple[str, str]]:
    id_inital = r'a-zA-Z!$%&*\/:<=>?^_~'
    id_subsequent = f'{id_inital}0-9+-.@'

    ordered_token_pattern = [
        
        ('Quote',             "'"),
        # ('Quasiquote',        '`'),
        # ('CommaAt',           ',@'),
        # ('Comma',             ','),
        # ('VectorLeftBracket', '#('),
        ('LeftBracket',       '('),
        ('RightBracket',      ')'),
        ('String',            regex(r'"(([^\\]|\"|\\)*?)"')),
        ('Character',         regex(r'#\\(.)')),
        ('Boolean',           regex(r'(#t|#f)')),
        ('Number',            regex(r'([+-]*\d+)')),
        ('Identifier',        regex(f'([{id_inital}][{id_subsequent}]*)')),
        ('PeculiarIdentifier',regex(r'(\+|-|\.\.\.)')), # 也许可以去掉这个分类, 直接把这个也叫Identifier
        ('Dot',               '.') # 有三个连续的点的话优先解析为...
    ]

    def match(str):
        for name, pattern in ordered_token_pattern:
            if type(pattern) == type(""):
                if str.startswith(pattern):
                    return name, '', len(pattern)
            else:
                match = pattern.match(str)
                if match:
                    # print(match.group(0))
                    return name, match.group(1), len(match.group(0))
        return 'Error', 'Not vaild token', 0
    
    # TODO: Add line/char number support
    while len(string) != 0:
        string = string.strip(' ')
        token_name, token_data, token_len = match(string)
        string = string[token_len:]
        yield (token_name, token_data)

# ========================= Parser ======================================

class IterBuffer:
    def __init__(self, generator, end_with=None, *args):
        self.iterator = generator(*args)
        self.end_with = end_with
        # self.begin = True

        self._now, self._after = None, None
        try:
            self._now = next(self.iterator)
            self._after = next(self.iterator)
        except StopIteration:
            raise RuntimeError('Not enough elements: less than 2')
    
    def now(self):
        return self._now
    
    def after(self):
        return self._after
    
    def __iter__(self):
        return self
    
    def __next__(self):
        # if self.begin:
        #     self.begin = False
        #     return self._now
        if self._now == self.end_with:
            raise StopIteration
        
        old, new = self._now, None
        try:
            new = next(self.iterator)
        except StopIteration:
            self._now, self._after = self._after, self.end_with
        else:
            self._now, self._after = self._after, new
        return old
    
def not_vaild(productor_name: str, *args):
    raise RuntimeError(f'Not vaild {productor_name}: {args}')

def make_list(data: any) -> list:
    if type(data) == type([]):
        return data
    else: return [data]

def pluralize(token_buffer: IterBuffer, func: Callable[[X], Y], end_token: T) -> Union[List[Y], Tuple[List[Y], T]]:
    results = []
    ends = make_list(end_token)
    need_return_end = type(end_token) == type([])

    while True:
        token, _ = token_buffer.now()
        
        if token in ends:
            next(token_buffer)
            return (results, token) if need_return_end else results

        else:
            results.append(func(token_buffer))

# like typed variable
class AstNode:
    def __init__(self, types, sons, extra_data=None):
        self.types = make_list(types)
        self.sons = make_list(sons)
        self.extra_data = extra_data
    
    def __getitem__(self, key):
        if type(key) == type(''):
            return self.extra_data[key]
        elif type(key) == type(0):
            return self.sons[key]
        else:
            raise NotImplementedError('Key should either a int or a string')
    
    def is_type(self, type):
        return type in self.types
    
    def in_type(self, types):
        return len(set(types).union(set(self.types))) != 0

    def to_dict(self):
        sons_dict = None
        if type(self.sons[0]) != type(AstNode('', [])):
            sons_dict = self.sons[0]
        else: sons_dict = list(map(AstNode.to_dict, self.sons))

        return {
            'types': self.types,
            'sons': sons_dict
        }

def parse(token_buffer: IterBuffer):
    token, data = next(token_buffer)

    # print(f'Now token: ({token}, {data})')

    # expression list
    if token == 'LeftBracket':
        return AstNode('ExprList', pluralize(token_buffer, parse, 'RightBracket')) 

    # atoms
    elif token == 'Quote':
        return AstNode('Quote', parse(token_buffer))
    elif token == 'Number':
        return AstNode('Number', int(data))
    elif token == 'String':
        return AstNode('String', str(data))
    elif token == 'Identifier' or token == 'PeculiarIdentifier':
        # 特判一个紧跟着...的Identifier, 方便后面marco pattern判断
        if token_buffer.now() == ('PeculiarIdentifier', '...'):
            next(token_buffer) # eat ...
            return AstNode(['Identifier', 'Ellipsis'], str(data))
        else:
            return AstNode('Identifier', str(data))
    elif token == 'Boolean':
        return AstNode('Boolean', data == '#t')
    elif token == 'Character':
        return AstNode('Character', data)

# ========================= Runtime Object ======================================

NullEnvironment = {}

class Environment:
    def __init__(self, father: Environment, inital_binds={}, inital_marcos={}):
        self.father = father
        self.binds = inital_binds.copy() # py默认arg pass-by-reference
        self.marcos = inital_marcos.copy()
    
    def get_var(self, key): 
        if key not in self.binds:
            if not self.father:
                raise RuntimeError('Unbound vairable')
            return self.father.get_var(key)
        
        return self.binds[key]
    
    def get_marco(self, key): 
        if key not in self.marcos:
            if not self.father:
                raise RuntimeError('Unbound keyword')
            return self.father.get_marco(key)

        return self.marcos[key]

    def bind_var(self, key, val): self.binds[key] = val
    
    def bind_marco(self, key, rule): self.marcos[key] = rule

    def is_marco(self, name): 
        return name in self.marcos or (self.father and self.father.is_marco(name))

    def is_bind(self, name): 
        return name in self.binds  or (self.father and self.father.is_bind(name))
        
    def __getitem__(self, name):
        return self.get_var(name)

    def __setitem__(self, name, value):
        return self.bind_var(name, value)
    
# def formal_amount(except_len: int):
#     def decorator(func):
#         def wrapper(operands, *args):
#             if len(operands) != except_len:
#                 raise RuntimeError(f'Too many or too little arguments. Except: {except_len}, Given: {len(operands)}')
#             return func(operands, *args)
#         return wrapper
#     return decorator

# TODO: 增加闭包支持
def make_lambda(operands: List[AstNode], now_env: Environment) -> Callable[[any, Environment], any]:
    def extract(now: AstNode, formals: List[str]) -> List[str]:
        if now.is_type('ExprList'):
            return list(reduce(concat, map(bind_env(extract, formals), now.sons), []))
        
        elif now.is_type('Identifier') and now[0] not in formals:
            return [now[0]]
        
        return []

    formals, bodys = operands[0], operands[1:]
    
    def bind_args(names: List[str], values: List[any], list_formal: str=None) -> Dict[str, any]:
        name_len, val_len = len(names), len(values)
        if name_len > val_len or (not list_formal and name_len != val_len):
            raise RuntimeError(f'Too many or too little arguments. Except: {name_len}, Given: {val_len}')

        binds = {}
        for name, i in enumerate(names):
            binds[name] = values[i]
        
        if list_formal:
            binds[list_formal] = values[name_len:]
        
        return binds
    
    EnvPrecursor = Dict[str, any]
    binder: Callable[[any, Environment], EnvPrecursor] = None
    # saver:  Callable[[], EnvPrecursor] = None

    formal_names: List[str] = None

    # 根据 formal 列表的不同形式构造将参数绑定到值上的 binder
    # <formals> -> <variable> 此时参数列表就会allocate一个list并在新env中bind到variable上
    if formals.is_type('Identifier'):
        formal_names = [formals[0]]
        binder = lambda values, env: bind_args([], values, formal_names[0])
    else:
        formal_names = list(map(lambda x: x[0], formals.sons))

        # <formals> -> (<variable>+ . <variable>)
        if len(formal_names) >= 3 and formal_names[-2] == '.':
            normal_formals = formal_names[:-2]
            list_formal = formal_names[-1]
            binder = lambda values, env: bind_args(normal_formals, values, list_formal)
        
        # <formals> -> (<variable>*)
        else:
            binder = lambda values, env: bind_args(formal_names, values)
    
    free_vars = reduce(concat, map(extract, bodys))
    
    def saver():
        ret = {}
        for var in free_vars:
            ret[var] = now_env.get_var()



# ========================= Marco ======================================

def lmap(f: Callable[[X], Y], iter: Iterable[X]) -> List[Y]:
    return list(map(f, iter))

def bind_env(f, *args) -> Callable:
    return lambda x: f(x, *args)

def not_none(iter) -> Iterable:
    return filter(lambda x: x != None, iter)

#           ============= Pattern ==================


def union(dict1: Dict[X, Y], dict2: Dict[X, Y]) -> Dict[X, Y]:
    ret = {}
    ret.update(dict1)
    ret.update(dict2)
    return ret
    # return dict(set(dict1).union(set(dict2)))

def concat(list1: List[X], list2: List[X]) -> List[x]:
    return list1 + list2

def make_pattern_list(ast: AstNode, literals: List[str]):
    last = ast[-1]
    if last.is_type('Ellispsis'):
        return make_pattern_ellipsis_list(ast, literals)
    else:
        return make_pattern_normal_list(ast, literals)

def make_pattern_normal_list(ast: AstNode, literals: List[str]):
    sub_patterns = lmap(bind_env(make_pattern_tree, literals), ast)

    def match(exprs):
        if not exprs.is_type('ExprList') or len(exprs.sons) != len(sub_patterns):
            return False, {}
        
        # 这块可能对性能比较敏感, 采取迭代形式, 实现短路
        total_result = {}
        for sub_pattern, expr in zip(sub_patterns, exprs):
            is_match, result = sub_pattern(expr)

            if not is_match:
                return False, {}
            
            total_result = union(total_result, result)
        
        return True, total_result
    
    return match

def make_pattern_ellipsis_list(ast: AstNode, literals: List[str]):
    sub_patterns = lmap(bind_env(make_pattern_tree, literals), ast)
    normal_subs, ellipsis_sub = sub_patterns[:-1], sub_patterns[-1]

    def match(exprs):
        if not exprs.is_type('ExprList') or len(normal_subs.sons) > len(sub_patterns):
            return False, {}
        
        total_result = {}
        for sub_pattern, expr in zip(normal_subs, exprs):
            is_match, result = sub_pattern(expr)

            if not is_match:
                return False, {}
            
            total_result = union(total_result, result)
        
        is_match, result = ellipsis_sub(exprs[len(normal_subs):])
        if not is_match:
            return False, {}
        
        return True, union(total_result, result)

    return match

# def make_pattern_ellipsis(ast, literals):
#     return lambda exprs: (True, {'...': exprs})

def make_pattern_identifier(ast: AstNode, literals: List[str]):
    return lambda expr: (True, {ast.sons[0]: expr})

def make_pattern_literal(ast: AstNode, literals: List[str]):
    return lambda expr: expr.is_type('Identifier') and expr[0] == ast[0], {}

def make_pattern_tree(ast: AstNode, literals: List[str]):
    if ast.is_type('ExprList'):
        return make_pattern_list(ast, literals)
    elif ast.is_type('Identifier'):
        name = ast[0]
        if name in literals:
            return make_pattern_literal(ast, literals)
        # elif name == '...': 
        #     return make_pattern_ellipsis(ast, literals) 
        else:
            return make_pattern_identifier(ast, literals)

#           ============= Template ==================

def transform(template: AstNode, lexical_binds: Dict[str, AstNode]) -> AstNode:
    if template.is_type('ExprList'):
        return AstNode('ExprList', lmap(bind_env(transform, lexical_binds), template.sons))
    
    elif template.is_type('Identifier') and template[0] in lexical_binds:
        return lexical_binds[template[0]]
    
    return template

#           ============= Form maker ==================

def make_syntax_rule(operands: List[AstNode]) -> Callable[[AstNode], AstNode]:
    literals = lmap(lambda node: node[0], operands[0])
    cases = lmap(lambda node: (make_pattern_tree(node[0], literals), node[1]), operands[1:])

    def use(expr: AstNode) -> AstNode:
        for pattern, template in cases:
            is_match, lexical_binds = pattern(expr)

            if is_match:
                return transform(template, lexical_binds)

        raise RuntimeError('Invaild marco use.')

    return use

def parse_marco_define(operands: AstNode, now_env: Environment) -> None:
    name, body = operands[0], operands[1]

    syntax_rule_operands = body.sons[1:] # skip the keyword 'syntax-rule'

    now_env.bind_marco(name, make_syntax_rule(syntax_rule_operands))

# ========================== Eval-Apply Loop =================================

def eval(ast: AstNode, now_env=NullEnvironment):
    # self-eval:
    if ast.in_type(['Quote', 'Number', 'String', 'Identifier', 'Boolean', 'Character']):
        return ast[0]

    
    # form
    operator, operands = ast[0], ast[1:]

    # special form
    def is_keyword(keyword): return operator[0] == keyword

    # core special form
    if is_keyword('lambda'):
        return make_lambda(operands, now_env)
    elif is_keyword('set!'):
        name, expr = operands[0], operands[1]
        now_env.bind_var(name, eval(expr, now_env))
        return now_env.get_var(name)
    elif is_keyword('if'):
        test_result = eval(operands[0], now_env)
        if test_result:
            return eval(operands[1], now_env)
        else:
            if len(operands) == 3:
                return eval(operands[2], now_env)
            else:
                return False
    
    # SF: marco definitaion
    elif is_keyword('define-syntax'):
        parse_marco_define(operands, now_env)


    # SF: user-defined keyword
    operator_name = operator[0]
    if now_env.is_marco(operator_name): # 也许改名叫is_keyword更好?
        marco = now_env.get_marco(operator_name)
        return eval(marco(ast), now_env)
    else:
        # normal form: procedure application
        operand_values = lmap(bind_env(eval, now_env), operands)
        proc = now_env.get_var(operator_name)
        return proc(operand_values, now_env)


#           ============= Pattern ==================

import pprint
fprint = pprint.PrettyPrinter(2).pprint

if __name__ == "__main__":
    program = '(a (b c ...))'
    token_buffer = IterBuffer(tokenize, None, program)  
    # print(list(token_buffer))
    ast = parse(token_buffer)
    fprint(ast.to_dict())
    pattern = make_pattern_tree(ast,[])

    new_ast = parse(IterBuffer(tokenize, None, '((a b) (f (g 2 3 4)))'))

    is_match, result = pattern(new_ast)
    # print(str(new_ast))
    fprint(result['c'].to_dict())

    