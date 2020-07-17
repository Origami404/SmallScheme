from typing import overload
from . import *
from .scheme_parser import AstLiteral, AstNode
from .pre_eval import Environment

class Symbol:
    def __init__(self, symbol: str):
        self.symbol = symbol
    
    def data(self) -> str:
        return self.symbol

# AtomValueType = Union[bool, str, int, Symbol]
# Procdure = Callable[[List[Union[AtomValueType, 'Procdure']], Environment], Union[AtomValueType, 'Procdure']]
# ValueType = Union[AtomValueType, Procdure]

Procedure = Callable[[List['ValueType'], Environment], 'ValueType']
ValueType = Union[bool, str, int, Symbol, Procedure, list]

AtomAstLiteral = Literal['Identifier', 'Number', 'String', 'Boolean']

self_eval_types: List[AstLiteral] = ['Number', 'String', 'Boolean', 'Character']

def split(form: AstNode) -> Tuple[AstNode, List[AstNode]]:
    return form.sons[0], form.sons[:1]

def not_empty(l: List[Any], message: str='Bad syntax') -> None:
    assuming_len(l, 0, message)

def assuming_type(types: List[AstLiteral], ast: AstNode, ) -> None:
    if not ast.in_type(types):
        raise RuntimeError(f'Wrong type, except {types}, given: {ast.types}')

def assuming_len(l: List[Any], length: int, message: str='Bad syntax') -> None:
    if len(l) != length:
        raise RuntimeError(message)

@overload
def as_atom(type: Literal['Identifier', 'String'], ast: AstNode) -> str: ...
@overload
def as_atom(type: Literal['Number'], ast: AstNode) -> int: ...
@overload
def as_atom(type: Literal['Boolean'], ast: AstNode) -> bool: ...

def ast_atom(type, ast):
    assuming_type(type, ast)
    return type.data()

def eval(ast: AstNode, env: Environment) -> ValueType:
    # self-eval
    if ast.in_type(self_eval_types):
        return ast.data()

    if ast.is_type('Quote'):
        return eval_quote(ast[0], env)

    if ast.is_type('ExprList'):
        proc, args = split(ast)

        def is_keyword(name: str) -> bool:
            return proc.is_type('Identifier') and proc.data() == name

        # sepcial form
        if is_keyword('define-var'):
            eval_define(args, env)
        elif is_keyword('lambda'):
            eval_lambda(args, env)
        elif is_keyword('set!'):
            eval_set(args, env)
        elif is_keyword('if'):
            eval_if(args, env)
        elif is_keyword('cond'):
            eval_cond(args, env)
        elif is_keyword('begin'):
            eval_begin(args, env)
        
        # normal form: application
        eval_applicaiton(proc, args, env)

    if ast.is_type('Program'):
        new_env = Environment(env) # new scope
        return eval_in_order(ast.sons, new_env)
    
    raise RuntimeError('Invaild ast.')
    

def eval_define(operands: List[AstNode], env: Environment) -> ValueType:
    assuming_len(operands, 2)
    
    name_ast, var_ast = operands[0], operands[1]
    name, var = as_atom('Identifier', name_ast), eval(var_ast, env)
    env.bind(name, var)
    return var

def eval_lambda(operands: List[AstNode], env: Environment) -> ValueType:
    return make_lambda(operands, env)

def eval_set(operands: List[AstNode], env: Environment) -> ValueType:
    assuming_len(operands, 2)

    name_ast, var_ast = operands[0], operands[1]
    name, var = as_atom('Identifier', name_ast), eval(var_ast, env)
    
    if env.has(name):
        env.bind(name, var)
    else:
        raise RuntimeError(f'Unbound name in set!: {name}')

    return var


def eval_in_order(asts: List[AstNode], env: Environment) -> ValueType:
    not_empty(asts, 'Empty body is not be allowed')

    front, last = asts[:-1], asts[-1]
    for ast in front:
        eval(ast, env)
    return eval(last, env)    






def eval_quote(ast: AstNode, env: Environment) -> ValueType:
    if ast.in_type(self_eval_types):
        return ast.data()
    elif ast.is_type('Identifier'):
        return Symbol(ast.data())
    elif ast.is_type('ExprList'):
        return lmap(bind_tail(eval_quote, env), ast.sons)
    
    raise RuntimeError('Invaild quote.')

def make_lambda(operands: List[AstNode], env: Environment) -> Procedure:

    formals, bodys = operands[0], operands[1:]

    BindInfo = Dict[str, ValueType]

    def normal_bind(formals: List[str], exprs: List[ValueType]) -> BindInfo:
        if len(formals) != len(exprs):
            raise RuntimeError('Argumen amount too many or too less')
        ret = {}
        for f, v in zip(formals, exprs):
            ret[f] = v
        return ret

    def to_str(ids: List[AstNode]) -> List[str]:
        return lmap(lambda x: x.data(), ids)

    binder: Callable[[List[ValueType]], BindInfo]
    if formals.is_type('Dot'):
        normal_formals = to_str(formals.sons[:-1])
        left = formals[-1].data()

        def bind(exprs: List[ValueType]) -> BindInfo:
            nbind = normal_bind(normal_formals, exprs[:len(normal_formals)])
            nbind[left] = exprs[len(normal_formals) + 1 :]
            return nbind

        binder = bind

    elif formals.is_type('Identifier'):
        binder = lambda exprs: {formals.data(): exprs} # type: ignore

    else:
        binder = lambda exprs: normal_bind(formals, exprs) # type: ignore

    def find_free(ast: AstNode) -> List[str]:
        if ast.is_type('Identifier') and ast.data() not in formals:
            return [ast.data()]
        elif ast.is_type('ExprList'):
            return reduce(concat, map(find_free, ast.sons), [])
        return []
    
    free_names = reduce(concat, map(find_free, bodys), [])
    free_vars = lmap(lambda name: env.get(name), free_names)

    def proc(exprs: List[ValueType], env: Environment) -> ValueType:
        now_env = Environment(env, free_vars).update(binder(exprs))
        for body in bodys[:-1]:
            eval(body, now_env)
        return eval(bodys[-1], now_env)
    
    return proc
     
