from . import *
from .scheme_parser import AstNode
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

self_eval_types = ['Number', 'String', 'Boolean', 'Character']

def eval(ast: AstNode, env: Environment) -> ValueType:
    # self-eval
    if ast.in_type(self_eval_types):
        return ast.data()

    if ast.is_type('Quote'):
        return eval_quote(ast[0], env)
    
    # form: without marco
    if ast.is_type('ExprList'):
        proc_ast, arg_asts = ast[0], ast[1:]

        def is_special(keyword: str) -> bool:
            return proc_ast.is_type('Identifier') and proc_ast.data() == keyword

        if is_special('lambda'):
            return make_lambda(arg_asts, env)
        elif is_special('set!'):
            name, expr = arg_asts[0].data(), arg_asts[1]
            env.bind(name, eval(expr, env))
            return env.get(name)
        elif is_special('if'):
            cond = eval(arg_asts[0], env)
            return eval(arg_asts[1 if cond else 2], env)

    if ast.is_type('Program'):
        fronts, end = ast.sons[:-1], ast[-1]
        for front in fronts:
            eval(front, env)
        return eval(end, env)
    
def eval_quote(ast: AstNode, env: Environment) -> ValueType:
    if ast.in_type(self_eval_types):
        return ast.data()
    elif ast.is_type('Identifier'):
        return Symbol(ast.data())
    elif ast.is_type('ExprList'):
        return lmap(bind_tail(eval_quote, env), ast.sons)

def make_lambda(operands: List[AstNode], env: Environment) -> Procedure:
    formals, bodys = operands[0], operands[1:]

    def normal_bind(formals: List[str], exprs: List[ValueType]) -> Dict[str, ValueType]:
        if len(formals) != len(exprs):
            raise RuntimeError('Argumen amount too many or too less')
        ret = {}
        for f, v in zip(formals, exprs):
            ret[f] = v
        return ret

    def to_str(ids: List[AstNode]) -> List[str]:
        return lmap(lambda x: x.data(), ids)

    binder: Callable[[List[ValueType]], ValueType] = None
    if formals.is_type('Dot'):
        normal_formals = to_str(formals[:-1])
        left = formals[-1].data()

        def bind(exprs):
            nbind = normal_bind(normal_formals, exprs[:len(normal_formals)])
            nbind[left] = exprs[len(normal_formals) + 1 :]
            return nbind

        binder = bind

    elif formals.is_type('Identifier'):
        binder = lambda exprs: {formals.data(): exprs}

    else:
        binder = lambda exprs: normal_bind(formals, exprs) 

    def find_free(ast: AstNode) -> List[str]:
        if ast.is_type('Identifier') and ast.data() not in formals:
            return [ast.data()]
        elif ast.is_type('ExprList'):
            return reduce(concat, map(find_free, ast.sons), [])
        return []
    
    free_vars = reduce(concat, map(find_free, bodys), [])
    
    def proc(exprs: List[ValueType], env: Environment) -> ValueType:
        now_env = Environment(env, free_vars).update(binder(exprs))
        for body in bodys[:-1]:
            eval(body, now_env)
        return eval(bodys[-1], now_env)
    
    return proc

