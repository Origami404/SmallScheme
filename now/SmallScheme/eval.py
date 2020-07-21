from SmallScheme import bind_tail
from .runtime import *
from .parser import *
from . import *


def basic_runtime() -> EvalEnv:
    return Environment(None)

# 啊啊啊啊我好想要单子啊!!!
# 我不想每次都手动bind_tail一个environment啊!!!
# 我要fish arrow!!!!

def eval(ast: AstNode, env: EvalEnv) -> SchValue:
    
    # Identifier
    if isinstance(ast, IdentifierNode):
        return env.get(ast.data())

    # self-eval: Number Boolean Character String
    if isinstance(ast, AtomNode):
        return eval_atom(ast, env)
    
    if isinstance(ast, ExprListNode):
        return eval_expr_list(ast, env)
    
    raise RuntimeError('Can not eval. Unknown ast type. ')

def eval_list(asts: List[AstNode], env: EvalEnv) -> List[SchValue]:
    return lmap(bind_tail(eval, env), asts)

def eval_atom(ast: AtomNode, env: EvalEnv) -> SchValue:
    if isinstance(ast, NumberNode):
        return Number(ast.data())
    elif isinstance(ast, StringNode):
        return String(ast.data())
    elif isinstance(ast, BooleanNode):
        return Boolean(ast.data())
    elif isinstance(ast, CharacterNode):
        return Character(ast.data()) 
    raise RuntimeError('Not an atom node')
    

def eval_expr_list(ast: ExprListNode, env: EvalEnv) -> SchValue:
    operator_ast, operand_asts = ast.split()

    if isinstance(operator_ast, IdentifierNode):
        name = operator_ast.data()
        
        if name == 'define':
            return eval_define(operand_asts, env)
        elif name == 'lambda':
            return eval_lambda(operand_asts, env)
        elif name == 'set!':
            return eval_set(operand_asts, env)
        elif name == 'if':
            return eval_if(operand_asts, env)
        elif name == 'cond':
            return eval_cond(operand_asts, env)
        elif name == 'begin':
            return eval_begin(operand_asts, env)
        elif name == 'quote':
            return eval_quote(operand_asts, env)
    
    # 其他的ID/表达式都去求值求出来
    proc = eval(operator_ast, env)
    operands = eval_list(operand_asts, env)
    if isinstance(proc, Procedure):
        return apply(proc, operands)
    else:
        raise RuntimeError('Not a procedure.')
            
def apply(proc: Procedure, operands: List[SchValue]) -> SchValue:
    return proc.call(operands)

def eval_define(operands: List[AstNode], env: EvalEnv) -> SchValue:
    assuming_len_in(operands, 2, None)
    name_ast = operands[0]
    if isinstance(name_ast, IdentifierNode):
        return eval_set(operands, env)
    else:
        header = name_ast.as_node(ExprListNode)
        name = header.sons[0].as_node(IdentifierNode).data()
        formals = ExprListNode(header.sons[1:])

        lambda_operands: List[AstNode] = [formals]
        lambda_operands.extend(operands[1:])
        proc = eval_lambda(lambda_operands, env)
        
        env.bind(name, proc)
        return Boolean(False)


def make_unend_list(vals: List[SchValue]) -> Pair:
    if len(vals) == 2:
        return Pair(vals[0], vals[1])
    else:
        return Pair(vals[0], make_unend_list(vals[1:]))

def eval_quote(operands: List[AstNode], env: EvalEnv) -> SchValue:
    assuming_len(operands, 1)
    quoted = operands[0]
    if isinstance(quoted, IdentifierNode):
        return Symbol(quoted.data())
    elif isinstance(quoted, AtomNode):
        return eval_atom(quoted, env)
    
    # ExprList quotion
    quoted = quoted.as_node(ExprListNode)
    son_values = lmap(bind_tail(eval_quote, env), quoted.sons)

    if quoted.is_dot():
        assuming_len_in(quoted.sons, 2, None, 'There must be sth behind a dot.')
        return make_unend_list(son_values)
    else:
        return make_sch_list(son_values)
    
def eval_begin(operands: List[AstNode], env: EvalEnv) -> SchValue:
    return eval_list(operands, env)[-1]

def eval_cond(operands: List[AstNode], env: EvalEnv) -> SchValue:
    for branch in operands:
        branch = branch.as_node(ExprListNode)
        assuming_len(branch.sons, 2)

        cond = eval(branch.sons[0], env)
        if cond:
            return eval(branch.sons[1], env)
    return Boolean(False)

def eval_set(operands: List[AstNode], env: EvalEnv) -> SchValue:
    assuming_len(operands, 2)

    name = operands[0].as_node(IdentifierNode).data()
    value = eval(operands[1], env) 
    env.bind(name, value)

    return Boolean(False)

def eval_if(operands: List[AstNode], env: EvalEnv) -> SchValue:
    assuming_len_in(operands, 2, 3)
    cond = eval(operands[0], env)
    if cond:
        return eval(operands[1], env)
    elif len(operands) == 3:
        return eval(operands[2], env)
    else:
        return Boolean(False)

# ==================== eval_lambda ==============================

def make_sch_list(v: List[SchValue] ) -> Union[Pair, Nil]:
    if len(v) == 0:
        return nil
    return Pair(v[0], make_sch_list(v[1:]))

def extract_names(asts: List[AstNode]) -> List[str]:
    return lmap(lambda ast: ast.as_node(IdentifierNode).data(), asts)

PreEnv = Dict[str, SchValue]
def make_binder(formal_ast: AstNode) -> Callable[[List[SchValue]], PreEnv]:
    def list_binder(name: str, values: List[SchValue]) -> PreEnv:
        return { name: make_sch_list(values) }

    def normal_binder(names: List[str], values: List[SchValue]) -> PreEnv:
        if len(names) != len(values):
            raise RuntimeError(f'Unmatch argument amount: except: {len(names)}, given: {len(values)}')

        pre_env = {}
        for name, value in zip(names, values):
            pre_env[name] = value
        return pre_env

    if isinstance(formal_ast, IdentifierNode):
        name = formal_ast.data()
        return lambda values: list_binder(name, values)
    
    elif isinstance(formal_ast, ExprListNode):
        if not formal_ast.is_dot():
            names = extract_names(formal_ast.sons)
            return lambda values: normal_binder(names, values)
        else: # dotted
            normal_names = extract_names(formal_ast.sons[:-1])
            list_name = formal_ast.sons[-1].as_node(IdentifierNode).data()
            
            def mixin_binder(values: List[SchValue]) -> PreEnv:
                normal_length = len(normal_names)
                normal_values, list_values = values[:normal_length], values[normal_length:]
                return union(normal_binder(normal_names, normal_values), \
                             list_binder(list_name, list_values))
            return mixin_binder

    raise RuntimeError(f'Invaild formals formal: {formal_ast.to_scheme()}')

def bind_free(formal_ast: AstNode, body: List[AstNode], env: EvalEnv) -> PreEnv:
    formal_names: List[str]
    if isinstance(formal_ast, IdentifierNode):
        formal_names = [formal_ast.data()]
    else:
        formal_names = extract_names(formal_ast.as_node(ExprListNode).as_list())   

    from copy import deepcopy
    def bind_free(ast: AstNode) -> PreEnv:
        if isinstance(ast, ExprListNode):
            return reduce(union, map(bind_free, ast.sons), {})
        elif isinstance(ast, IdentifierNode) and ast.data() not in formal_names:
            # 不拷贝可能在外层free_var对应的binding被set!了之后导致引用它的闭包里面的值的修改
            # 浅拷贝需要考虑引用了外层Pair但是被set-car!之类的情况
            # 所以深拷贝
            return { ast.data(): deepcopy(env.get(ast.data())) }
        return {}
    
    return reduce(union, map(bind_free, body), {})

def eval_lambda(operands: List[AstNode], env: EvalEnv) -> Procedure:
    formal_ast, body_asts = operands[0], operands[1:]

    formal_binder = make_binder(formal_ast)
    free_var_pre_env = bind_free(formal_ast, body_asts, env)

    def func(values: List[SchValue]) -> SchValue:
        formal_pre_env = formal_binder(values)
        proc_env = Environment(None, union(formal_pre_env, free_var_pre_env))
        return eval_list(body_asts, proc_env)[-1]

    return Procedure(func)
