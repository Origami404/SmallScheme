from . import *
from .scheme_parser import AstNode
from typing import Optional

# 处理掉所有的 宏定义 与 宏展开

# eval 时的环境, 其实应该就相当于标准库的 ChainMap ?
class Environment:
    def __init__(self, father: 'Environment'=None, inital={}):
        self.father = father
        self.binds = inital

    def bind(self, name: str, var: Any) -> None:
        self.binds[name] = var
    
    def get(self, name: str) -> Any:
        if name not in self.binds:
            if self.father:
                return self.father.get(name)
            raise RuntimeError(f'Unbound name: {name}')
        return self.binds[name]
    
    def has(self, name: str) -> bool:
        try:
            self.get(name)
        except RuntimeError:
            return False
        return True
    
    def update(self, binds: Dict[str, Any]) -> 'Environment':
        self.binds = union(self.binds, binds)
        return self

# Python 默认是 pass-by-reference 的, env可能会随着传到子节点那里被子节点修改
# TODO: 修改以使其遵守作用域规则: 目前: 基于代码文本上前后的; 预期: 遵循scope的 
def per_eval(ast: AstNode, env: Environment) -> Optional[AstNode]:
    # 对于单个 Identifier 的宏替换
    if ast.is_type('Identifier') and env.has(ast.data()):
        transformer = env.get(ast.data())
        return transformer(ast)
    
    pre_eval_sons = lambda: lnmap(bind_tail(per_eval, env), ast.sons)

    # 对于 ExprList ...
    if ast.is_type('ExprList'):
        operator, operands = ast[0], ast.sons[1:]
        if operator.is_type('Identifier'):
            # 处理 宏定义
            if operator.data() == 'define-syntax':
                eval_marco_define(operands, env)
                return None
            
            # 如果这个 ExprList 的开头是环境里的 keyword, 就进行 宏展开
            if env.has(operator.data()):
                transformer = env.get(ast.data())
                return transformer(ast)
        
        # 如果都不是上面的情况, 还要递归下去处理它的子节点
        return AstNode('ExprList', pre_eval_sons())

    if ast.in_type(['Quote', 'Program']):
        return AstNode(ast.types, pre_eval_sons())

    return ast

# 构造出 syntax-rule 并顺路绑定上
def eval_marco_define(operands: List[AstNode], now_env: Environment) -> None:
    name, body = operands[0].data(), operands[1]

    syntax_rule_operands = body.sons[1:] # skip the keyword 'syntax-rule'

    now_env.bind(name, make_syntax_rule(syntax_rule_operands))

# 构造 宏
# 具体到 Python 代码, Scheme 里的一个 宏 其实就是一个对 AstNode 进行变换的函数 use(ast: AstNode) -> AstNode
# 它就是在构造函数 use
def make_syntax_rule(operands: List[AstNode]) -> Callable[[AstNode], AstNode]:
    # 它第一个一定要是一个类型为'ExprList'的AstNode
    literals = lmap(lambda node: node[0], operands[0]) # type: ignore
    cases = lmap(lambda node: (make_pattern_tree(node[0], literals), node[1]), operands[1:])

    def use(expr: AstNode) -> AstNode:
        # 按顺序匹配各条 pattern
        for pattern, template in cases:
            is_match, lexical_binds = pattern(expr)

            if is_match:
                return transform(template, lexical_binds)

        # 所有的 pattern 都匹配不到的话就丢异常
        raise RuntimeError('Invaild marco use.')

    return use

# 将匹配好的 AST 在匹配到的 lexical binds 下进行展开
def transform(template: AstNode, lexical_binds: Dict[str, AstNode]) -> AstNode:
    if template.is_type('ExprList'):
        return AstNode('ExprList', lmap(bind_tail(transform, lexical_binds), template.sons))
    
    elif template.is_type('Identifier') and template[0] in lexical_binds:
        # 已经验证过这个叫template的ASTNode一定是Identifier了
        return lexical_binds[template[0]] # type: ignore
    
    return template

# ======================= 下面是对 Pattern 的处理 =============================

# 所有的 make_pattern_* 都返回一个具有如下类型的函数:
PatternMatcher = Callable[[AstNode], Tuple[bool, Dict[str, AstNode]]]
# 其中第一个返回值表示匹配是否成功
# 第二个返回值是一个字典, 以对应的 identifier 为 key, 以其匹配到的 lexcial bind 为 value

# 所有的 make_pattern_* 都接受两个参数 ast: AstNode 跟 literals: List[str]
# 其中 ast      为 syntax-rule 里某条 pattern 的AST
#     literals 为 syntax-rule 里规定的字面量的列表

# 总的函数
def make_pattern_tree(ast: AstNode, literals: List[str]) -> PatternMatcher:
    if ast.is_type('ExprList'):
        return make_pattern_list(ast, literals)
    elif ast.is_type('Identifier'):
        name = ast[0]
        if name in literals:
            return make_pattern_literal(ast, literals)
        else:
            return make_pattern_identifier(ast, literals)
    raise RuntimeError('Invaild pattern')

# 简单的 Pattern 处理
def make_pattern_identifier(ast: AstNode, literals: List[str]) -> PatternMatcher:
    # Python would never support annotation in lambda
    return lambda expr: (True, {ast.sons[0]: expr}) # type: ignore 

def make_pattern_literal(ast: AstNode, literals: List[str]) -> PatternMatcher:
    return lambda expr: expr.is_type('Identifier') and expr[0] == ast[0], {} # type: ignore

# 处理 列表型的 Pattern

# 处理 (<pattern>*) 跟 (<pattern>* <pattern> ...)
def make_pattern_list(ast: AstNode, literals: List[str]) -> PatternMatcher:
    last = ast[-1]
    if last.is_type('Ellipsis'):
        return make_pattern_ellipsis_list(ast, literals)
    else:
        return make_pattern_normal_list(ast, literals)

# 处理 (<pattern> *)
def make_pattern_normal_list(ast: AstNode, literals: List[str]) -> PatternMatcher:
    sub_patterns = lmap(bind_tail(make_pattern_tree, literals), ast.sons)

    def match(exprs: AstNode):
        # 对 exprs 的基本检查
        if not exprs.is_type('ExprList') or len(exprs.sons) != len(sub_patterns):
            return False, {}
        
        # 这块可能对性能比较敏感, 采取迭代形式, 实现短路
        total_result = {}
        for sub_pattern, expr in zip(sub_patterns, exprs.sons): # 递归匹配
            is_match, result = sub_pattern(expr)
            
            if not is_match: # 短路
                return False, {}
            
            total_result = union(total_result, result)
        
        return True, total_result
    
    return match

# 处理 (<pattern>* <pattern> ...)
def make_pattern_ellipsis_list(ast: AstNode, literals: List[str]) -> PatternMatcher:
    sub_patterns = lmap(bind_tail(make_pattern_tree, literals), ast.sons)
    normal_subs, ellipsis_sub = sub_patterns[:-1], sub_patterns[-1]

    def match(exprs):
        if not exprs.is_type('ExprList') or len(normal_subs) > len(sub_patterns):
            return False, {}
        
        total_result = {}
        for sub_pattern, expr in zip(normal_subs, exprs.sons):
            is_match, result = sub_pattern(expr)

            if not is_match:
                return False, {}
            
            total_result = union(total_result, result)
        
        is_match, result = ellipsis_sub(exprs[len(normal_subs):])
        if not is_match:
            return False, {}
        
        return True, union(total_result, result)

    return match
