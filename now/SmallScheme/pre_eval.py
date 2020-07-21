from re import match
from . import *
from .parser import AstNode, ExprListNode, IdentifierNode
from typing import Generic, Optional, Type
from .runtime import Environment, assuming_len, not_empty

# 处理掉所有的 宏定义 与 宏展开

MarcoEnvironment = Environment[Callable[[AstNode], AstNode]]

# Python 默认是 pass-by-reference 的, env可能会随着传到子节点那里被子节点修改
# TODO: 修改以使其遵守作用域规则: 目前: 基于代码文本上前后的; 预期: 遵循scope的 
def pre_eval(ast: AstNode, env: MarcoEnvironment) -> Optional[AstNode]:
    # print(f'New: {ast.to_scheme()}')
    # 对于单个 Identifier 的宏替换
    if isinstance(ast, IdentifierNode) and env.has(ast.data()):
        transformer = env.get(ast.data())
        return pre_eval(transformer(ast), env) 
    
    pre_eval_sons = bind_tail(pre_eval, env)

    # 对于 ExprList ...
    if isinstance(ast, ExprListNode):
        operator, operands = ast.split()
        if isinstance(operator, IdentifierNode):
            # 处理 宏定义
            if operator.data() == 'define-syntax':
                eval_marco_define(operands, env)
                return None
            
            # quote 里的东西不要宏展开
            if operator.data() == 'quote':
                return ast

            # 如果这个 ExprList 的开头是环境里的 keyword, 就进行 宏展开
            if env.has(operator.data()):
                transformer = env.get(operator.data())
                return pre_eval(transformer(ast), env) 
        
        # 如果都不是上面的情况, 还要递归下去处理它的子节点
        return ExprListNode(lnmap(pre_eval_sons, ast.as_list()))

    return ast

# 构造出 syntax-rule 并顺路绑定上
def eval_marco_define(operands: List[AstNode], now_env: MarcoEnvironment) -> None:
    assuming_len(operands, 2)

    name = operands[0].as_node(IdentifierNode).data()
    body = operands[1].as_node(ExprListNode)

    syntax_rule_operands = body.sons[1:] # skip the keyword 'syntax-rule'

    now_env.bind(name, make_syntax_rule(syntax_rule_operands))


LexicalBindings = Dict[str, Union[AstNode, List[AstNode]]]
MatchResult = Tuple[bool, LexicalBindings]
PatternMatcher = Callable[[AstNode], MatchResult]

# 构造 宏
# 具体到 Python 代码, Scheme 里的一个 宏 其实就是一个对 AstNode 进行变换的函数 use(ast: AstNode) -> AstNode
# 它就是在构造函数 use
def make_syntax_rule(operands: List[AstNode]) -> Callable[[AstNode], AstNode]:
    # 它第一个一定要是一个类型为'ExprList'的AstNode
    literals_ast =  operands[0].as_node(ExprListNode)
    literals = lmap(lambda node: node.as_node(IdentifierNode).data(), literals_ast.sons) 

    def make_rule(rule_: AstNode) -> Tuple[PatternMatcher, AstNode]:
        rule = rule_.as_node(ExprListNode)
        assuming_len(rule.sons, 2)

        pattern_ast, template_ast = rule.sons[0], rule.sons[1]
        return (make_pattern_tree(pattern_ast, literals), template_ast)

    cases = lmap(make_rule, operands[1:])

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
def transform(template: AstNode, lexical_binds: LexicalBindings) -> AstNode:
    # 主要是处理...的ID, 那些不能直接递归下去返回, 会把全套函数签名弄得一团糟的
    # 直接在处理ExprList的时候把那些东西处理掉
    if isinstance(template, ExprListNode):
        trans_sons: List[AstNode] = []
        for son in template.sons:
            if isinstance(son, IdentifierNode) and son.data() in lexical_binds:
                binding = lexical_binds[son.data()]
                # 绑定了一个List的name一定是一个后面带着...的ID
                # 在这里把它展开
                if isinstance(binding, list):
                    trans_sons.extend(binding)
                else:
                    trans_sons.append(binding)
            else:
                trans_sons.append(son)
        return ExprListNode(trans_sons)
    
    # 单个的ID Marco里面是不可能有...的
    if isinstance(template, IdentifierNode) and template.data() in lexical_binds:
        binding = lexical_binds[template.data()]
        if isinstance(binding, list):
            raise RuntimeError('Ellipsised identifier can not be used out of the ExprList')
        return binding
    
    # 其他的什么Number啊之类的
    return template

# ======================= 下面是对 Pattern 的处理 =============================

# 所有的 make_pattern_* 都返回一个具有PatternMatcher类型的函数:
# 其中第一个返回值表示匹配是否成功
# 第二个返回值是一个字典, 以对应的 identifier 为 key, 以其匹配到的 lexcial bind 为 value

# 所有的 make_pattern_* 都接受两个参数 ast: AstNode 跟 literals: List[str]
# 其中 ast      为 syntax-rule 里某条 pattern 的AST
#     literals 为 syntax-rule 里规定的字面量的列表

# 总的函数
def make_pattern_tree(ast: AstNode, literals: List[str]) -> PatternMatcher:
    if isinstance(ast, ExprListNode):
        return make_pattern_list(ast, literals)
    elif isinstance(ast, IdentifierNode):
        name = ast.data()
        if name in literals:
            return make_pattern_literal(ast)
        else:
            return make_pattern_identifier(ast)
    raise RuntimeError('Invaild pattern')

# 简单的 Pattern 处理
def make_pattern_identifier(ast: IdentifierNode) -> PatternMatcher:
    def matcher(target: AstNode) -> MatchResult:
        return True, {ast.data(): target}
    return matcher

def make_pattern_literal(ast: IdentifierNode) -> PatternMatcher:
    def matcher(target: AstNode) -> MatchResult:
        return isinstance(target, IdentifierNode) and target.data() == ast.data(), {}
    return matcher

# 处理 列表型的 Pattern

# 处理 (<pattern>*) 跟 (<pattern>* <pattern> ...)
def make_pattern_list(ast: ExprListNode, literals: List[str]) -> PatternMatcher:
    not_empty(ast.sons)
    if ast.is_ellipsis():
        return make_pattern_ellipsis_list(ast, literals)
    else:
        return make_pattern_normal_list(ast, literals)

# 处理 (<pattern> *)
def make_pattern_normal_list(ast: ExprListNode, literals: List[str]) -> PatternMatcher:
    sub_patterns = lmap(bind_tail(make_pattern_tree, literals), ast.sons)

    def matcher(target: AstNode) -> MatchResult:
        # 对 exprs 的基本检查
        if not isinstance(target, ExprListNode) or len(target.sons) != len(sub_patterns):
            return False, {}
        
        # 这块可能对性能比较敏感, 采取迭代形式, 实现短路
        total_result = {}
        for sub_pattern, expr in zip(sub_patterns, target.sons): # 递归匹配
            is_match, result = sub_pattern(expr)
            
            if not is_match: # 短路
                return False, {}
            
            total_result = union(total_result, result)
        
        return True, total_result
    
    return matcher

# 处理 (<pattern>* <pattern> ...)
def make_pattern_ellipsis_list(ast: ExprListNode, literals: List[str]) -> PatternMatcher:
    normal_pattern = lmap(bind_tail(make_pattern_tree, literals), ast.sons[:-1])
    normal_length = len(normal_pattern)
    ellipsis_name = ast.sons[-1].as_node(IdentifierNode).data()

    def matcher(target: AstNode) -> MatchResult:
        if not isinstance(target, ExprListNode) or len(target.sons) < normal_length:
            return False, {}
        
        normal_target = target.sons[:normal_length]
        total_result: Dict[str, Union[AstNode, List[AstNode]]] = {}
        for sub_pattern, sub_target in zip(normal_pattern, normal_target):
            is_match, result = sub_pattern(sub_target)

            if not is_match:
                return False, {}
            
            total_result = union(total_result, result)

        ellipsis_target = target.sons[:normal_length]
        total_result[ellipsis_name] = ellipsis_target

        return True, total_result

    return matcher
