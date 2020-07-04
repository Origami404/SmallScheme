from util import *
from scheme_parser import AstNode

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

def per_eval(ast: AstNode, env: Environment) -> AstNode:
    if ast.is_type('Identifier') and env.has(ast.data()):
        transformer = env.get(ast.data())
        return transformer(ast)
    
    if ast.is_type('ExprList'):
        operator, operands = ast[0], ast.sons[1:]
        if operator.is_type('Identifier'):
            if operator.data() == 'define-syntax':
                eval_marco_define(operands, env)
                return None
            if env.has(operator.data()):
                return env.get(operator.data())(ast)
        return AstNode('ExprList', lnmap(bind_tail(per_eval, env), ast.sons))

    if ast.is_type('Program'):
        new_sub_ast = []
        for sub_ast in ast.sons:
            new_ast = per_eval(sub_ast, env)
            if new_ast:
                new_sub_ast.append(new_ast)
            # print(env.binds)
        return AstNode('Program', new_sub_ast)

        # return AstNode('Program', lnmap(bind_tail(per_eval, env), ast.sons))

    return ast

def make_pattern_list(ast: AstNode, literals: List[str]):
    last = ast[-1]
    if last.is_type('Ellispsis'):
        return make_pattern_ellipsis_list(ast, literals)
    else:
        return make_pattern_normal_list(ast, literals)

def make_pattern_normal_list(ast: AstNode, literals: List[str]):
    sub_patterns = lmap(bind_tail(make_pattern_tree, literals), ast)

    def match(exprs: AstNode):
        if not exprs.is_type('ExprList') or len(exprs.sons) != len(sub_patterns):
            return False, {}
        
        # 这块可能对性能比较敏感, 采取迭代形式, 实现短路
        total_result = {}
        for sub_pattern, expr in zip(sub_patterns, exprs.sons):
            is_match, result = sub_pattern(expr)

            if not is_match:
                return False, {}
            
            total_result = union(total_result, result)
        
        return True, total_result
    
    return match

def make_pattern_ellipsis_list(ast: AstNode, literals: List[str]):
    sub_patterns = lmap(bind_tail(make_pattern_tree, literals), ast)
    normal_subs, ellipsis_sub = sub_patterns[:-1], sub_patterns[-1]

    def match(exprs):
        if not exprs.is_type('ExprList') or len(normal_subs.sons) > len(sub_patterns):
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
        return AstNode('ExprList', lmap(bind_tail(transform, lexical_binds), template.sons))
    
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

def eval_marco_define(operands: AstNode, now_env: Environment) -> None:
    name, body = operands[0].data(), operands[1]

    syntax_rule_operands = body.sons[1:] # skip the keyword 'syntax-rule'

    now_env.bind(name, make_syntax_rule(syntax_rule_operands))
