# 分词 -> 解析 -> 预求值: 宏定义 -> 预求值: 宏用 -> 求值 ->　．．． 
from SmallScheme.lexer import make_lexer
from SmallScheme.parser import ExprListNode, parse_program
from SmallScheme.pre_eval import pre_eval, Environment
from SmallScheme import fprint

if __name__ == "__main__":
    program = ''' 
    (define-syntax for-each
         (syntax-rules (tmp)
                ((for-each proc l)
                    (map proc l))))

    (display (for-each (lambda (x) (* x x)) '(1 2 3 4)))
    (newline)
    '''
    token_buffer = make_lexer(program)
    ast = parse_program(token_buffer)
    print(ast.to_scheme())
    # fprint(ast.to_dict())
    trans_ast = pre_eval(ast, Environment(None))
    
    # fprint(ast.to_dict())

    # fprint(trans_ast.to_dict())
    
    print(trans_ast.to_scheme())