# 分词 -> 解析 -> 预求值: 宏定义 -> 预求值: 宏用 -> 求值 ->　．．． 
from SmallScheme.lexer import make_lexer
from SmallScheme.parser import ExprListNode, parse_program
from SmallScheme.pre_eval import pre_eval, Environment
from SmallScheme.eval import eval
from SmallScheme import fprint
from SmallScheme.runtime import inital_environment

if __name__ == "__main__":
    program = ''' 
        (define (map proc list)
        (if (null? list)
            '()
            (cons (proc (car list)) (map proc (cdr list)))))

        (define-syntax for-each
        (syntax-rules (map)
            ((for-each proc l)
             (map proc l))))

        (display (for-each (lambda (x) (* x x)) '(1 2 3 4)))
        (newline)
    '''
    token_buffer = make_lexer(program)
    ast = parse_program(token_buffer)
    trans_ast = pre_eval(ast, Environment(None))
    if trans_ast:
        eval(trans_ast, inital_environment())