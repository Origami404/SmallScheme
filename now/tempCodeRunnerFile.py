    program = ''' 
        (define (map proc list)
        (if (null? list)
            '()
            (cons (proc (car list)) (map proc (cdr list)))))

        (define-syntax for-each
        (syntax-rules (tmp)
            ((for-each proc l)
             (map proc l))))

        (display (for-each (lambda (x) (* x x)) '(1 2 3 4)))
        (newline)
    '''