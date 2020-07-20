# Python for Simple Scheme
# 
# todo:
#   - Add more kinds of number
#   - Add quasiquote support

from re import compile as regex

def tokenize(string):
    id_inital = r'a-zA-Z!$%&*\/:<=>?^_~'
    id_subsequent = f'{id_inital}0-9+-.@'

    ordered_token_pattern = [
        ('Dot',               '.'),
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
        ('PeculiarIdentifier',regex(r'(\+|-|\.\.\.)'))
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

class IterBuffer:
    def __init__(self, generator, end_with=None, **args):
        self.iterator = generator(args)
        self.end_with = end_with

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
        new = None
        try:
            new = next(self.iterator)
        except StopIteration:
            self._now, self._after = self._after, self.end_with
        else:
            self._now, self._after = self._after, new
        return self._now
    
def pluralize(token_buffer, func, end_token):
    results = []
    ends = make_list(end_token)
    need_return_end = type(end_token) == type([])

    while True:
        token, _ = token_buffer.now()
        
        if token in ends:
            return (results, token) if need_return_end else results

        else:
            results.append(func(token_buffer))

def not_vaild(productor_name, *args):
    raise RuntimeError(f'Not vaild {productor_name}: {args}')

def make_list(data):
    if type(data) == type([]):
        return data
    else: return [data]

class AstNode:
    def __init__(self, types, sons, extra_data=None):
        self.types = make_list(types)
        self.sons = make_list(sons)
        self.extra_data = extra_data
    
    def __getitem__(self, key):
        return self.extra_data[key]
    
    def is_type(self, type):
        return type in self.types

    def __str__(self):
        extra_data_str = self.extra_data + ", " if self.extra_data else ""
        return f'[{self.types}, {extra_data_str}[{", ".join(map(str, self.sons))}]]'

syntactic_keyword = [
    # expression keyword
    'quote', 'lambda', 'if', 'set!', 'begin', 'cond', 'and', 'or', 
    'case', 'let', 'let*', 'letrec', 'do', 'delay', 'quasiquote',
    # syntactic keyword
    'else', '=>', 'define', 'unquote', 'unquote-splicing'
]

self_eval_tokens = ['Number', 'Boolean', 'Character', 'String']

def parse_list(token_buffer):
    token, _ = next(token_buffer)

    abbrev_prefix_tokens = ['Quote', 'Quasiquote', 'CommaAt', 'Comma']
    if token in abbrev_prefix_tokens:
        return AstNode('Abbrev', parse_datum(token_buffer))
    
    datums, end = pluralize(token_buffer, parse_datum, ['Dot', 'RightBracket'])
    if end == 'RightBracket':
        return AstNode('List', datums)
    else:
        return AstNode(['List', 'Half'], datums, {'end_datum': parse_datum(token_buffer)})    
    not_vaild('list')


def parse_datum(token_buffer):
    token, data = next(token_buffer)
    if token in ['Boolean', 'Number', 'Character', 'String', 'Identifier']:
        return {'type': token.replace('Identifier', 'Symbol'), 'value': data}
    elif token == 'LeftBracket':
        return parse_list(token_buffer)
    # elif token == 'VectorLeftBracket':
    #     return AstNode('Vector', {'datums': pluralize(token_buffer, parse_datum, 'RightBracket')})
    not_vaild('datum')


def parse_bracket(token_buffer):
    exprs = pluralize(token_buffer, parse_expression, 'RightBracket')
    
    if len(exprs) == 0:
        not_vaild('bracket', '() is not a vaild bracket expression')

    # keyword
    operator_expr = exprs[0]
    if operator_expr.is_type('Keyword'):
        return AstNode(['KeywordApplication'], exprs)

    # procedure call
    else:
        return AstNode('Application', exprs)

def parse_variable(token_buffer):
    token, data = next(token_buffer)
    if token == 'Identifier' and token not in syntactic_keyword:
        return AstNode(['Identifier', 'Symbol', 'Valiable'], [], {'name': data})
    not_vaild('variable')

def eat_token(token_buffer, token):
    if type(token) == type(()):
        assert(next(token_buffer) == token)
    elif type(token) == type(''):
        assert(next(token_buffer) == (token, ''))
    else: raise RuntimeError('这token有毒我吃不下')

def parse_lambda(token_buffer):
    eat_token(token_buffer, ('Identifier', 'lambda')) # eat lambda
    eat_token(token_buffer, 'LeftBracket') # eat (
 
    types = ['Lambda']
    extra_data = {
        'formals': [],
        'left_formal': None
    }

    formals, end = pluralize(token_buffer, parse_variable, ['RightBracket', 'Dot'])
    if end == 'RightBracket':
        types.append('NormalLambda')
    else:
        if len(formals) == 0:
            not_vaild('lambda dot-formals')
        types.append('DotLambda')
        extra_data['left_formal'] = parse_variable(token_buffer)
    extra_data['formals'] = formals

    body = pluralize(token_buffer, parse_expression, 'RightBracket')

    return AstNode(types, body, extra_data)


def parse_set(token_buffer):
    eat_token(token_buffer, ('Identifier', 'set!')) # eat set!
    return AstNode(['Keyword', 'Set!'], [parse_variable(token_buffer), parse_expression(token_buffer)])
    
def parse_if(token_buffer):
    eat_token(token_buffer, ('Identifier', 'if'))
    return AstNode(['Keyword', 'If'], pluralize(token_buffer, parse_expression, 'RightBracket'))

def parse_derived_expression(token_buffer):
    return AstNode(['Keyword', 'Marco'])

def parse_expression(token_buffer):
    token, data = next(token_buffer)


    # bracket: core-keyword & derived expression, procedure call, expression list
    if token == 'LeftBracket':
        # return parse_bracket(token_buffer)
        _, operator = token_buffer.now()
        if operator == 'lambda':
            return parse_lambda(token_buffer)
        elif operator == 'set!':
            return parse_set(token_buffer)
        elif operator == 'if':
            return parse_if(token_buffer)
        elif operator in syntactic_keyword:
            return parse_derived_expression(token_buffer)
        else:
            return parse_application(token_buffer)
    
    # variable
    elif token == 'Identifier':
        if token in syntactic_keyword:
            return AstNode('Keyword', [], {'name': data})
        return AstNode('Identifier', [], {'name': data})
    
    # literal
    elif token == 'Quote':
        return AstNode(['Literal', 'Quote'], parse_datum(token_buffer))
    elif token in self_eval_tokens:
        return AstNode(['Literal', 'SelfEval', token], [], {'value': data})

    # TODO: Add Vector support. 
    # elif token == 'VectorLeftBracket':
    #     return AstNode('vector', pluralize(token_buffer, parse_datum, 'RightBracket'))

    not_vaild('expression', token, data)



if __name__ == "__main__":
    for token, data in tokenize("(define (apply-one f arg) (f arg 1))"):
        print(token, data)
    # print(list(tokenize("(define (apply-one f arg) (f arg))")))