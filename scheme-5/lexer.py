from util import *

TokenLiteral = Literal[
    'Quote', 
    # 'Quasiquote','CommaAt', 'Comma', 
    # 'VectorLeftBractet', 
    'LeftBracket', 'RightBracket', 
    'String', 'Character', 'Boolean', 'Number', 
    'Identifier', 'Dot'
]

def eat_space(string: str) -> str:
    space_pattern = regex(r'\s*')
    match = space_pattern.match(string)
    if match:
        string = string[len(match[0]):]
    return string

# 实现一个可以向前看一个 token 的 lexer
def tokenize(string: str) -> Iterable[Tuple[TokenLiteral, str]]:
    id_inital = r'a-zA-Z!$%&*\/:<=>?^_~'
    id_subsequent = f'{id_inital}0-9+-.@'

    ordered_token_pattern = [
        
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
        ('Identifier',        regex(r'(\+|-|\.\.\.)')),
        ('Dot',               '.') # 有三个连续的点的话优先解析为...
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
        # return 'Error', 'Not vaild token', 0
        raise RuntimeError(f'Not vaild token: {str}')
    
    # TODO: Add line/char number support
    while len(string) != 0:
        string = eat_space(string)
        if len(string) == 0:
            break
        token_name, token_data, token_len = match(string)
        string = string[token_len:]
        yield (token_name, token_data)


# 一个可以将 [x, y, z ...] 的生成器变成 [(x, y), (y, z), (z, ?), ...] 的生成器的玩意
class IterBuffer:
    def __init__(self, generator, end_with=None, *args):
        self.iterator = generator(*args)
        self.end_with = end_with
        # self.begin = True

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
        # if self.begin:
        #     self.begin = False
        #     return self._now
        if self._now == self.end_with:
            raise StopIteration
        
        old, new = self._now, None
        try:
            new = next(self.iterator)
        except StopIteration:
            self._now, self._after = self._after, self.end_with
        else:
            self._now, self._after = self._after, new
        return old

def make_lexer(string):
    return IterBuffer(tokenize, None, string)