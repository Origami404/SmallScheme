#include <iostream>
#include <string>
#include <cctype>
#include <cstring>
#include <algorithm>
#include <variant>
#include <vector>
#include <memory>
#include <map>


using std::string, std::pair, std::move, std::vector;
using NumType = long long;


namespace Lexer {
enum class TokenType {
    LeftBracket = 0, RightBracket, Identifier,  Define, Lambda, Quote, String, Number, NUL_C
};

using Token = pair<TokenType, std::variant<string, NumType>>;

std::ostream& operator<<(std::ostream &out, const Token &t) {
    const vector<string> token_name = {
         "LeftBracket", "RightBracket", "Identifier", /* "Define", "Lambda", */ "Quote", "String", "Number", "NUL_C" 
    };
    const auto &[token, v] = t;
    const auto &name = token_name[static_cast<int>(token)];

    if (token == TokenType::Number) {
        out << "(" << name << ", " << std::get<NumType>(v) << ")";
    } else {
        out << "(" << name << ", " << std::get<string>(v) << ")";
    }

    return out;
}

Token parse_token();

Token token_now, token_ahead;
const Token& now() {
    return token_now;
}
const Token& head() {
    return token_ahead;
}
const Token next() {
    Token n = move(token_now);
    token_now = token_ahead;
    token_ahead = parse_token();
    return n;
}

const int MAX_BUFFER = 1e7;
char buf[MAX_BUFFER], *buf_now;
bool is_id_char(const char c) { return !(c == '(' || c == ')' || c == '\'' || c == '\0' || c == '"' || isspace(c)); }
Token parse_token() {
    using ::std::isspace;
first: 
    if (*buf_now == '(') {
        buf_now += 1;
        return {TokenType::LeftBracket, ""};

    } else if (*buf_now == ')') {
        buf_now += 1;
        return {TokenType::RightBracket, ""};

    } else if (*buf_now == '\'') {
        buf_now += 1;
        return {TokenType::Quote, ""};

    } else if (isspace(*buf_now)) {
        while (isspace(*buf_now))
            buf_now += 1;
        goto first;

    } else if (*buf_now == '"') {
        buf_now += 1; // eat '"'
        string str;
        while (*buf_now != '"') {
            str += *buf_now;
            buf_now += 1;
        }
        buf_now += 1; // eat
            
        return {TokenType::String, str};

    } else if (isdigit(*buf_now)) {
        NumType val = 0;
        while (isdigit(*buf_now)) {
            val = val*10 + (*buf_now - '0');
            buf_now += 1;
        }
            
        return {TokenType::Number, val};

    } else if (*buf_now == '\0') {
        return {TokenType::NUL_C, ""};
    } else {
        string id;
        while (is_id_char(*buf_now)) {
            id += *buf_now;
            buf_now += 1;
        }
        
        if (id == "define")
            return {Define, id};
        else if (id == "lambda")
            return {Lambda, id};
        else 
            return {TokenType::Identifier, id};
    }
}

void init() {
    buf_now = buf;
    token_now = parse_token();
    token_ahead = parse_token();
}

}

using Lexer::now, Lexer::next, Lexer::head, Lexer::TokenType;

namespace AST {
template <typename T>
using uptr = std::unique_ptr<T>;
using ::std::make_unique;
using ::std::cout;

enum class NodeType {
    String, Number, Apply, Identifier, Quote, Program
};

class Node {
public:
    virtual void print() const = 0;
    virtual NodeType type() const = 0;
    virtual ~Node() {}
};



class ExpressionNode : public Node {};
class AtomNode : public ExpressionNode {};

class ProgramNode : public Node {
public: 
    ProgramNode(vector<uptr<ExpressionNode>> &&exprs)
        : exprs(move(exprs)) {}
    NodeType type() const { return NodeType::Program; }
    void print() const {
        cout << "(Program ";
        for (const auto &expr_ptr : exprs) 
            expr_ptr->print();
        cout << ")";
    }
private:
    vector<uptr<ExpressionNode>> exprs;
};

class ApplyNode : public ExpressionNode {
public:
    ApplyNode(uptr<ExpressionNode> &functor, vector<uptr<ExpressionNode>> &real_args)
        : functor(move(functor)), real_args(move(real_args)) {}
    NodeType type() const { return NodeType::Apply; }
    void print() const {
        cout << "(Apply ";
        functor->print();
        for (const auto &arg_ptr : real_args)
            arg_ptr->print();
        cout << ")";
    }
private:
    uptr<ExpressionNode> functor;
    vector<uptr<ExpressionNode>> real_args;
};

class QuoteNode : public AtomNode {
public:
    QuoteNode(uptr<ExpressionNode> code)
        : code(move(code)) {}
    NodeType type() const { return NodeType::Quote; }
    void print() const {
        cout << "(Quote "; code->print(); cout << ")";
    }
private:
    uptr<ExpressionNode> code;
};

class IdentifierNode : public AtomNode {
public:
    IdentifierNode(const string &name) : name(name) {}
    NodeType type() const { return NodeType::Identifier; }
    void print() const {
        cout << "(Identifier " << name << ")";
    }
private:
    const string name;
};

class NumberNode : public AtomNode {
public:
    NumberNode(const NumType &num) : num(num) {}
    NodeType type() const { return NodeType::Number; }
    void print() const { cout << "(Number " << num << ")"; }
private:
    const NumType num;
};  

class StringNode : public AtomNode {
public: 
    StringNode(const string &str) : str(str) {}
    NodeType type() const { return NodeType::String; }
    void print() const { cout << "(String " << str << ")"; }
private:
    const string str;
};

}

namespace Parser {

using namespace ::AST;

// uptr<ProgramNode> parse_program();
// uptr<ExpressionNode> parse_expression();
// uptr<AtomNode> parse_atom();
// uptr<QuoteNode> parse_quote();
// // uptr<OperatorApplyNode> parse_Operator_application();
// // uptr<MarcoApplyNode> parse_marco_application();

// uptr<ApplyNode> parse_application();

uptr<ProgramNode>     parse_program();
uptr<ExpressionNode>  parse_expression();
uptr<ApplyNode>       parse_apply();
uptr<ExpressionNode>  parse_functor();

vector<uptr<ExpressionNode>> parse_expr_list();

void debug(const string &func_name) {
    cout << "Now: " << func_name << now() << ::std::endl;
}

uptr<ProgramNode> parse_program() {
    debug("Program");
    return make_unique<ProgramNode>(move(parse_expr_list()));
} 

vector<uptr<ExpressionNode>> parse_expr_list() {
    debug("ExprList");
    vector<uptr<ExpressionNode>> exprs;
    const auto is_expr = [&](const TokenType &t) {
        return t == TokenType::Identifier 
            || t == TokenType::Number
            || t == TokenType::String
            || t == TokenType::Quote
            || t == TokenType::LeftBracket;
    };
    while (is_expr(now().first)) 
        exprs.push_back(move(parse_expression()));
    return exprs;
}


uptr<ExpressionNode> parse_expression() {
    debug("Expr");
    const auto [token, data] = now();
    uptr<ExpressionNode> ret;
    if (token == TokenType::LeftBracket) {
        ret = parse_apply(); 
    } else {
        if (token == TokenType::Identifier) {
            ret = make_unique<IdentifierNode>(move(std::get<string>(data))); next(); 
        } else if (token == TokenType::Number) {
            ret = make_unique<NumberNode>(move(std::get<NumType>(data))); next();
        } else if (token == TokenType::String) {
            ret = make_unique<StringNode>(move(std::get<string>(data))); next();
        } else if (token == TokenType::Quote) {
            next(); ret = make_unique<QuoteNode>(parse_expression());
        }
    }
    return ret;
}

uptr<ApplyNode> parse_apply() {
    debug("Apply");
    next(); // eat LeftBracket
    const auto [token, data] = now();

    uptr<ExpressionNode> functor;
    if (token == TokenType::Identifier) {
        const auto operator_name = move(std::get<string>(data));
        functor = make_unique<IdentifierNode>(move(std::get<string>(data)));

        next(); // eat functor
    } else if (token == TokenType::LeftBracket) {
        functor = parse_apply();
    }

    vector<uptr<ExpressionNode>> real_args = parse_expr_list();
    next(); // eat RightBracket
    return make_unique<ApplyNode>(functor, real_args);
}

}

namespace Interpreter {
using std::map;
using ::AST::uptr;
class Environment;
enum ArgumentType { Normal, Marco };

class Operator {
    using FuncType = std::function<Variable(const Environment&)>;
    using FArgType = pair<string, ArgumentType>;

    Operator(FuncType &body, const vector<string> &args, ArgumentType type, uptr<Environment> env) 
        : body(move(body)), parameter(args.size()), self_env(move(env)) {
            for (const auto &arg : args)
                parameter.push_back({arg, type});
        }  
    Operator(FuncType &body, const vector<FArgType> &args, uptr<Environment> env) 
        : body(move(body)), parameter(args), self_env(move(env)) {}
private:
    FuncType body;
    vector<FArgType> parameter;
    uptr<Environment> self_env;
};

class QuoteVar {
    using ExpressionNode = ::AST::ExpressionNode;
    QuoteVar(ExpressionNode *code) : code(code) {}
private: 
    ExpressionNode *code;
};

enum class VariableType { String, Number, Quote, Operator };
class Variable {
    Variable(const string &str) : value(str), type(VariableType::String) {}
    Variable(const NumType &val) : value(val), type(VariableType::Number) {}
    Variable(const QuoteVar &quote) : value(quote), type(VariableType::Quote) {}
    Variable(Operator &op) : value(move(op)), type(VariableType::Operator) {}
private:
    std::variant<string, NumType, QuoteVar, Operator> value;
    VariableType type;
};

class Environment {
public:
    Environment(Environment * const father) : operators(), variables(), father(father) {}

    void bind(const string &name) {}
private:
    map<string, Operator> operators;
    map<string, Variable> variables;
    Environment * const father;
};

}

namespace Test {
using namespace ::Lexer;
using namespace ::AST;
using namespace::Parser;
using std::cin, std::cout, std::endl;

// (define hhh (list (+ 1 2) "ss"))
const vector<string> cases = {
    "(define hhh (list (+ 1 2) \"ss\"))",
    "(define (my_magic x y z) (list '(list x y) \"f\" #t))",
    "(define (make-eq-pred val) (lambda (x) (= x val)))"
};

void ready(const string &test_case) {
    std::strcpy(::Lexer::buf, test_case.c_str());
    ::Lexer::init();
}

void test_lexer() { 
    
    // cin >> Lexer::buf;
    ready(cases[0]);
    while (head().first != TokenType::NUL_C) {
        cout << next() << " ";
        // cout << '(' << now_token << ", " << v << ')' << endl;
    }
    cout << endl;
}

void test_parser() {
    ready(cases[2]);
    uptr<ProgramNode> program = parse_program();
    program->print();
    cout << endl;
}
}

int main() {
    Test::test_parser();
    // std::cout << TokenType::LeftBracket;
    return 0;
}