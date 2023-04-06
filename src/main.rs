use std::{
    collections::HashMap,
    fmt::{Debug, Display},
};
use Expr::*;

// macro_rules! e {
//     ($left:expr, $right:expr) => {
//         App(Box::new($left), Box::new($right))
//     };
//     ($i:ident) => {
//         Var(stringify!($i).to_string())
//     };
//
//     (op $i:ident) => {
//         Op(stringify!($i).to_string())
//     };
// }

fn main() {
    let mut repl = Repl::new();
    repl.run().unwrap();
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Expr {
    App(Box<Expr>, Box<Expr>),
    Var(String),
    Op(String),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            App(a, b) => write!(f, "({a} {b})"),
            Var(a) => write!(f, "{a}"),
            Op(a) => write!(f, "!{a}"),
        }
    }
}

type Bindings = HashMap<String, Expr>;

fn pattern_match_impl(expr: &Expr, patt: &Expr, binds: &mut Bindings) -> bool {
    match (expr, patt) {
        (_, Var(name)) => {
            if let Some(bind) = binds.get(name) {
                return bind == expr;
            };
            binds.insert(name.to_string(), expr.clone());
        }
        (Op(expr_op), Op(patt_op)) => {
            return expr_op == patt_op;
        }
        (_, Op(_)) => return false,
        (App(expr_head, expr_body), App(pat_head, patt_body)) => {
            return pattern_match_impl(expr_head, pat_head, binds)
                && pattern_match_impl(&expr_body, &patt_body, binds)
        }
        (Var(_), App(_, _)) => return false,
        (Op(_), App(_, _)) => return false,
    };
    true
}

fn pattern_match(expr: &Expr, pattern: &Expr) -> Option<Bindings> {
    let mut bindings = HashMap::new();

    if pattern_match_impl(expr, pattern, &mut bindings) {
        Some(bindings)
    } else {
        None
    }
}

#[derive(Clone)]
struct RRule {
    patt: Expr,
    re: Expr,
}

impl Debug for RRule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> {}", self.patt, self.re)
    }
}

fn replace(statement: &Expr, binds: &Bindings) -> Option<Expr> {
    match statement {
        App(head, body) => Some(App(
            Box::new(replace(head, binds)?),
            Box::new(replace(body, binds)?),
        )),
        Var(name) => Some(binds.get(name)?.clone()),
        Op(_) => Some(statement.clone()),
    }
}

// fn deep_apply(expr: &Expr, rule: &RRule) -> Expr {
//     match pattern_match(expr, &rule.patt) {
//         Some(binds) => deep_apply(&replace(&rule.re, &binds).unwrap(), rule),
//         None => match expr {
//             App(head, body) => App(
//                 Box::new(deep_apply(&head, rule)),
//                 Box::new(deep_apply(&body, rule)),
//             ),
//             Var(_) => expr.clone(),
//             Op(_) => expr.clone(),
//         },
//     }
// }

fn apply(expr: &Expr, rules: &[RRule]) -> Expr {
    let mut new = expr.clone();
    let mut old;

    loop {
        old = new.clone();
        for rule in rules {
            new = apply_impl(&new, rule);
        }
        if old == new {
            break;
        }
    }

    new
}

fn apply_impl(expr: &Expr, rule: &RRule) -> Expr {
    match pattern_match(expr, &rule.patt) {
        None => match expr {
            App(left, right) => App(
                Box::new(apply_impl(&left, rule)),
                Box::new(apply_impl(&right, rule)),
            ),
            _ => expr.clone(),
        },
        Some(binds) => apply_impl(&replace(&rule.re, &binds).unwrap(), rule),
    }
}

fn lexe(text: &str) -> Vec<Token> {
    let text = text.replace("(", " ( ");
    let text = text.replace(")", " ) ");
    let text = text.replace("->", " -> ");

    let mut tokens = Vec::new();
    for word in text.split_whitespace() {
        tokens.push(match word {
            "(" => Token::Lparen,
            ")" => Token::Rparen,
            "rule" => Token::RuleKW,
            "->" => Token::Arrow,
            _ => {
                if word.starts_with("!") {
                    Token::OpId(word.strip_prefix("!").unwrap().to_string())
                } else {
                    Token::Id(word.to_string())
                }
            }
        })
    }
    tokens
}

#[derive(Debug, Clone)]
enum Token {
    Id(String),
    Lparen,
    Rparen,
    OpId(String),
    RuleKW,
    Arrow,
}

struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

#[derive(Debug, Clone)]
enum TreeNode {
    Rule { name: String, rule: RRule },
    Expr(Expr),
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, index: 0 }
    }
    fn next(&mut self) -> Option<&Token> {
        let token = self.tokens.get(self.index);
        self.index += 1;
        token
    }
    fn parse(&mut self) -> Option<TreeNode> {
        let x = self.next();
        match x {
            Some(Token::Lparen) => {
                let expr = Some(TreeNode::Expr(self.parse_expr()?));
                match self.next() {
                    None => (),
                    Some(_) => return None,
                }
                expr
            }
            Some(Token::RuleKW) => Some(TreeNode::Rule {
                name: match self.next() {
                    Some(Token::Id(name)) => name.to_string(),
                    _ => return None,
                },
                rule: self.parse_rule()?,
            }),
            _ => None,
        }
    }
    fn parse_rule(&mut self) -> Option<RRule> {
        match self.next() {
            Some(Token::Lparen) => (),
            _ => return None,
        }
        let patt = self.parse_expr()?;
        match self.next() {
            Some(Token::Arrow) => (),
            _ => return None,
        };
        match self.next() {
            Some(Token::Lparen) => (),
            _ => return None,
        }
        let re = self.parse_expr()?;

        Some(RRule { patt, re })
    }
    fn parse_expr(&mut self) -> Option<Expr> {
        let mut exprs = Vec::new();
        while let Some(thing) = self.next() {
            match thing {
                Token::Id(name) => exprs.push(Var(name.clone())),
                Token::Lparen => exprs.push(self.parse_expr()?),
                Token::Rparen => break,
                Token::OpId(name) => exprs.push(Op(name.clone())),
                _ => return None,
            }
        }
        exprs
            .iter()
            .map(|a| a.clone())
            .reduce(|acc, a| App(Box::new(acc.clone()), Box::new(a.clone())))
    }
}

struct Repl {
    rules: HashMap<String, RRule>,
}

use linefeed::{Interface, ReadResult};

impl Repl {
    fn new() -> Self {
        Self {
            rules: HashMap::new(),
        }
    }
    fn run(&mut self) -> Result<(), std::io::Error> {
        let reader = Interface::new("application")?;

        reader.set_prompt("etl> ")?;

        while let ReadResult::Input(input) = reader.read_line()? {
            match input.as_str() {
                "bye" => break,
                _ => {
                    let lexed = lexe(&input);

                    let mut parser = Parser::new(lexed);

                    match parser.parse() {
                        Some(TreeNode::Expr(expr)) => {
                            println!(
                                "{}",
                                apply(
                                    &expr,
                                    &self
                                        .rules
                                        .iter()
                                        .map(|a| { (*a.1).clone() })
                                        .collect::<Vec<RRule>>(),
                                )
                            )
                        }
                        Some(TreeNode::Rule { name, rule }) => {
                            println!("Added rule {} : {:?}", name, rule);
                            self.rules.insert(name, rule);
                        }
                        None => println!("Syntax error!"),
                    }
                }
            }
        }

        println!("Goodbye.");
        Ok(())
    }
}

impl Display for TreeNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TreeNode::Expr(expr) => write!(f, "{expr}"),
            TreeNode::Rule { name, rule } => write!(f, "rule {name}: {rule:?}"),
        }
    }
}
