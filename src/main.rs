use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
};
use Expr::*;

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

fn apply(expr: &Expr, rules: &[RRule]) -> Expr {
    let mut new = expr.clone();
    let mut old;

    loop {
        old = new.clone();
        for rule in rules {
            new = apply_impl_iterative(&new, rule);
        }
        if old == new {
            break;
        }
        println!("{old}");
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
fn apply_impl_iterative(expr: &Expr, rule: &RRule) -> Expr {
    use std::collections::VecDeque;

    // Stack element: (current_expr, processed_flag)
    let mut stack: Vec<(Expr, bool)> = Vec::new();
    let mut results: Vec<Expr> = Vec::new();

    stack.push((expr.clone(), false));

    while let Some((current_expr, visited)) = stack.pop() {
        if visited {
            // Rebuild if App, otherwise keep as-is
            match current_expr {
                App(_, _) => {
                    let right_expr = results.pop().unwrap();
                    let left_expr = results.pop().unwrap();
                    results.push(App(Box::new(left_expr), Box::new(right_expr)));
                }
                _ => {
                    results.push(current_expr);
                }
            }
        } else {
            // Check for rule match
            if let Some(binds) = pattern_match(&current_expr, &rule.patt) {
                if let Some(replaced) = replace(&rule.re, &binds) {
                    // Reapply the rule on the replaced expression
                    stack.push((replaced, false));
                } else {
                    results.push(current_expr); // fallback if replacement fails
                }
            } else {
                match &current_expr {
                    App(left, right) => {
                        // Post-order traversal: push current node (marked), then children
                        stack.push((current_expr.clone(), true));
                        stack.push((*right.clone(), false));
                        stack.push((*left.clone(), false));
                    }
                    _ => {
                        // Leaf node, no match: push directly
                        results.push(current_expr.clone());
                    }
                }
            }
        }
    }

    results.pop().unwrap()
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
            "<>" => Token::RuleKW,
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
    RuleKW,
    OpId(String),
}

struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

#[derive(Debug, Clone)]
enum TreeNode {
    Rule { rule: RRule },
    Expr(Expr),
}

fn check_rule(rule: &RRule) -> bool {
    let mut re_sym = HashSet::new();
    let mut patt_sym = HashSet::new();

    fn traverse_expr(expr: &Expr, re_sym: &mut HashSet<String>) {
        match expr {
            App(expr, expr1) => {traverse_expr(&expr, re_sym); traverse_expr(&expr1, re_sym);},
            Var(v) => {re_sym.insert(v.to_string());},
            Op(_) => return,
        }
    }

    traverse_expr(&rule.patt, &mut patt_sym);
    traverse_expr(&rule.re, &mut re_sym);

    return re_sym.is_subset(&patt_sym);
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
            Some(Token::Lparen) => (),
            _ => return None,
        }
        let re = self.parse_expr()?;
        let rule = RRule { patt, re };
        if check_rule(&rule) {
            Some(rule)
        } else {
            println!("new token on right side. Unacceptable!");
            None
        }
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
    rules: Vec<RRule>,
}

impl Repl {
    fn new() -> Self {
        Self {
            rules: Vec::new(),
        }
    }
    fn run(&mut self) -> Result<(), std::io::Error> {
            let stdin = std::io::stdin();
            for input in stdin.lines() {
                if let Ok(input) = input {

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
                                        .map(|a| { (*a).clone() })
                                        .collect::<Vec<RRule>>(),
                                )
                            )
                        }
                        Some(TreeNode::Rule { rule }) => {
                            self.rules.push(rule);
                        }
                        None => println!("Syntax error!"),
                    }
                }
            }}}

        println!("Goodbye.");
        Ok(())
    }
}

impl Display for TreeNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TreeNode::Expr(expr) => write!(f, "{expr}"),
            TreeNode::Rule { rule } => write!(f, "{rule:?}"),
        }
    }
}
