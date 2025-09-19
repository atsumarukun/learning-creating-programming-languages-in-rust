use std::{collections::HashMap, io::Read};

use nom::{
    Finish, IResult, Parser,
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, multispace0},
    combinator::{opt, recognize},
    error::ParseError,
    multi::{fold_many0, many0, separated_list0},
    number::complete::recognize_float,
    sequence::{delimited, pair},
};

#[derive(Debug, PartialEq, Clone)]
enum Expression<'src> {
    Ident(&'src str),
    NumLiteral(f64),
    FnInvoke(&'src str, Vec<Expression<'src>>),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Sub(Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
    Div(Box<Expression<'src>>, Box<Expression<'src>>),
}

#[derive(Debug, PartialEq, Clone)]
enum Statement<'src> {
    Expression(Expression<'src>),
    VarDef(&'src str, Expression<'src>),
    VarAssign(&'src str, Expression<'src>),
}

type Statements<'a> = Vec<Statement<'a>>;

fn space_delimited<'src, O, E>(
    f: impl Parser<&'src str, Output = O, Error = E>,
) -> impl Parser<&'src str, Output = O, Error = E>
where
    E: ParseError<&'src str>,
{
    delimited(multispace0, f, multispace0)
}

fn number(input: &'_ str) -> IResult<&'_ str, Expression<'_>> {
    let (r, v) = space_delimited(recognize_float).parse(input)?;
    Ok((
        r,
        Expression::NumLiteral(v.parse().map_err(|_| {
            nom::Err::Error(nom::error::Error {
                input,
                code: nom::error::ErrorKind::Digit,
            })
        })?),
    ))
}

fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))
    .parse(input)
}

fn ident(input: &'_ str) -> IResult<&'_ str, Expression<'_>> {
    let (r, res) = space_delimited(identifier).parse(input)?;
    Ok((r, Expression::Ident(res)))
}

fn parens(input: &'_ str) -> IResult<&'_ str, Expression<'_>> {
    space_delimited(delimited(tag("("), expr, tag(")"))).parse(input)
}

fn func_call(input: &'_ str) -> IResult<&'_ str, Expression<'_>> {
    let (r, ident) = space_delimited(identifier).parse(input)?;
    let (r, args) = space_delimited(delimited(
        tag("("),
        many0(delimited(multispace0, expr, space_delimited(opt(tag(","))))),
        tag(")"),
    ))
    .parse(r)?;
    Ok((r, Expression::FnInvoke(ident, args)))
}

fn factor(input: &'_ str) -> IResult<&'_ str, Expression<'_>> {
    alt((number, func_call, ident, parens)).parse(input)
}

fn term(input: &'_ str) -> IResult<&'_ str, Expression<'_>> {
    let (input, init) = factor(input)?;

    fold_many0(
        pair(space_delimited(alt((char('*'), char('/')))), factor),
        move || init.clone(),
        |acc, (op, val): (char, Expression)| match op {
            '*' => Expression::Mul(Box::new(acc), Box::new(val)),
            '/' => Expression::Div(Box::new(acc), Box::new(val)),
            _ => panic!("Multiplicative expression should have '*' or '/' operator"),
        },
    )
    .parse(input)
}

fn expr(input: &'_ str) -> IResult<&'_ str, Expression<'_>> {
    let (input, init) = term(input)?;

    fold_many0(
        pair(space_delimited(alt((char('+'), char('-')))), term),
        move || init.clone(),
        |acc, (op, val): (char, Expression)| match op {
            '+' => Expression::Add(Box::new(acc), Box::new(val)),
            '-' => Expression::Sub(Box::new(acc), Box::new(val)),
            _ => panic!("Additive expression should have '+' or '-' operator"),
        },
    )
    .parse(input)
}

fn var_def(input: &'_ str) -> IResult<&'_ str, Statement<'_>> {
    let (input, _) = space_delimited(tag("var")).parse(input)?;
    let (input, name) = space_delimited(identifier).parse(input)?;
    let (input, _) = space_delimited(char('=')).parse(input)?;
    let (input, expr) = space_delimited(expr).parse(input)?;
    Ok((input, Statement::VarDef(name, expr)))
}

fn var_assign(input: &'_ str) -> IResult<&'_ str, Statement<'_>> {
    let (input, name) = space_delimited(identifier).parse(input)?;
    let (input, _) = space_delimited(char('=')).parse(input)?;
    let (input, expr) = space_delimited(expr).parse(input)?;
    Ok((input, Statement::VarAssign(name, expr)))
}

fn expr_statement(input: &'_ str) -> IResult<&'_ str, Statement<'_>> {
    let (input, res) = expr(input)?;
    Ok((input, Statement::Expression(res)))
}

fn statement(input: &'_ str) -> IResult<&'_ str, Statement<'_>> {
    alt((var_def, var_assign, expr_statement)).parse(input)
}

fn statements(input: &'_ str) -> Result<Statements<'_>, nom::error::Error<&'_ str>> {
    let (_, res) = separated_list0(tag(";"), statement).parse(input).finish()?;
    Ok(res)
}

fn unary_fn(f: fn(f64) -> f64) -> impl Fn(Vec<Expression>, &HashMap<&str, f64>) -> f64 {
    move |args, variables| {
        f(eval(
            args.into_iter().next().expect("function missing argument"),
            variables,
        ))
    }
}

fn binary_fn(f: fn(f64, f64) -> f64) -> impl Fn(Vec<Expression>, &HashMap<&str, f64>) -> f64 {
    move |args, variables| {
        let mut args = args.into_iter();
        let lhs = eval(
            args.next().expect("function missing the first argument"),
            variables,
        );
        let rhs = eval(
            args.next().expect("function missing the second argument"),
            variables,
        );
        f(lhs, rhs)
    }
}

fn eval(expr: Expression, vars: &HashMap<&str, f64>) -> f64 {
    use Expression::*;
    match expr {
        Ident("pi") => std::f64::consts::PI,
        Ident(id) => *vars.get(id).expect("Variable not found"),
        NumLiteral(n) => n,
        FnInvoke("sqrt", args) => unary_fn(f64::sqrt)(args, vars),
        FnInvoke("sin", args) => unary_fn(f64::sin)(args, vars),
        FnInvoke("cos", args) => unary_fn(f64::cos)(args, vars),
        FnInvoke("tan", args) => unary_fn(f64::tan)(args, vars),
        FnInvoke("asin", args) => unary_fn(f64::asin)(args, vars),
        FnInvoke("acos", args) => unary_fn(f64::acos)(args, vars),
        FnInvoke("atan", args) => unary_fn(f64::atan)(args, vars),
        FnInvoke("atan2", args) => binary_fn(f64::atan2)(args, vars),
        FnInvoke("pow", args) => binary_fn(f64::powf)(args, vars),
        FnInvoke("exp", args) => unary_fn(f64::exp)(args, vars),
        FnInvoke("log", args) => binary_fn(f64::log)(args, vars),
        FnInvoke("log10", args) => unary_fn(f64::log10)(args, vars),
        FnInvoke(name, _) => {
            panic!("Unknown function {name:?}")
        }
        Add(lhs, rhs) => eval(*lhs, vars) + eval(*rhs, vars),
        Sub(lhs, rhs) => eval(*lhs, vars) - eval(*rhs, vars),
        Mul(lhs, rhs) => eval(*lhs, vars) * eval(*rhs, vars),
        Div(lhs, rhs) => eval(*lhs, vars) / eval(*rhs, vars),
    }
}

fn main() {
    let mut buf = String::new();
    if !std::io::stdin().read_to_string(&mut buf).is_ok() {
        panic!("Failed to read from stdin");
    }
    let parsed_statements = match statements(&buf) {
        Ok(parsed_statements) => parsed_statements,
        Err(e) => {
            eprintln!("Parse error: {e:?}");
            return;
        }
    };

    let mut variables = HashMap::new();

    for statement in parsed_statements {
        match statement {
            Statement::Expression(expr) => {
                println!("eval: {:?}", eval(expr, &variables));
            }
            Statement::VarDef(name, expr) => {
                let value = eval(expr, &variables);
                variables.insert(name, value);
            }
            Statement::VarAssign(name, expr) => {
                if !variables.contains_key(name) {
                    panic!("Variable is not defined");
                }
                let value = eval(expr, &variables);
                variables.insert(name, value);
            }
        }
    }
}
