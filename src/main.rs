use std::{collections::HashMap, io::Read, ops::ControlFlow};

use nom::{
    Finish, IResult, Parser,
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, multispace0, none_of},
    combinator::{opt, recognize},
    error::ParseError,
    multi::{fold_many0, many0, separated_list0},
    number::complete::recognize_float,
    sequence::{delimited, pair, preceded, terminated},
};

#[derive(Debug, PartialEq, Clone)]
enum Value {
    F64(f64),
    I64(i64),
    Str(String),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::F64(v) => write!(f, "{v}"),
            Self::I64(v) => write!(f, "{v}"),
            Self::Str(v) => write!(f, "{v}"),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use Value::*;
        match (self, other) {
            (F64(lhs), F64(rhs)) => lhs.partial_cmp(rhs),
            (I64(lhs), I64(rhs)) => lhs.partial_cmp(rhs),
            (F64(lhs), I64(rhs)) => lhs.partial_cmp(&(*rhs as f64)),
            (I64(lhs), F64(rhs)) => (*lhs as f64).partial_cmp(rhs),
            (Str(lhs), Str(rhs)) => lhs.partial_cmp(rhs),
            _ => None,
        }
    }
}

impl Value {
    fn as_i64(&self) -> Option<i64> {
        match self {
            Self::F64(val) => Some(*val as i64),
            Self::I64(val) => Some(*val),
            Self::Str(val) => val.parse().ok(),
        }
    }
}

fn coerce_f64(a: &Value) -> f64 {
    match a {
        Value::F64(v) => *v as f64,
        Value::I64(v) => *v as f64,
        _ => panic!("The string could not be parsed as f64"),
    }
}

fn coerce_i64(a: &Value) -> i64 {
    match a {
        Value::F64(v) => *v as i64,
        Value::I64(v) => *v as i64,
        _ => panic!("The string could not be parsed as i64"),
    }
}

fn coerce_str(a: &Value) -> String {
    match a {
        Value::F64(v) => v.to_string(),
        Value::I64(v) => v.to_string(),
        Value::Str(v) => v.clone(),
    }
}

fn binary_op_str(
    lhs: &Value,
    rhs: &Value,
    d: impl Fn(f64, f64) -> f64,
    i: impl Fn(i64, i64) -> i64,
    s: impl Fn(&str, &str) -> String,
) -> Value {
    use Value::*;
    match (lhs, rhs) {
        (F64(lhs), rhs) => F64(d(*lhs, coerce_f64(&rhs))),
        (lhs, F64(rhs)) => F64(d(coerce_f64(&lhs), *rhs)),
        (I64(lhs), I64(rhs)) => I64(i(*lhs, *rhs)),
        (Str(lhs), Str(rhs)) => Str(s(lhs, rhs)),
        _ => panic!("Unsupported operator between {:?} and {:?}", lhs, rhs),
    }
}

impl std::ops::Add for Value {
    type Output = Value;

    fn add(self, rhs: Self) -> Self::Output {
        binary_op_str(
            &self,
            &rhs,
            |lhs, rhs| lhs + rhs,
            |lhs, rhs| lhs + rhs,
            |lhs, rhs| lhs.to_owned() + rhs,
        )
    }
}

impl std::ops::Sub for Value {
    type Output = Value;

    fn sub(self, rhs: Self) -> Self::Output {
        binary_op_str(
            &self,
            &rhs,
            |lhs, rhs| lhs - rhs,
            |lhs, rhs| lhs - rhs,
            |_, _| panic!("Strings cannot be subtracted"),
        )
    }
}

impl std::ops::Mul for Value {
    type Output = Value;

    fn mul(self, rhs: Self) -> Self::Output {
        binary_op_str(
            &self,
            &rhs,
            |lhs, rhs| lhs * rhs,
            |lhs, rhs| lhs * rhs,
            |_, _| panic!("Strings cannot be multiplied"),
        )
    }
}

impl std::ops::Div for Value {
    type Output = Value;

    fn div(self, rhs: Self) -> Self::Output {
        binary_op_str(
            &self,
            &rhs,
            |lhs, rhs| lhs / rhs,
            |lhs, rhs| lhs / rhs,
            |_, _| panic!("Strings cannot be divided"),
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
enum Expression<'src> {
    Ident(&'src str),
    NumLiteral(f64),
    StrLiteral(String),
    FnInvoke(&'src str, Vec<Expression<'src>>),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Sub(Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
    Div(Box<Expression<'src>>, Box<Expression<'src>>),
    Gt(Box<Expression<'src>>, Box<Expression<'src>>),
    Lt(Box<Expression<'src>>, Box<Expression<'src>>),
    If(
        Box<Expression<'src>>,
        Box<Statements<'src>>,
        Option<Box<Statements<'src>>>,
    ),
}

#[derive(Debug, PartialEq, Clone)]
enum Statement<'src> {
    Expression(Expression<'src>),
    VarDef(&'src str, Expression<'src>),
    VarAssign(&'src str, Expression<'src>),
    For {
        loop_var: &'src str,
        start: Expression<'src>,
        end: Expression<'src>,
        stmts: Statements<'src>,
    },
    FnDef {
        name: &'src str,
        args: Vec<&'src str>,
        stmts: Statements<'src>,
    },
    Return(Expression<'src>),
    Break,
    Continue,
}

type Statements<'a> = Vec<Statement<'a>>;

struct UserFn<'src> {
    args: Vec<&'src str>,
    stmts: Statements<'src>,
}

struct NativeFn {
    code: Box<dyn Fn(&[Value]) -> Value>,
}

enum FnDef<'src> {
    User(UserFn<'src>),
    Native(NativeFn),
}

impl<'src> FnDef<'src> {
    fn call(&self, args: &[Value], frame: &StackFrame) -> Value {
        match self {
            Self::User(code) => {
                let mut new_frame = StackFrame::push_stack(frame);
                new_frame.vars = args
                    .iter()
                    .zip(code.args.iter())
                    .map(|(arg, name)| (name.to_string(), arg.clone()))
                    .collect();
                match eval_stmts(&code.stmts, &mut new_frame) {
                    EvalResult::Continue(val) | EvalResult::Break(BreakResult::Return(val)) => val,
                    EvalResult::Break(BreakResult::Break) => {
                        panic!("Breaking outside loop is prohibited")
                    }
                    EvalResult::Break(BreakResult::Continue) => {
                        panic!("Continuing outside loop is prohibited")
                    }
                }
            }
            Self::Native(code) => (code.code)(args),
        }
    }
}

type Variables = HashMap<String, Value>;
type Functions<'src> = HashMap<String, FnDef<'src>>;

struct StackFrame<'src> {
    vars: Variables,
    funcs: Functions<'src>,
    uplevel: Option<&'src StackFrame<'src>>,
}

fn print(values: &[Value]) -> Value {
    println!("print: {}", values[0]);
    Value::I64(0)
}

fn p_dbg(values: &[Value]) -> Value {
    println!("dbg: {:?}", values[0]);
    Value::I64(0)
}

impl<'src> StackFrame<'src> {
    fn new() -> Self {
        let mut funcs = Functions::new();
        funcs.insert("sqrt".to_string(), unary_fn(f64::sqrt));
        funcs.insert("sin".to_string(), unary_fn(f64::sin));
        funcs.insert("cos".to_string(), unary_fn(f64::cos));
        funcs.insert("tan".to_string(), unary_fn(f64::tan));
        funcs.insert("asin".to_string(), unary_fn(f64::asin));
        funcs.insert("acos".to_string(), unary_fn(f64::acos));
        funcs.insert("atan".to_string(), unary_fn(f64::atan));
        funcs.insert("atan2".to_string(), binary_fn(f64::atan2));
        funcs.insert("pow".to_string(), binary_fn(f64::powf));
        funcs.insert("exp".to_string(), unary_fn(f64::exp));
        funcs.insert("log".to_string(), binary_fn(f64::log));
        funcs.insert("log10".to_string(), unary_fn(f64::log10));
        funcs.insert(
            "print".to_string(),
            FnDef::Native(NativeFn {
                code: Box::new(print),
            }),
        );
        funcs.insert(
            "dbg".to_string(),
            FnDef::Native(NativeFn {
                code: Box::new(p_dbg),
            }),
        );
        funcs.insert(
            "i64".to_string(),
            FnDef::Native(NativeFn {
                code: Box::new(move |args| {
                    Value::I64(coerce_i64(args.first().expect("function missing argument")))
                }),
            }),
        );
        funcs.insert(
            "f64".to_string(),
            FnDef::Native(NativeFn {
                code: Box::new(move |args| {
                    Value::F64(coerce_f64(args.first().expect("function missing argument")))
                }),
            }),
        );
        funcs.insert(
            "str".to_string(),
            FnDef::Native(NativeFn {
                code: Box::new(move |args| {
                    Value::Str(coerce_str(args.first().expect("function missing argument")))
                }),
            }),
        );
        Self {
            vars: Variables::new(),
            funcs,
            uplevel: None,
        }
    }

    fn push_stack(uplevel: &'src Self) -> Self {
        Self {
            vars: HashMap::new(),
            funcs: HashMap::new(),
            uplevel: Some(uplevel),
        }
    }

    fn get_fn(&self, name: &str) -> Option<&FnDef<'src>> {
        let mut next_frame = Some(self);
        while let Some(frame) = next_frame {
            if let Some(func) = frame.funcs.get(name) {
                return Some(func);
            }
            next_frame = frame.uplevel;
        }
        None
    }
}

fn space_delimited<'src, O, E>(
    f: impl Parser<&'src str, Output = O, Error = E>,
) -> impl Parser<&'src str, Output = O, Error = E>
where
    E: ParseError<&'src str>,
{
    delimited(multispace0, f, multispace0)
}

fn open_brace(input: &str) -> IResult<&str, ()> {
    let (input, _) = space_delimited(char('{')).parse(input)?;
    Ok((input, ()))
}

fn close_brace(input: &str) -> IResult<&str, ()> {
    let (input, _) = space_delimited(char('}')).parse(input)?;
    Ok((input, ()))
}

fn num_literal(input: &'_ str) -> IResult<&'_ str, Expression<'_>> {
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

fn str_literal(input: &'_ str) -> IResult<&'_ str, Expression<'_>> {
    let (input, _) = preceded(multispace0, char('"')).parse(input)?;
    let (input, val) = many0(none_of("\"")).parse(input)?;
    let (input, _) = terminated(char('"'), multispace0).parse(input)?;
    Ok((
        input,
        Expression::StrLiteral(
            val.iter()
                .collect::<String>()
                .replace("\\\\", "\\")
                .replace("\\n", "\n"),
        ),
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
    alt((str_literal, num_literal, func_call, ident, parens)).parse(input)
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

fn num_expr(input: &'_ str) -> IResult<&'_ str, Expression<'_>> {
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

fn cond_expr(input: &'_ str) -> IResult<&'_ str, Expression<'_>> {
    let (input, first) = num_expr(input)?;
    let (input, cond) = space_delimited(alt((char('<'), char('>')))).parse(input)?;
    let (input, second) = num_expr(input)?;

    Ok((
        input,
        match cond {
            '<' => Expression::Lt(Box::new(first), Box::new(second)),
            '>' => Expression::Gt(Box::new(first), Box::new(second)),
            _ => unreachable!(),
        },
    ))
}

fn if_expr(input: &'_ str) -> IResult<&'_ str, Expression<'_>> {
    let (input, _) = space_delimited(tag("if")).parse(input)?;
    let (input, cond) = expr(input)?;
    let (input, t_case) = delimited(open_brace, statements, close_brace).parse(input)?;
    let (input, f_case) = opt(preceded(
        space_delimited(tag("else")),
        delimited(open_brace, statements, close_brace),
    ))
    .parse(input)?;

    Ok((
        input,
        Expression::If(Box::new(cond), Box::new(t_case), f_case.map(Box::new)),
    ))
}

fn expr(input: &'_ str) -> IResult<&'_ str, Expression<'_>> {
    alt((if_expr, cond_expr, num_expr)).parse(input)
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

fn for_statement(input: &'_ str) -> IResult<&'_ str, Statement<'_>> {
    let (input, _) = space_delimited(tag("for")).parse(input)?;
    let (input, loop_var) = space_delimited(identifier).parse(input)?;
    let (input, _) = space_delimited(tag("in")).parse(input)?;
    let (input, start) = space_delimited(expr).parse(input)?;
    let (input, _) = space_delimited(tag("to")).parse(input)?;
    let (input, end) = space_delimited(expr).parse(input)?;
    let (input, stmts) = delimited(open_brace, statements, close_brace).parse(input)?;
    Ok((
        input,
        Statement::For {
            loop_var,
            start,
            end,
            stmts,
        },
    ))
}

fn fn_def_statement(input: &'_ str) -> IResult<&'_ str, Statement<'_>> {
    let (input, _) = space_delimited(tag("fn")).parse(input)?;
    let (input, name) = space_delimited(identifier).parse(input)?;
    let (input, _) = space_delimited(tag("(")).parse(input)?;
    let (input, args) = separated_list0(char(','), space_delimited(identifier)).parse(input)?;
    let (input, _) = space_delimited(tag(")")).parse(input)?;
    let (input, stmts) = delimited(open_brace, statements, close_brace).parse(input)?;
    Ok((input, Statement::FnDef { name, args, stmts }))
}

fn return_statement(input: &'_ str) -> IResult<&'_ str, Statement<'_>> {
    let (input, _) = space_delimited(tag("return")).parse(input)?;
    let (input, ex) = space_delimited(expr).parse(input)?;
    Ok((input, Statement::Return(ex)))
}

fn break_statement(input: &'_ str) -> IResult<&'_ str, Statement<'_>> {
    let (input, _) = space_delimited(tag("break")).parse(input)?;
    Ok((input, Statement::Break))
}

fn continue_statement(input: &'_ str) -> IResult<&'_ str, Statement<'_>> {
    let (input, _) = space_delimited(tag("continue")).parse(input)?;
    Ok((input, Statement::Continue))
}

fn statement(input: &'_ str) -> IResult<&'_ str, Statement<'_>> {
    alt((
        for_statement,
        fn_def_statement,
        terminated(
            alt((
                var_def,
                var_assign,
                return_statement,
                break_statement,
                continue_statement,
                expr_statement,
            )),
            char(';'),
        ),
    ))
    .parse(input)
}

fn statements(input: &'_ str) -> IResult<&'_ str, Statements<'_>> {
    let (input, stmts) = many0(statement).parse(input)?;
    let (input, _) = opt(char(';')).parse(input)?;
    Ok((input, stmts))
}

fn statements_finish(input: &'_ str) -> Result<Statements<'_>, nom::error::Error<&'_ str>> {
    let (_, res) = statements(input).finish()?;
    Ok(res)
}

fn unary_fn<'a>(f: fn(f64) -> f64) -> FnDef<'a> {
    FnDef::Native(NativeFn {
        code: Box::new(move |args| {
            Value::F64(f(coerce_f64(
                args.into_iter().next().expect("function missing argument"),
            )))
        }),
    })
}

fn binary_fn<'a>(f: fn(f64, f64) -> f64) -> FnDef<'a> {
    FnDef::Native(NativeFn {
        code: Box::new(move |args| {
            let mut args = args.into_iter();
            let lhs = coerce_f64(args.next().expect("function missing the first argument"));
            let rhs = coerce_f64(args.next().expect("function missing the second argument"));
            Value::F64(f(lhs, rhs))
        }),
    })
}

#[derive(Debug)]
enum BreakResult {
    Return(Value),
    Break,
    Continue,
}

type EvalResult = ControlFlow<BreakResult, Value>;

fn eval<'src>(expr: &Expression<'src>, frame: &mut StackFrame<'src>) -> EvalResult {
    use Expression::*;
    let res = match expr {
        Ident("pi") => Value::F64(std::f64::consts::PI),
        Ident(id) => frame
            .vars
            .get(*id)
            .cloned()
            .expect(&format!("Variable not found: {id}")),
        NumLiteral(n) => Value::F64(*n),
        StrLiteral(s) => Value::Str(s.clone()),
        FnInvoke(name, args) => {
            let mut arg_vals = vec![];
            for arg in args.iter() {
                arg_vals.push(eval(arg, frame)?);
            }
            if let Some(func) = frame.get_fn(*name) {
                func.call(&arg_vals, frame)
            } else {
                panic!("Unknown function {name:?}");
            }
        }
        Add(lhs, rhs) => eval(lhs, frame)? + eval(rhs, frame)?,
        Sub(lhs, rhs) => eval(lhs, frame)? - eval(rhs, frame)?,
        Mul(lhs, rhs) => eval(lhs, frame)? * eval(rhs, frame)?,
        Div(lhs, rhs) => eval(lhs, frame)? / eval(rhs, frame)?,
        Gt(lhs, rhs) => {
            if eval(lhs, frame)? > eval(rhs, frame)? {
                Value::I64(1)
            } else {
                Value::I64(0)
            }
        }
        Lt(lhs, rhs) => {
            if eval(lhs, frame)? < eval(rhs, frame)? {
                Value::I64(1)
            } else {
                Value::I64(0)
            }
        }
        If(cond, t_case, f_case) => {
            if coerce_i64(&eval(cond, frame)?) != 0 {
                eval_stmts(t_case, frame)?
            } else if let Some(f_case) = f_case {
                eval_stmts(f_case, frame)?
            } else {
                Value::I64(0)
            }
        }
    };
    EvalResult::Continue(res)
}

fn eval_stmts<'src>(stmts: &[Statement<'src>], frame: &mut StackFrame<'src>) -> EvalResult {
    let mut last_result = EvalResult::Continue(Value::I64(0));
    for statement in stmts {
        match statement {
            Statement::Expression(expr) => {
                last_result = EvalResult::Continue(eval(expr, frame)?);
            }
            Statement::VarDef(name, expr) => {
                let value = eval(expr, frame)?;
                frame.vars.insert(name.to_string(), value);
            }
            Statement::VarAssign(name, expr) => {
                if !frame.vars.contains_key(*name) {
                    panic!("Variable is not defined");
                }
                let value = eval(expr, frame)?;
                frame.vars.insert(name.to_string(), value);
            }
            Statement::For {
                loop_var,
                start,
                end,
                stmts,
            } => {
                let start = eval(start, frame)?
                    .as_i64()
                    .expect("Start needs to be integer");
                let end = eval(end, frame)?.as_i64().expect("End needs to be integer");
                for i in start..end {
                    frame.vars.insert(loop_var.to_string(), Value::I64(i));
                    match eval_stmts(stmts, frame) {
                        EvalResult::Continue(val) => last_result = EvalResult::Continue(val),
                        EvalResult::Break(BreakResult::Return(val)) => {
                            return EvalResult::Break(BreakResult::Return(val));
                        }
                        EvalResult::Break(BreakResult::Break) => break,
                        EvalResult::Break(BreakResult::Continue) => continue,
                    }
                }
            }
            Statement::FnDef { name, args, stmts } => {
                frame.funcs.insert(
                    name.to_string(),
                    FnDef::User(UserFn {
                        args: args.clone(),
                        stmts: stmts.clone(),
                    }),
                );
            }
            Statement::Return(expr) => {
                return EvalResult::Break(BreakResult::Return(eval(expr, frame)?));
            }
            Statement::Break => return EvalResult::Break(BreakResult::Break),
            Statement::Continue => return EvalResult::Break(BreakResult::Continue),
        }
    }
    last_result
}

fn main() {
    let mut buf = String::new();
    if !std::io::stdin().read_to_string(&mut buf).is_ok() {
        panic!("Failed to read from stdin");
    }
    let parsed_statements = match statements_finish(&buf) {
        Ok(parsed_statements) => parsed_statements,
        Err(e) => {
            eprintln!("Parse error: {e:?}");
            return;
        }
    };

    let mut frame = StackFrame::new();

    let _ = eval_stmts(&parsed_statements, &mut frame);
}
