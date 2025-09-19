use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, multispace0},
    combinator::recognize,
    error::ParseError,
    multi::{fold_many0, many0},
    number::complete::recognize_float,
    sequence::{delimited, pair},
};

#[derive(Debug, PartialEq, Clone)]
enum Expression<'src> {
    Ident(&'src str),
    NumLiteral(f64),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Sub(Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
    Div(Box<Expression<'src>>, Box<Expression<'src>>),
}

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

fn factor(input: &'_ str) -> IResult<&'_ str, Expression<'_>> {
    alt((number, ident, parens)).parse(input)
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

fn eval(expr: Expression) -> f64 {
    match expr {
        Expression::Ident("pi") => std::f64::consts::PI,
        Expression::Ident(id) => panic!("Unknown name {:?}", id),
        Expression::NumLiteral(n) => n,
        Expression::Add(lhs, rhs) => eval(*lhs) + eval(*rhs),
        Expression::Sub(lhs, rhs) => eval(*lhs) - eval(*rhs),
        Expression::Mul(lhs, rhs) => eval(*lhs) * eval(*rhs),
        Expression::Div(lhs, rhs) => eval(*lhs) / eval(*rhs),
    }
}

fn main() {
    fn ex_eval<'src>(input: &'src str) -> Result<f64, nom::Err<nom::error::Error<&'src str>>> {
        expr(input).map(|(_, e)| eval(e))
    }

    let input = "123";
    println!("source: {:?}, parsed: {:?}", input, ex_eval(input));

    let input = "2 * pi";
    println!("source: {:?}, parsed: {:?}", input, ex_eval(input));

    let input = "(123 + 456 ) + pi";
    println!("source: {:?}, parsed: {:?}", input, ex_eval(input));

    let input = "10 - (100 + 1)";
    println!("source: {:?}, parsed: {:?}", input, ex_eval(input));

    let input = "(3 + 7) / (2 + 3)";
    println!("source: {:?}, parsed: {:?}", input, ex_eval(input));
}
