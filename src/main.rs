#[derive(Debug, PartialEq)]
enum Expression<'src> {
    Ident(&'src str),
    NumLiteral(f64),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
}

fn advance_char(input: &str) -> &str {
    let mut chars = input.chars();
    chars.next();
    chars.as_str()
}

fn peek_char(input: &str) -> Option<char> {
    input.chars().next()
}

fn whitespace(mut input: &str) -> &str {
    while matches!(peek_char(input), Some(' ')) {
        input = advance_char(input);
    }
    input
}

fn number(mut input: &'_ str) -> Option<(&'_ str, Expression<'_>)> {
    let start = input;
    if matches!(peek_char(input), Some('-' | '+' | '.' | '0'..='9')) {
        input = advance_char(input);
        while matches!(peek_char(input), Some('.' | '0'..='9')) {
            input = advance_char(input);
        }
        if let Ok(num) = start[..(start.len() - input.len())].parse::<f64>() {
            return Some((input, Expression::NumLiteral(num)));
        }
        return None;
    }
    None
}

fn ident(mut input: &'_ str) -> Option<(&'_ str, Expression<'_>)> {
    let start = input;
    if matches!(peek_char(input), Some('a'..='z' | 'A'..='Z')) {
        input = advance_char(input);
        while matches!(peek_char(input), Some('a'..='z' | 'A'..='Z' | '0'..='9')) {
            input = advance_char(input);
        }
        return Some((
            input,
            Expression::Ident(&start[..(start.len() - input.len())]),
        ));
    }
    None
}

fn lparen(input: &str) -> Option<&str> {
    if matches!(peek_char(input), Some('(')) {
        return Some(advance_char(input));
    }
    None
}

fn rparen(input: &str) -> Option<&str> {
    if matches!(peek_char(input), Some(')')) {
        return Some(advance_char(input));
    }
    None
}

fn plus(input: &str) -> Option<&str> {
    if matches!(peek_char(input), Some('+')) {
        return Some(advance_char(input));
    }
    None
}

fn token(i: &'_ str) -> Option<(&'_ str, Expression<'_>)> {
    if let Some(res) = ident(whitespace(i)) {
        return Some(res);
    }

    if let Some(res) = number(whitespace(i)) {
        return Some(res);
    }

    None
}

fn paren(input: &'_ str) -> Option<(&'_ str, Expression<'_>)> {
    let next_input = lparen(whitespace(input))?;

    let (next_input, expr) = expr(next_input)?;

    let next_input = rparen(whitespace(next_input))?;

    Some((next_input, expr))
}

fn term(input: &'_ str) -> Option<(&'_ str, Expression<'_>)> {
    if let Some(res) = paren(input) {
        return Some(res);
    }

    if let Some(res) = token(input) {
        return Some(res);
    }

    None
}

fn add_term(input: &'_ str) -> Option<(&'_ str, Expression<'_>)> {
    let (next_input, lhs) = term(input)?;

    let next_input = plus(whitespace(next_input))?;

    Some((next_input, lhs))
}

fn add(mut input: &'_ str) -> Option<(&'_ str, Expression<'_>)> {
    let mut left = None;
    while let Some((next_input, expr)) = add_term(input) {
        if let Some(prev_left) = left {
            left = Some(Expression::Add(Box::new(prev_left), Box::new(expr)));
        } else {
            left = Some(expr);
        }
        input = next_input;
    }
    let left = left?;

    let (next_input, rhs) = expr(input)?;

    Some((next_input, Expression::Add(Box::new(left), Box::new(rhs))))
}

fn expr(input: &'_ str) -> Option<(&'_ str, Expression<'_>)> {
    if let Some(res) = add(input) {
        return Some(res);
    }

    if let Some(res) = term(input) {
        return Some(res);
    }

    None
}

fn main() {
    let input = "123";
    println!("source: {:?}, parsed: {:?}", input, expr(input));

    let input = "Hello + world";
    println!("source: {:?}, parsed: {:?}", input, expr(input));

    let input = "(123 + 456 ) + world";
    println!("source: {:?}, parsed: {:?}", input, expr(input));

    let input = "car + cdr + cdr";
    println!("source: {:?}, parsed: {:?}", input, expr(input));

    let input = "((1 + 2) + (3 + 4)) + 5 + 6";
    println!("source: {:?}, parsed: {:?}", input, expr(input));
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_whitespace() {
        assert_eq!(whitespace("    "), "");
    }

    #[test]
    fn test_number() {
        assert_eq!(
            number("-123.45"),
            Some((" ", Expression::NumLiteral(-123.45)))
        );
    }

    #[test]
    fn test_ident() {
        assert_eq!(ident("Adam"), Some(("", Expression::Ident("Adam"))));
    }
}
