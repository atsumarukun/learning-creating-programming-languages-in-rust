#[derive(Debug, PartialEq)]
enum Token<'src> {
    Ident(&'src str),
    Number(f64),
    LParen,
    RParen,
}

#[derive(Debug, PartialEq)]
enum TokenTree<'src> {
    Token(Token<'src>),
    Tree(Vec<TokenTree<'src>>),
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

fn number(mut input: &'_ str) -> Option<(&'_ str, Token<'_>)> {
    let start = input;
    if matches!(peek_char(input), Some('-' | '+' | '.' | '0'..='9')) {
        input = advance_char(input);
        while matches!(peek_char(input), Some('.' | '0'..='9')) {
            input = advance_char(input);
        }
        if let Ok(num) = start[..(start.len() - input.len())].parse::<f64>() {
            return Some((input, Token::Number(num)));
        }
        return None;
    }
    None
}

fn ident(mut input: &'_ str) -> Option<(&'_ str, Token<'_>)> {
    let start = input;
    if matches!(peek_char(input), Some('a'..='z' | 'A'..='Z')) {
        input = advance_char(input);
        while matches!(peek_char(input), Some('a'..='z' | 'A'..='Z' | '0'..='9')) {
            input = advance_char(input);
        }
        return Some((input, Token::Ident(&start[..(start.len() - input.len())])));
    }
    None
}

fn lparen(mut input: &'_ str) -> Option<(&'_ str, Token<'_>)> {
    if matches!(peek_char(input), Some('(')) {
        input = advance_char(input);
        return Some((input, Token::LParen));
    }
    None
}

fn rparen(mut input: &'_ str) -> Option<(&'_ str, Token<'_>)> {
    if matches!(peek_char(input), Some(')')) {
        input = advance_char(input);
        return Some((input, Token::RParen));
    }
    None
}

fn token(i: &'_ str) -> Option<(&'_ str, Token<'_>)> {
    if let Some(res) = ident(whitespace(i)) {
        return Some(res);
    }

    if let Some(res) = number(whitespace(i)) {
        return Some(res);
    }

    if let Some(res) = lparen(whitespace(i)) {
        return Some(res);
    }

    if let Some(res) = rparen(whitespace(i)) {
        return Some(res);
    }

    None
}

fn source(mut input: &'_ str) -> (&'_ str, TokenTree<'_>) {
    let mut tokens = vec![];
    while !input.is_empty() {
        input = if let Some((next_input, token)) = token(input) {
            match token {
                Token::LParen => {
                    let (next_input, tt) = source(next_input);
                    tokens.push(tt);
                    next_input
                }
                Token::RParen => return (next_input, TokenTree::Tree(tokens)),
                _ => {
                    tokens.push(TokenTree::Token(token));
                    next_input
                }
            }
        } else {
            break;
        }
    }
    (input, TokenTree::Tree(tokens))
}

fn main() {
    let s = "Hello world";
    println!("source: {:?}, parsed:\n {:?}", s, source(s));

    let s = "(123  456 ) world";
    println!("source: {:?}, parsed:\n {:?}", s, source(s));

    let s = "((car cdr) cdr)";
    println!("source: {:?}, parsed:\n {:?}", s, source(s));

    let s = "()())))((()))";
    println!("source: {:?}, parsed:\n {:?}", s, source(s));
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
        assert_eq!(number("-123.45"), Some(("", Token::Number(-123.45))));
    }

    #[test]
    fn test_ident() {
        assert_eq!(ident("Adam"), Some(("", Token::Ident("Adam"))));
    }
}
