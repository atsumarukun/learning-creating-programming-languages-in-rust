fn whitespace(mut input: &str) -> &str {
    while matches!(input.chars().next(), Some(' ')) {
        let mut chars = input.chars();
        chars.next();
        input = chars.as_str();
    }
    input
}

fn number(mut input: &str) -> &str {
    if matches!(input.chars().next(), Some('-' | '+' | '.' | '0'..='9')) {
        let mut chars = input.chars();
        chars.next();
        input = chars.as_str();
        while matches!(input.chars().next(), Some('.' | '0'..='9')) {
            let mut chars = input.chars();
            chars.next();
            input = chars.as_str();
        }
    }
    input
}

fn ident(mut input: &str) -> &str {
    if matches!(input.chars().next(), Some('a'..='z' | 'A'..='Z')) {
        while matches!(
            input.chars().next(),
            Some('a'..='z' | 'A'..='Z' | '0'..='9')
        ) {
            let mut chars = input.chars();
            chars.next();
            input = chars.as_str();
        }
    }
    input
}

fn main() {}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_whitespace() {
        assert_eq!(whitespace("    "), "");
    }

    #[test]
    fn test_number() {
        assert_eq!(number("-123.45"), "")
    }

    #[test]
    fn test_ident() {
        assert_eq!(ident("Adam"), "")
    }
}
