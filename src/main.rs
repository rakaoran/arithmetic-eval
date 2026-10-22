use std::{iter::Peekable, process::exit, slice::Iter};

fn main() {
        loop {
                let mut buf: String = String::new();
                let n = std::io::stdin().read_line(&mut buf).unwrap();

                // eof, like in python and node cli you do ctrl-d to quit instead of sending a sigint
                if n == 0 {
                        exit(0)
                }

                let tokens = tokenize(&buf);
                if let Ok(tokens) = tokens {
                        let result = Parser::parse(tokens.iter().peekable());
                        if let Ok(result) = result {
                                println!("= {}", result.evaluate())
                        } else {
                                println!("Syntax error")
                        }
                } else {
                        println!("Unknown symbol")
                }
        }
}

fn tokenize(str: &String) -> Result<Vec<Token>, ()> {
        let mut chars = str.chars().peekable();
        let mut tokens = Vec::<Token>::new();

        while let Some(&c) = chars.peek() {
                match c {
                        ' ' | '\n' | '\t' => {
                                chars.next();
                        }
                        '(' => {
                                tokens.push(Token::LPar);
                                chars.next();
                        }
                        ')' => {
                                tokens.push(Token::RPar);
                                chars.next();
                        }
                        '-' => {
                                tokens.push(Token::Minus);
                                chars.next();
                        }
                        '+' => {
                                tokens.push(Token::Plus);
                                chars.next();
                        }
                        '*' => {
                                tokens.push(Token::Star);
                                chars.next();
                        }
                        '/' => {
                                tokens.push(Token::Slash);
                                chars.next();
                        }
                        '^' => {
                                tokens.push(Token::Power);
                                chars.next();
                        }
                        '0'..='9' | '.' => {
                                let mut num = String::new();
                                while let Some(&d) = chars.peek()
                                        && d.is_ascii_digit()
                                {
                                        num.push(d);
                                        chars.next();
                                }

                                if let Some(&d) = chars.peek()
                                        && d == '.'
                                {
                                        num.push(d);
                                        chars.next();
                                        while let Some(&d) = chars.peek()
                                                && d.is_ascii_digit()
                                        {
                                                num.push(d);
                                                chars.next();
                                        }
                                }

                                tokens.push(Token::Num(num.parse::<f64>().unwrap()));
                        }
                        _ => return Err(()),
                };
        }
        tokens.push(Token::Eof);
        Ok(tokens)
}

#[derive(Debug)]
enum Token {
        Num(f64),
        Plus,
        Power,
        Minus,
        Star,
        Slash,
        LPar,
        RPar,
        Eof,
}

#[derive(Debug, Clone)]
enum Op {
        Add,
        Sub,
        Mul,
        Div,
        Power,
}

#[derive(Debug, Clone)]
enum Expression {
        Num(f64),
        MinusExp(Box<Expression>),
        Binary {
                op: Op,
                lhs: Box<Expression>,
                rhs: Box<Expression>,
        },
}

impl Expression {
        fn evaluate(&self) -> f64 {
                match self {
                        Self::Num(num) => *num,
                        Self::MinusExp(exp) => -*&exp.evaluate(),
                        Self::Binary { op, lhs, rhs } => match op {
                                Op::Add => lhs.evaluate() + rhs.evaluate(),
                                Op::Sub => lhs.evaluate() - rhs.evaluate(),
                                Op::Mul => lhs.evaluate() * rhs.evaluate(),
                                Op::Div => lhs.evaluate() / rhs.evaluate(),
                                Op::Power => lhs.evaluate().powf(rhs.evaluate()),
                        },
                }
        }
}

struct Parser<'a> {
        tokens: Peekable<Iter<'a, Token>>,
}

impl<'a> Parser<'a> {
        fn parse(tokens: Peekable<Iter<'a, Token>>) -> Result<Expression, ()> {
                let mut parser = Self::new(tokens);
                let result = parser.parse_addsub()?;
                if parser.tokens.len() != 1 {
                        return Err(());
                }
                Ok(*result)
        }
        fn new(tokens: Peekable<Iter<'a, Token>>) -> Self {
                Self { tokens }
        }

        fn parse_addsub(&mut self) -> Result<Box<Expression>, ()> {
                let lhs = self.parse_muldiv()?;
                self.parse_addsub_prime(lhs)
        }
        fn parse_addsub_prime(&mut self, mut lhs: Box<Expression>) -> Result<Box<Expression>, ()> {
                let token = self.tokens.peek().unwrap();
                let op = match token {
                        Token::Plus => Some(Op::Add),
                        Token::Minus => Some(Op::Sub),
                        Token::RPar | Token::Eof => None,
                        _ => return Err(()),
                };
                if let Some(op) = op {
                        self.tokens.next();
                        lhs = Box::new(Expression::Binary {
                                op: op,
                                lhs: lhs,
                                rhs: self.parse_muldiv()?,
                        });
                        self.parse_addsub_prime(lhs)
                } else {
                        Ok(lhs)
                }
        }

        fn parse_muldiv(&mut self) -> Result<Box<Expression>, ()> {
                let lhs = self.parse_expo()?;
                self.parse_muldiv_prime(lhs)
        }
        fn parse_muldiv_prime(&mut self, mut lhs: Box<Expression>) -> Result<Box<Expression>, ()> {
                let token = self.tokens.peek().unwrap();
                let op = match token {
                        Token::Star => Some(Op::Mul),
                        Token::Slash => Some(Op::Div),
                        Token::RPar | Token::Eof | Token::Plus | Token::Minus => None,
                        _ => return Err(()),
                };
                if let Some(op) = op {
                        self.tokens.next();
                        lhs = Box::new(Expression::Binary {
                                op: op,
                                lhs: lhs,
                                rhs: self.parse_expo()?,
                        });
                        self.parse_muldiv_prime(lhs)
                } else {
                        Ok(lhs)
                }
        }

        fn parse_expo(&mut self) -> Result<Box<Expression>, ()> {
                let lhs = self.parse_f()?;
                let rhs = self.parse_expo_prime()?;
                if let Expression::MinusExp(exp) = &*lhs {
                        // let exp = Box::new(exp);
                        if let Some(rhs) = rhs {
                                Ok(Box::new(Expression::MinusExp(Box::new(
                                        Expression::Binary {
                                                op: Op::Power,
                                                lhs: exp.clone(),
                                                rhs,
                                        },
                                ))))
                        } else {
                                Ok(lhs)
                        }
                } else {
                        if let Some(rhs) = rhs {
                                Ok(Box::new(Expression::Binary {
                                        op: Op::Power,
                                        lhs,
                                        rhs,
                                }))
                        } else {
                                Ok(lhs)
                        }
                }
        }
        fn parse_expo_prime(&mut self) -> Result<Option<Box<Expression>>, ()> {
                let token = self.tokens.peek().unwrap();
                match token {
                        Token::Power => {
                                self.tokens.next();
                                let lhs = self.parse_f()?;
                                let rhs = self.parse_expo_prime()?;
                                if let Expression::MinusExp(exp) = &*lhs {
                                        if let Some(boxexpl) = rhs {
                                                Ok(Some(Box::new(Expression::MinusExp(Box::new(
                                                        Expression::Binary {
                                                                op: Op::Power,
                                                                lhs: exp.clone(),
                                                                rhs: boxexpl,
                                                        },
                                                )))))
                                        } else {
                                                Ok(Some(lhs))
                                        }
                                } else {
                                        if let Some(boxexpl) = rhs {
                                                Ok(Some(Box::new(Expression::Binary {
                                                        op: Op::Power,
                                                        lhs: lhs,
                                                        rhs: boxexpl,
                                                })))
                                        } else {
                                                Ok(Some(lhs))
                                        }
                                }
                        }
                        Token::RPar
                        | Token::Eof
                        | Token::Plus
                        | Token::Minus
                        | Token::Star
                        | Token::Slash => Ok(None),

                        _ => Err(()),
                }
        }

        fn parse_f(&mut self) -> Result<Box<Expression>, ()> {
                let lookahead = self.tokens.peek().unwrap();
                match lookahead {
                        Token::Num(num) => {
                                self.tokens.next();
                                Ok(Box::new(Expression::Num(*num)))
                        }
                        Token::Minus => {
                                self.tokens.next();
                                Ok(Box::new(Expression::MinusExp(self.parse_f()?)))
                        }
                        Token::Plus => {
                                self.tokens.next();
                                self.parse_f()
                        }
                        Token::LPar => {
                                self.tokens.next();
                                let a = self.parse_addsub()?;
                                let next_token = self.tokens.next().unwrap();
                                if let Token::RPar = next_token {
                                        if let Expression::MinusExp(exp) = &*a {
                                                return Ok(Box::new(Expression::Binary {
                                                        op: Op::Sub,
                                                        lhs: Box::new(Expression::Num(0.0)),
                                                        rhs: exp.clone(),
                                                }));
                                        }
                                        Ok(a)
                                } else {
                                        Err(())
                                }
                        }
                        _ => Err(()),
                }
        }
}

#[cfg(test)]
mod tests {
        use super::*;

        fn evaluate(input: &str) -> Result<f64, ()> {
                let input = input.to_string();
                let tokens = tokenize(&input)?;
                let expression = Parser::parse(tokens.iter().peekable())?;
                Ok(expression.evaluate())
        }

        fn assert_evaluates_to(input: &str, expected: f64) {
                let actual = evaluate(input).unwrap();
                assert!(
                        (actual - expected).abs() < 1e-10,
                        "expected {input} to equal {expected}, got {actual}"
                );
        }

        // Normal cases

        #[test]
        fn adds_two_integers() {
                assert_evaluates_to("2 + 3", 5.0);
        }

        #[test]
        fn adds_decimal_numbers() {
                assert_evaluates_to("1.25 + 2.75", 4.0);
        }

        #[test]
        fn subtracts_from_left_to_right() {
                assert_evaluates_to("20 - 5 - 3", 12.0);
        }

        #[test]
        fn multiplication_precedes_addition() {
                assert_evaluates_to("2 + 3 * 4", 14.0);
        }

        #[test]
        fn parentheses_override_precedence() {
                assert_evaluates_to("(2 + 3) * 4", 20.0);
        }

        #[test]
        fn multiplication_and_division_are_left_associative() {
                assert_evaluates_to("24 / 3 * 2", 16.0);
        }

        #[test]
        fn evaluates_nested_parentheses() {
                assert_evaluates_to("((2 + 3) * (7 - 4)) / 5", 3.0);
        }

        #[test]
        fn raises_number_to_power() {
                assert_evaluates_to("2 ^ 10", 1024.0);
        }

        #[test]
        fn exponentiation_is_right_associative() {
                assert_evaluates_to("2 ^ 3 ^ 2", 512.0);
        }

        #[test]
        fn evaluates_complex_expression() {
                assert_evaluates_to("((3 + 5) * 2 ^ 3 - 4) / (7 - 3)", 15.0);
        }

        // Edge cases

        #[test]
        fn exponentiation_precedes_unary_minus() {
                assert_evaluates_to("-2 ^ 2", -4.0);
        }

        #[test]
        fn parenthesized_negative_base_is_squared() {
                assert_evaluates_to("(-2) ^ 2", 4.0);
        }

        #[test]
        fn explicitly_negated_power_stays_negative() {
                assert_evaluates_to("-(2 ^ 2)", -4.0);
        }

        #[test]
        fn power_inside_negative_exponent_is_evaluated_first() {
                assert_evaluates_to("2 ^ -2 ^ 2", 0.0625);
        }

        #[test]
        fn unary_minus_after_multiplication_has_lower_precedence_than_power() {
                assert_evaluates_to("3 * -2 ^ 2", -12.0);
        }

        #[test]
        fn accepts_decimal_without_leading_zero() {
                assert_evaluates_to(".5 + .25", 0.75);
        }

        #[test]
        fn accepts_decimal_with_trailing_point() {
                assert_evaluates_to("5. + 2.", 7.0);
        }

        #[test]
        fn rejects_number_with_multiple_decimal_points() {
                assert!(evaluate("1.2.3").is_err());
        }

        #[test]
        fn rejects_scientific_notation_when_unsupported() {
                assert!(evaluate("1e3 + 2").is_err());
        }

        #[test]
        fn division_by_zero_is_infinite() {
                assert!(evaluate("1 / 0").unwrap().is_infinite());
        }

        #[test]
        fn zero_divided_by_zero_is_nan() {
                assert!(evaluate("0 / 0").unwrap().is_nan());
        }

        #[test]
        fn rejects_empty_expression() {
                assert!(evaluate("").is_err());
        }

        #[test]
        fn rejects_whitespace_only_expression() {
                assert!(evaluate(" \t\n ").is_err());
        }

        #[test]
        fn rejects_empty_parentheses() {
                assert!(evaluate("()").is_err());
        }

        #[test]
        fn rejects_missing_closing_parenthesis() {
                assert!(evaluate("2 * (3 + 4").is_err());
        }

        #[test]
        fn rejects_unexpected_closing_parenthesis() {
                assert!(evaluate("2 + 3)").is_err());
        }

        #[test]
        fn rejects_missing_left_operand() {
                assert!(evaluate("* 2").is_err());
        }

        #[test]
        fn rejects_missing_right_operand() {
                assert!(evaluate("2 +").is_err());
        }

        #[test]
        fn rejects_consecutive_binary_operators() {
                assert!(evaluate("2 * / 3").is_err());
        }

        #[test]
        fn rejects_implicit_multiplication() {
                assert!(evaluate("(2 + 1)(3 + 1)").is_err());
        }
}
