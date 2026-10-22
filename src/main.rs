use std::{iter::Peekable, slice::Iter};

fn main() {
        loop {
                let mut buf: String = String::new();
                std::io::stdin().read_line(&mut buf).unwrap();

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
                        '0'..='9' => {
                                let mut left_digits: Vec<u32> = Vec::new();
                                let mut right_digits: Vec<u32> = Vec::new();
                                while let Some(&d) = chars.peek()
                                        && d.is_ascii_digit()
                                {
                                        left_digits.push(d.to_digit(10).unwrap());
                                        chars.next();
                                }

                                if let Some(&d) = chars.peek()
                                        && d == '.'
                                {
                                        chars.next();
                                        while let Some(&d) = chars.peek()
                                                && d.is_ascii_digit()
                                        {
                                                right_digits.push(d.to_digit(10).unwrap());
                                                chars.next();
                                        }
                                }

                                let mut num: f64 = 0.0;
                                let mut tens = 1.0;
                                left_digits.reverse();
                                for digit in left_digits {
                                        num = num + digit as f64 * tens;
                                        tens *= 10.0;
                                }
                                let mut decimals = 0.1;
                                for digit in right_digits {
                                        num = num + (digit as f64 * decimals);
                                        decimals /= 10.0;
                                }
                                tokens.push(Token::Num(num));
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

#[derive(Debug)]
enum Op {
        Add,
        Sub,
        Mul,
        Div,
        Power,
}

#[derive(Debug)]
enum Expression {
        Num(f64),
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
        fn parse_expo_prime(&mut self) -> Result<Option<Box<Expression>>, ()> {
                let token = self.tokens.peek().unwrap();
                match token {
                        Token::Power => {
                                self.tokens.next();
                                let lhs = self.parse_f()?;
                                let rhs = self.parse_expo_prime()?;
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
                                Ok(Box::new(Expression::Binary {
                                        op: Op::Sub,
                                        lhs: Box::new(Expression::Num(0.0)),
                                        rhs: self.parse_f()?,
                                }))
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
                                        Ok(a)
                                } else {
                                        Err(())
                                }
                        }
                        _ => Err(()),
                }
        }
}
