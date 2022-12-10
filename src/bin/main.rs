//! Both the `Parser` and the `Compiler` may fail, in which case they would return
//! an error represented by `Result<T, &'static str>`, for easier error reporting.

use std::collections::HashMap;
use std::io::Write;
use std::iter::Peekable;
use std::ops::DerefMut;
use std::str::Chars;

use inkwell::context::Context;
use inkwell::passes::PassManager;
use inkwell::OptimizationLevel;

use crate::Token::*;
use kaleidoscope::cli;
use kaleidoscope::compiler::Compiler;
use kaleidoscope::types::{Expr, Function, Prototype};

const ANONYMOUS_FUNCTION_NAME: &str = "anonymous";
// OBJECTIVE: add global variables support

// ======================================================================================
// LEXER ================================================================================
// ======================================================================================

/// Represents a primitive syntax token.
#[derive(Debug, Clone)]
pub enum Token {
    Binary,
    Comma,
    Semicolon,
    Comment,
    Def,
    Else,
    EOF,
    Extern,
    For,
    Ident(String),
    If,
    In,
    LParen,
    Number(f64),
    Op(char),
    RParen,
    Then,
    Unary,
    Var,
}

/// Defines an error encountered by the `Lexer`.
pub struct LexError {
    pub error: &'static str,
    pub index: usize,
}

impl LexError {
    pub fn new(msg: &'static str) -> LexError {
        LexError {
            error: msg,
            index: 0,
        }
    }

    pub fn with_index(msg: &'static str, index: usize) -> LexError {
        LexError { error: msg, index }
    }
}

/// Defines the result of a lexing operation; namely a
/// `Token` on success, or a `LexError` on failure.
pub type LexResult = Result<Token, LexError>;

/// Defines a lexer which transforms an input `String` into
/// a `Token` stream.
pub struct Lexer<'a> {
    input: &'a str,
    chars: Box<Peekable<Chars<'a>>>,
    pos: usize,
}

impl<'a> Lexer<'a> {
    /// Creates a new `Lexer`, given its source `input`.
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            input,
            chars: Box::new(input.chars().peekable()),
            pos: 0,
        }
    }

    /// Lexes and returns the next `Token` from the source code.
    pub fn lex(&mut self) -> LexResult {
        let chars = self.chars.deref_mut();
        let src = self.input;

        let mut pos = self.pos;

        // Skip whitespaces
        loop {
            // Note: the following lines are in their own scope to
            // limit how long 'chars' is borrowed, and in order to allow
            // it to be borrowed again in the loop by 'chars.next()'.
            {
                let ch = chars.peek();

                if ch.is_none() {
                    self.pos = pos;

                    return Ok(Token::EOF);
                }

                if !ch.unwrap().is_whitespace() {
                    break;
                }
            }

            chars.next();
            pos += 1;
        }

        let start = pos;
        let next = chars.next();

        if next.is_none() {
            return Ok(Token::EOF);
        }

        pos += 1;

        // Actually get the next token.
        let result = match next.unwrap() {
            '(' => Ok(Token::LParen),
            ')' => Ok(Token::RParen),
            ',' => Ok(Token::Comma),
            ';' => Ok(Token::Semicolon),

            '#' => {
                // Comment
                loop {
                    let ch = chars.next();
                    pos += 1;

                    if ch == Some('\n') {
                        break;
                    }
                }

                Ok(Token::Comment)
            }

            '.' | '0'..='9' => {
                // Parse number literal
                loop {
                    let ch = match chars.peek() {
                        Some(ch) => *ch,
                        None => return Ok(Token::EOF),
                    };

                    // Parse float.
                    if ch != '.' && !ch.is_ascii_hexdigit() {
                        break;
                    }

                    chars.next();
                    pos += 1;
                }

                Ok(Token::Number(src[start..pos].parse().unwrap()))
            }

            'a'..='z' | 'A'..='Z' | '_' => {
                // Parse identifier
                loop {
                    let ch = match chars.peek() {
                        Some(ch) => *ch,
                        None => return Ok(Token::EOF),
                    };

                    // A word-like identifier only contains underscores and alphanumeric characters.
                    if ch != '_' && !ch.is_alphanumeric() {
                        break;
                    }

                    chars.next();
                    pos += 1;
                }

                match &src[start..pos] {
                    "def" => Ok(Token::Def),
                    "extern" => Ok(Token::Extern),
                    "if" => Ok(Token::If),
                    "then" => Ok(Token::Then),
                    "else" => Ok(Token::Else),
                    "for" => Ok(Token::For),
                    "in" => Ok(Token::In),
                    "unary" => Ok(Token::Unary),
                    "binary" => Ok(Token::Binary),
                    "var" => Ok(Token::Var),

                    ident => Ok(Token::Ident(ident.to_string())),
                }
            }

            op => {
                // Parse operator
                Ok(Token::Op(op))
            }
        };

        // Update stored position, and return
        self.pos = pos;

        result
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    /// Lexes the next `Token` and returns it.
    /// On EOF or failure, `None` will be returned.
    fn next(&mut self) -> Option<Self::Item> {
        match self.lex() {
            Ok(EOF) | Err(_) => None,
            Ok(token) => Some(token),
        }
    }
}

// ======================================================================================
// PARSER ===============================================================================
// ======================================================================================

/// Represents the `Expr` parser.
pub struct Parser<'a> {
    tokens: Vec<Token>,
    pos: usize,
    prec: &'a mut HashMap<char, i32>,
}

// I'm ignoring the 'must_use' lint in order to call 'self.advance' without checking
// the result when an EOF is acceptable.
#[allow(unused_must_use)]
impl<'a> Parser<'a> {
    /// Creates a new parser, given an input `str` and a `HashMap` binding
    /// an operator and its precedence in binary expressions.
    pub fn new(input: String, op_precedence: &'a mut HashMap<char, i32>) -> Self {
        let mut lexer = Lexer::new(input.as_str());
        let tokens = lexer.by_ref().collect();

        Parser {
            tokens,
            prec: op_precedence,
            pos: 0,
        }
    }

    /// Parses the content of the parser.
    pub fn parse(&mut self) -> Result<Function, &'static str> {
        let result = match self.current()? {
            Def => self.parse_def(),
            Extern => self.parse_extern(),
            _ => self.parse_toplevel_expr(),
        };

        match result {
            Ok(result) => {
                // is this check needed?
                // if !self.at_end() {
                //     Err("Unexpected token after parsed expression.")
                // } else {
                //     Ok(result)
                // }
                Ok(result)
            }

            err => err,
        }
    }

    /// Returns the current `Token`, without performing safety checks beforehand.
    fn curr(&self) -> Token {
        self.tokens[self.pos].clone()
    }

    /// Bound-unchecked curr method. This method is not a redundancy because sometimes panicking is
    /// useful for catching bugs.
    fn get_curr(&self) -> Option<Token> {
        self.tokens.get(self.pos).map(|t| t.clone())
    }

    /// Returns the current `Token`, or an error that
    /// indicates that the end of the file has been unexpectedly reached if it is the case.
    fn current(&self) -> Result<Token, &'static str> {
        if self.pos >= self.tokens.len() {
            Err("Unexpected end of file.")
        } else {
            Ok(self.tokens[self.pos].clone())
        }
    }

    /// Advances the position, and returns an empty `Result` whose error
    /// indicates that the end of the file has been unexpectedly reached.
    /// This allows to use the `self.advance()?;` syntax.
    fn advance(&mut self) -> Result<(), &'static str> {
        let npos = self.pos + 1;

        self.pos = npos;

        if npos < self.tokens.len() {
            Ok(())
        } else {
            Err("Unexpected end of file.")
        }
    }

    /// Returns a value indicating whether or not the `Parser`
    /// has reached the end of the input.
    fn at_end(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    /// Returns the precedence of the current `Token`, or 0 if it is not recognized as a binary operator.
    fn get_tok_precedence(&self) -> i32 {
        if let Ok(Op(op)) = self.current() {
            *self.prec.get(&op).unwrap_or(&100)
        } else {
            -1
        }
    }

    /// Parses the prototype of a function, whether external or user-defined.
    fn parse_prototype(&mut self) -> Result<Prototype, &'static str> {
        let (id, is_operator, precedence) = match self.get_curr() {
            Some(Ident(id)) => {
                self.advance()?;

                (id, false, 0)
            }

            Some(Binary) => {
                self.advance()?;

                let op = match self.curr() {
                    Op(ch) => ch,
                    _ => return Err("Expected operator in custom operator declaration."),
                };

                self.advance()?;

                let mut name = String::from("binary");

                name.push(op);

                let prec = if let Number(prec) = self.curr() {
                    self.advance()?;

                    prec as usize
                } else {
                    0
                };

                self.prec.insert(op, prec as i32);

                (name, true, prec)
            }

            Some(Unary) => {
                self.advance()?;

                let op = match self.curr() {
                    Op(ch) => ch,
                    _ => return Err("Expected operator in custom operator declaration."),
                };

                let mut name = String::from("unary");

                name.push(op);

                self.advance()?;

                (name, true, 0)
            }

            _ => return Err("Expected identifier in prototype declaration."),
        };

        match self.curr() {
            LParen => (),
            _ => return Err("Expected '(' character in prototype declaration."),
        }

        self.advance()?;

        if let RParen = self.curr() {
            self.advance();

            return Ok(Prototype {
                name: id,
                args: vec![],
                is_op: is_operator,
                prec: precedence,
            });
        }

        let mut args = vec![];

        loop {
            match self.curr() {
                Ident(name) => args.push(name),
                _ => return Err("Expected identifier in parameter declaration."),
            }

            self.advance()?;

            match self.curr() {
                RParen => {
                    self.advance();
                    break;
                }
                Comma => {
                    self.advance();
                }
                _ => return Err("Expected ',' or ')' character in prototype declaration."),
            }
        }

        Ok(Prototype {
            name: id,
            args,
            is_op: is_operator,
            prec: precedence,
        })
    }

    /// Parses a user-defined function.
    fn parse_def(&mut self) -> Result<Function, &'static str> {
        // Eat 'def' keyword
        self.pos += 1;

        // Parse signature of function
        let proto = self.parse_prototype()?;

        // Parse body of function
        let body = self.parse_expr()?;

        // Return new function
        Ok(Function {
            prototype: proto,
            body: Some(body),
            is_anon: false,
        })
    }

    /// Parses an external function declaration.
    fn parse_extern(&mut self) -> Result<Function, &'static str> {
        // Eat 'extern' keyword
        self.pos += 1;

        // Parse signature of extern function
        let proto = self.parse_prototype()?;

        Ok(Function {
            prototype: proto,
            body: None,
            is_anon: false,
        })
    }

    /// Parses any expression.
    fn parse_expr(&mut self) -> Result<Expr, &'static str> {
        match self.parse_unary_expr() {
            Ok(left) => self.parse_binary_expr(0, left),
            err => err,
        }
    }

    /// Parses a literal number.
    fn parse_nb_expr(&mut self) -> Result<Expr, &'static str> {
        // Simply convert Token::Number to Expr::Number
        match self.curr() {
            Number(nb) => {
                self.advance();
                Ok(Expr::Number(nb))
            }
            _ => Err("Expected number literal."),
        }
    }

    /// Parses an expression enclosed in parenthesis.
    fn parse_paren_expr(&mut self) -> Result<Expr, &'static str> {
        match self.current()? {
            LParen => (),
            _ => return Err("Expected '(' character at start of parenthesized expression."),
        }

        self.advance()?;

        let expr = self.parse_expr()?;

        match self.current()? {
            RParen => (),
            _ => return Err("Expected ')' character at end of parenthesized expression."),
        }

        self.advance();

        Ok(expr)
    }

    /// Parses an expression that starts with an identifier (either a variable or a function call).
    fn parse_id_expr(&mut self) -> Result<Expr, &'static str> {
        let id = match self.curr() {
            Ident(id) => id,
            _ => return Err("Expected identifier."),
        };

        if self.advance().is_err() {
            return Ok(Expr::Variable(id));
        }

        match self.curr() {
            LParen => {
                self.advance()?;

                if let RParen = self.curr() {
                    return Ok(Expr::Call {
                        fn_name: id,
                        args: vec![],
                    });
                }

                let mut args = vec![];

                loop {
                    args.push(self.parse_expr()?);

                    match self.current()? {
                        Comma => (),
                        RParen => break,
                        _ => return Err("Expected ',' character in function call."),
                    }

                    self.advance()?;
                }

                self.advance();

                Ok(Expr::Call { fn_name: id, args })
            }

            _ => Ok(Expr::Variable(id)),
        }
    }

    /// Parses an unary expression.
    fn parse_unary_expr(&mut self) -> Result<Expr, &'static str> {
        let op = match self.current()? {
            Op(ch) => {
                self.advance()?;
                ch
            }
            _ => return self.parse_primary(),
        };

        let mut name = String::from("unary");

        name.push(op);

        Ok(Expr::Call {
            fn_name: name,
            args: vec![self.parse_unary_expr()?],
        })
    }

    /// Parses a binary expression, given its left-hand expression.
    fn parse_binary_expr(&mut self, prec: i32, mut left: Expr) -> Result<Expr, &'static str> {
        loop {
            let curr_prec = self.get_tok_precedence();

            if curr_prec < prec || self.at_end() {
                return Ok(left);
            }

            let op = match self.curr() {
                Op(op) => op,
                _ => return Err("Invalid operator."),
            };

            self.advance()?;

            let mut right = self.parse_unary_expr()?;

            let next_prec = self.get_tok_precedence();

            if curr_prec < next_prec {
                right = self.parse_binary_expr(curr_prec + 1, right)?;
            }

            left = Expr::Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
    }

    /// Parses a conditional if..then..else expression.
    fn parse_conditional_expr(&mut self) -> Result<Expr, &'static str> {
        // eat 'if' token
        self.advance()?;

        let cond = self.parse_expr()?;

        // eat 'then' token
        match self.current() {
            Ok(Then) => self.advance()?,
            _ => return Err("Expected 'then' keyword."),
        }

        let then = self.parse_expr()?;

        // eat 'else' token
        match self.current() {
            Ok(Else) => self.advance()?,
            _ => return Err("Expected 'else' keyword."),
        }

        let otherwise = self.parse_expr()?;

        Ok(Expr::Conditional {
            cond: Box::new(cond),
            consequence: Box::new(then),
            alternative: Box::new(otherwise),
        })
    }

    /// Parses a loop for..in.. expression.
    fn parse_for_expr(&mut self) -> Result<Expr, &'static str> {
        // eat 'for' token
        self.advance()?;

        let name = match self.curr() {
            Ident(n) => n,
            _ => return Err("Expected identifier in for loop."),
        };

        // eat identifier
        self.advance()?;

        // eat '=' token
        match self.curr() {
            Op('=') => self.advance()?,
            _ => return Err("Expected '=' character in for loop."),
        }

        let start = self.parse_expr()?;

        // eat ',' token
        match self.current()? {
            Comma => self.advance()?,
            _ => return Err("Expected ',' character in for loop."),
        }

        let end = self.parse_expr()?;

        // parse (optional) step expression
        let step = match self.current()? {
            Comma => {
                self.advance()?;

                Some(self.parse_expr()?)
            }

            _ => None,
        };

        // eat 'in' token
        match self.current()? {
            In => self.advance()?,
            _ => return Err("Expected 'in' keyword in for loop."),
        }

        let body = self.parse_expr()?;

        Ok(Expr::For {
            var_name: name,
            start: Box::new(start),
            end: Box::new(end),
            step: step.map(Box::new),
            body: Box::new(body),
        })
    }

    /// Parses a var..in expression.
    fn parse_var_expr(&mut self) -> Result<Expr, &'static str> {
        // eat 'var' token
        self.advance()?;

        let mut variables = Vec::new();

        // parse variables
        loop {
            let name = match self.curr() {
                Ident(name) => name,
                _ => return Err("Expected identifier in 'var..in' declaration."),
            };

            self.advance()?;

            // read (optional) initializer
            let initializer = match self.curr() {
                Op('=') => Some({
                    self.advance()?;
                    self.parse_expr()?
                }),

                _ => None,
            };

            variables.push((name, initializer));

            match self.curr() {
                // some syntactical options here: 1 binding per statement with terminating semicolon; multiple variables.
                // going with the first option here for now
                Semicolon => {
                    let binding = variables.first().expect("variables not empty");
                    match binding {
                        (_, Some(binding_expr)) => return Ok(binding_expr.clone()),
                        _ => return Err("Expected expression in variable binding declaration."),
                    }
                }
                Comma => {
                    self.advance()?;
                }
                In => {
                    self.advance()?;
                    break;
                }
                _ => break,
            }
        }

        // parse body
        let body = self.parse_expr()?;

        Ok(Expr::VarIn {
            variables,
            body: Box::new(body),
        })
    }

    /// Parses a primary expression (an identifier, a number or a parenthesized expression).
    fn parse_primary(&mut self) -> Result<Expr, &'static str> {
        match self.curr() {
            Ident(_) => self.parse_id_expr(),
            Number(_) => self.parse_nb_expr(),
            LParen => self.parse_paren_expr(),
            If => self.parse_conditional_expr(),
            For => self.parse_for_expr(),
            Var => self.parse_var_expr(),
            _ => Err("Unknown expression."),
        }
    }

    /// Parses a top-level expression and makes an anonymous function out of it,
    /// for easier compilation.
    fn parse_toplevel_expr(&mut self) -> Result<Function, &'static str> {
        match self.parse_expr() {
            Ok(expr) => Ok(Function {
                prototype: Prototype {
                    name: ANONYMOUS_FUNCTION_NAME.to_string(),
                    args: vec![],
                    is_op: false,
                    prec: 0,
                },
                body: Some(expr),
                is_anon: true,
            }),

            Err(err) => Err(err),
        }
    }
}

// ======================================================================================
// PROGRAM ==============================================================================
// ======================================================================================

// macro used to print & flush without printing a new line
macro_rules! print_flush {
    ( $( $x:expr ),* ) => {
        print!( $($x, )* );

        std::io::stdout().flush().expect("Could not flush to standard output.");
    };
}

#[no_mangle]
pub extern "C" fn putchard(x: f64) -> f64 {
    print_flush!("{}", x as u8 as char);
    x
}

#[no_mangle]
pub extern "C" fn printd(x: f64) -> f64 {
    println!("{}", x);
    x
}

// Adding the functions above to a global array,
// so Rust compiler won't remove them.
#[used]
static EXTERNAL_FNS: [extern "C" fn(f64) -> f64; 2] = [putchard, printd];

/// Entry point of the program; acts as a REPL.
pub fn main() {
    let cli::Args {
        lexer: display_lexer_output,
        parser: display_parser_output,
        compiler: display_compiler_output,
    } = cli::opts();

    let context = Context::create();
    let module = context.create_module("repl");
    let builder = context.create_builder();

    // Create FPM
    let fpm = PassManager::create(&module);

    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.add_gvn_pass();
    fpm.add_cfg_simplification_pass();
    fpm.add_basic_alias_analysis_pass();
    fpm.add_promote_memory_to_register_pass();
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();

    fpm.initialize();

    let mut previous_exprs = Vec::new();

    loop {
        println!();
        print_flush!("?> ");

        let input = std::fs::read_to_string("programs/draft.kal").unwrap_or_default();

        if input.starts_with("exit") || input.starts_with("quit") {
            break;
        } else if input.chars().all(char::is_whitespace) {
            continue;
        }

        // Build precedence map
        let mut prec = HashMap::with_capacity(6);

        prec.insert('=', 2);
        prec.insert('<', 10);
        prec.insert('+', 20);
        prec.insert('-', 20);
        prec.insert('*', 40);
        prec.insert('/', 40);

        // Parse and (optionally) display input
        if display_lexer_output {
            println!(
                "-> Attempting to parse lexed input: \n{:?}\n",
                Lexer::new(input.as_str()).collect::<Vec<Token>>()
            );
        }

        // make module
        let module = context.create_module("tmp");

        // recompile every previously parsed function into the new module
        for prev in &previous_exprs {
            Compiler::compile(&context, &builder, &fpm, &module, prev)
                .expect("Cannot re-add previously compiled function.");
        }

        let (name, is_anonymous) = match Parser::new(input, &mut prec).parse() {
            Ok(fun) => {
                let is_anon = fun.is_anon;

                if display_parser_output {
                    if is_anon {
                        println!("-> Expression parsed: \n{:?}\n", fun.body);
                    } else {
                        println!("-> Function parsed: \n{:?}\n", fun);
                    }
                }

                match Compiler::compile(&context, &builder, &fpm, &module, &fun) {
                    Ok(function) => {
                        if display_compiler_output {
                            print_flush!("-> Expression compiled to IR:\n");
                            function.print_to_stderr();

                            std::fs::File::create("out.ll")
                                .expect("fs doesn't fail touching")
                                // inkwell has the thing we need as private llvm_value field, what now?
                                .write_all(format!("{:#?}", function).as_bytes())
                                .expect("fs doesn't fail writing");
                        }

                        if !is_anon {
                            // only add it now to ensure it is correct
                            previous_exprs.push(fun);
                        }

                        (function.get_name().to_str().unwrap().to_string(), is_anon)
                    }
                    Err(err) => {
                        println!("!> Error compiling function: {}", err);
                        continue;
                    }
                }
            }
            Err(err) => {
                println!("!> Error parsing expression: {}", err);
                continue;
            }
        };

        if is_anonymous {
            let ee = module
                .create_jit_execution_engine(OptimizationLevel::None)
                .unwrap();

            let maybe_fn =
                unsafe { ee.get_function::<unsafe extern "C" fn() -> f64>(name.as_str()) };
            let compiled_fn = match maybe_fn {
                Ok(f) => f,
                Err(err) => {
                    println!("!> Error during execution: {:?}", err);
                    continue;
                }
            };

            unsafe {
                println!("=> {}", compiled_fn.call());
            }
        }
        break; // breaking here to loop once
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    // reusable parser and compiler?
    fn init_compiler(input: String) {
        let display_lexer_output = true;
        let display_parser_output = true;
        let display_compiler_output = true;

        let context = Context::create();
        let module = context.create_module("repl");
        let builder = context.create_builder();

        // Create FPM
        let fpm = PassManager::create(&module);

        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.add_gvn_pass();
        fpm.add_cfg_simplification_pass();
        fpm.add_basic_alias_analysis_pass();
        fpm.add_promote_memory_to_register_pass();
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();

        fpm.initialize();

        let mut previous_exprs = Vec::new();

        println!();
        print_flush!("?> ");

        // make module
        let module = context.create_module("tmp");

        // recompile every previously parsed function into the new module
        for prev in &previous_exprs {
            Compiler::compile(&context, &builder, &fpm, &module, prev)
                .expect("Cannot re-add previously compiled function.");
        }

        // Build precedence map
        let mut prec = HashMap::with_capacity(6);

        prec.insert('=', 2);
        prec.insert('<', 10);
        prec.insert('+', 20);
        prec.insert('-', 20);
        prec.insert('*', 40);
        prec.insert('/', 40);

        // if input.chars().all(char::is_whitespace) {
        //     continue;
        // }

        // Parse and (optionally) display input
        if display_lexer_output {
            println!(
                "-> Attempting to parse lexed input: \n{:?}\n",
                Lexer::new(input.as_str()).collect::<Vec<Token>>()
            );
        }

        let (name, is_anonymous) = match Parser::new(input, &mut prec).parse() {
            Ok(fun) => {
                let is_anon = fun.is_anon;

                if display_parser_output {
                    if is_anon {
                        println!("-> Expression parsed: \n{:?}\n", fun.body);
                    } else {
                        println!("-> Function parsed: \n{:?}\n", fun);
                    }
                }

                match Compiler::compile(&context, &builder, &fpm, &module, &fun) {
                    Ok(function) => {
                        if display_compiler_output {
                            print_flush!("-> Expression compiled to IR:\n");
                            function.print_to_stderr();
                        }

                        if !is_anon {
                            // only add it now to ensure it is correct
                            previous_exprs.push(fun);
                        }

                        (function.get_name().to_str().unwrap().to_string(), is_anon)
                    }
                    Err(err) => {
                        panic!("!> Error compiling function: {}", err);
                        // continue;
                    }
                }
            }
            Err(err) => {
                panic!("!> Error parsing expression: {}", err);
                // continue;
            }
        };

        if is_anonymous {
            let ee = module
                .create_jit_execution_engine(OptimizationLevel::None)
                .unwrap();

            let maybe_fn =
                unsafe { ee.get_function::<unsafe extern "C" fn() -> f64>(name.as_str()) };
            let compiled_fn = match maybe_fn {
                Ok(f) => f,
                Err(err) => {
                    panic!("!> Error during execution: {:?}", err);
                    // continue;
                }
            };

            unsafe {
                println!("=> {}", compiled_fn.call());
            }
        }
    }
    #[test]
    fn smoke() {
        // let programs = os::ls("programs/*.kal");?
        let programs = vec!["programs/binding.kal", "programs/varin.kal"];
        for program in programs {
            let input = std::fs::read_to_string(program).unwrap_or_default();
            init_compiler(input);
        }
    }
}
