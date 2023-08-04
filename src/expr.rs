use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{ToTokens, quote};
use syn::{parse::{Parse, ParseStream}, Ident, Token, Result, token::{Dot, Semi, Bang, Add, Sub, Div, Star, EqEq}, Lit, Type, Expr};

use crate::custom_tokens::{invoke, the, magic, of, which, takes, an, invocation, plus, added, to, minus, opposite, inverse, divided, by, over, times, multiplied, equal, equivalent, be};

#[derive(Clone)]
pub enum LitOrIdent {
  Lit(Lit),
  Ident(Ident)
}

impl Parse for LitOrIdent {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(Lit) {
          return Ok(LitOrIdent::Lit(input.parse()?))
        } else if input.peek(Ident) {
          return Ok(LitOrIdent::Ident(input.parse()?))
        } else {
          return Err(input.error("no literal or identifier 💀"))
        }
    }
}

impl ToTokens for LitOrIdent {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            LitOrIdent::Lit(x) => tokens.extend(quote!{#x}),
            LitOrIdent::Ident(x) => tokens.extend(quote!{#x}),
        }
    }
}

#[derive(Clone)]
pub enum Operator {
  Add(Token![+]),
  Minus(Token![-]),
  Divide(Token![/]),
  Multiply(Token![*]),
  Equals(Token![==]),
  NotEquals(Token![!=]),
  GreaterThan(Token![>]),
  LessThan(Token![<]),
  GreaterThanOrEqualTo(Token![>=]),
  LessThanOrEqualTo(Token![<=]),
  And(Token![&&]),
  Or(Token![||])
}

impl Parse for Operator {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(plus) { // Add
          return Ok(Operator::Add(Add {spans: [input.parse::<plus>()?.span]}))
        } else if input.peek(added) && input.peek2(to) { // Add
          return Ok(Operator::Add(Add {spans: [input.parse::<added>()?.span.join(input.parse::<to>()?.span).unwrap()]}))
        } else if input.peek(minus) { // Minus
          return Ok(Operator::Minus(Sub {spans: [input.parse::<minus>()?.span]}))
        } else if input.peek(divided) && input.peek2(by) { // Divide
          return Ok(Operator::Divide(Div {spans: [input.parse::<divided>()?.span.join(input.parse::<by>()?.span).unwrap()]}))
        } else if input.peek(over) {
          return Ok(Operator::Divide(Div {spans: [input.parse::<over>()?.span]}))
        } else if input.peek(times) {
          return Ok(Operator::Multiply(Star {spans: [input.parse::<times>()?.span]}))
        } else if input.peek(multiplied) && input.peek2(by) {
          return Ok(Operator::Multiply(Star {spans: [input.parse::<multiplied>()?.span.join(input.parse::<by>()?.span).unwrap()]}))
        } else if input.peek(be) && input.peek2(equal) && input.peek3(to) {
          return Ok(Operator::Equals(EqEq {spans: [input.parse::<be>()?.span.join(input.parse::<equal>()?.span).unwrap(), input.parse::<to>()?.span]}))
        } else if input.peek(be) && input.peek2(equivalent) && input.peek3(to) {
          return Ok(Operator::Equals(EqEq {spans: [input.parse::<be>()?.span.join(input.parse::<equivalent>()?.span).unwrap(), input.parse::<to>()?.span]}))
        } else {
          return Err(input.error("cannot find a valid operator"))
        }
    }
}

impl ToTokens for Operator {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Operator::Add(x) => tokens.extend(quote!{#x}),
            Operator::Minus(x) => tokens.extend(quote!{#x}),
            Operator::Divide(x) => tokens.extend(quote!{#x}),
            Operator::Multiply(x) => tokens.extend(quote!{#x}),
            Operator::Equals(x) => tokens.extend(quote!{#x}),
            Operator::NotEquals(x) => tokens.extend(quote!{#x}),
            Operator::GreaterThan(x) => tokens.extend(quote!{#x}),
            Operator::LessThan(x) => tokens.extend(quote!{#x}),
            Operator::GreaterThanOrEqualTo(x) => tokens.extend(quote!{#x}),
            Operator::LessThanOrEqualTo(x) => tokens.extend(quote!{#x}),
            Operator::And(x) => tokens.extend(quote!{#x}),
            Operator::Or(x) => tokens.extend(quote!{#x}),
        }
    }
}

#[derive(Clone)]
pub struct Operation {
  lhs: LitOrIdent,
  op: Operator,
  rhs: LitOrIdent
}

impl Parse for Operation {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Operation {
          lhs: input.parse()?,
          op: input.parse()?,
          rhs: input.parse()?
        })
    }
}

impl ToTokens for Operation {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let lhs = &self.lhs;
        let op = &self.op;
        let rhs = &self.rhs;
        tokens.extend(quote!{#lhs #op #rhs})
    }
}

#[derive(Clone)]
pub struct UnaryOperation {
  bang: Token![!],
  expr: LitOrIdent
}

impl Parse for UnaryOperation {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(the) && input.peek2(opposite) && input.peek3(of) {
          let span1 = input.parse::<the>()?;
          let span2 = input.parse::<opposite>()?;
          let span3 = input.parse::<of>()?;
          let span = span1.span.join(span2.span);
          span.unwrap().join(span3.span);
          return Ok(UnaryOperation { bang: Bang {spans: [span.unwrap()]}, expr: input.parse()? })
          
        } else if input.peek(the) && input.peek2(inverse) && input.peek3(of) {
          let span1 = input.parse::<the>()?;
          let span2 = input.parse::<inverse>()?;
          let span3 = input.parse::<of>()?;
          let span = span1.span.join(span2.span);
          span.unwrap().join(span3.span);
          return Ok(UnaryOperation { bang: Bang {spans: [span.unwrap()]}, expr: input.parse()? })
        } else {
          return Err(input.error("could not find unary operation"))
        }
        
    }
}

impl ToTokens for UnaryOperation {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let bang = &self.bang;
        let expr = &self.expr;
        tokens.extend(quote!{#bang #expr})
    }
}

#[derive(Clone)]
pub enum Expression {
  FunctionCall(FunctionCall),
  LitOrIdent(LitOrIdent),
  Operation(Operation),
  UnaryOperation(UnaryOperation)
}

impl Parse for Expression {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(invoke) {
          return Ok(Expression::FunctionCall(input.parse()?))
        } else if input.peek(Lit) || input.peek(Ident) {
          if {
            let fork = input.fork();
            fork.parse::<LitOrIdent>()?;
            fork.parse::<Operator>().is_ok()
          } {
            return Ok(Expression::Operation(input.parse()?))
          } else {
            return Ok(Expression::LitOrIdent(input.parse()?))
          }
        } else {
          return Err(input.error("how"))
        } 
        
    }
}

impl ToTokens for Expression {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
          Expression::FunctionCall(x) => tokens.extend(quote!{#x}),
          Expression::LitOrIdent(x) => tokens.extend(quote!{#x}),
          Expression::Operation(x) => tokens.extend(quote!{#x}),
          Expression::UnaryOperation(x) => tokens.extend(quote!{#x}),
        }
    }
}

#[derive(Clone)]
pub struct FunctionCall {
  ident: Ident,
  args: Vec<Box<Expression>>,
}

impl Parse for FunctionCall {
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
      an_invocation_of(input)?;
      the_magic_of(input)?;
      let ident = input.parse::<Ident>()?;
      let mut args: Vec<Box<Expression>> = Vec::new();
      if input.peek(Token![,]) && input.peek2(which) && input.peek3(takes) {
        input.parse::<Token![,]>()?;
        input.parse::<which>()?;
        input.parse::<takes>()?;
        args.push(Box::new(input.parse::<Expression>()?));
        while !input.peek(Token![.]) || !input.peek(Token![;]) {
          input.parse::<Token![,]>()?;
          args.push(Box::new(input.parse::<Expression>()?));
        }
      } else if input.peek(Token![,]) {
        while !input.peek(Token![.]) || !input.peek(Token![;]) {
          input.parse::<Token![,]>()?;
          args.push(Box::new(input.parse::<Expression>()?));
        }
      }
      Ok(FunctionCall {
        ident,
        args
      })
    }
}

impl ToTokens for FunctionCall {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
      let ident = self.ident.to_owned();
      let args: Vec<Expression> = self.args.to_owned().into_iter().map(|f| *f).collect();
        tokens.extend(quote!{#ident(#(#args),*)})
    }
}
fn the_magic_of(input: ParseStream) -> Result<()> {
  if input.peek(the) && input.peek2(magic) && input.peek3(of) {
    input.parse::<the>()?;
    input.parse::<magic>()?;
    input.parse::<of>()?;
  }
  return Ok(())
}

fn an_invocation_of(input: ParseStream) -> Result<()> {
  if input.peek(invoke) {
    input.parse::<invoke>()?;
  } else if input.peek(an) && input.peek2(invocation) && input.peek3(of) {
    input.parse::<an>()?;
    input.parse::<invocation>()?;
    input.parse::<of>()?;
  }
  return Ok(())
}