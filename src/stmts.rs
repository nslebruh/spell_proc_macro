use proc_macro::TokenStream;
use quote::{ToTokens, quote};
use syn::{parse::{Parse, ParseStream}, Ident, Type, Token, token::{Comma, Fn, Semi, Return as SynReturn, Let}, LitStr, Lit};

use crate::{custom_tokens::{AssignmentToken, FullStop, that, bestows, conjure, which, requires, nothing, thusly, grants, conclude, containing, should, then, the, essence, of, proclaimeth, Return, bestow, concatenated, with, to, and}, expr::Expression, the_magic_of, find_type, comma_or_and};
use syn::Result;

#[derive(Clone)]
pub enum Statement {
  FunctionAssignment(Function),
  Assignment(Assignment),
  Expression((Expression, FullStop)),
  Proclaimeth(Proclaimeth),
  Return(ReturnStatement)
  //If(If)
}

impl Parse for Statement {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if {
            let fork = input.fork();
            fork.parse::<AssignmentToken>().is_ok()
          } 
        {
          return Ok(Statement::Assignment(input.parse()?))
        } else if {
          let fork = input.fork();
          fork.parse::<Expression>().is_ok() && fork.parse::<FullStop>().is_ok()
        } {
          return Ok(Statement::Expression((input.parse()?, input.parse()?)))
        } else if input.peek(conjure) {
          return Ok(Statement::FunctionAssignment(input.parse()?))
        } else if input.peek(bestow) {
          return Ok(Statement::Return(input.parse()?))
        } else if input.peek(proclaimeth) {
          return Ok(Statement::Proclaimeth(input.parse()?))
        } else {
          return Err(input.error("Not a valid statement"))
        }
        
    }
}

impl ToTokens for Statement {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Statement::FunctionAssignment(x) => tokens.extend(quote!{#x}),
            Statement::Assignment(x) => tokens.extend(quote!{#x}),
            Statement::Expression((x, y)) => {
              let semi: Semi = (*y).into();
              tokens.extend(quote!{#x #semi})
            },
            Statement::Proclaimeth(x) => {
              tokens.extend(quote!{#x})
            },
            Statement::Return(x) => tokens.extend(quote!{#x}),
        }
    }
}

#[derive(Clone)]
pub struct Proclaimeth {
  proclaimeth: proclaimeth,
  value: LitStr,
  args: Vec<Box<Expression>>
}

impl Parse for Proclaimeth {
    fn parse(input: ParseStream) -> Result<Self> {
        let proclaimeth = input.parse::<proclaimeth>()?;
        input.parse::<Option<Token![,]>>()?;
        let value = input.parse::<LitStr>()?;
        let mut args: Vec<Box<Expression>> = Vec::new();
        if input.peek(Token![,]) {
          while !input.peek(Token![.]) {
            if input.peek(Token![,]) {
              input.parse::<Token![,]>()?;
            } else if input.peek(Token![;]) {
              input.parse::<Token![;]>()?;
            }
            args.push(Box::new(input.parse()?))
          }
          input.parse::<Token![.]>()?;
          return Ok(Self {
            proclaimeth,
            value,
            args
          })
        } else if input.peek(Token![.]) {
          input.parse::<Token![.]>()?;
          return Ok(Self {
            proclaimeth,
            value,
            args
          })
        } else {
          return Err(input.error("expected arguments or ."))
        }
    }
}

impl ToTokens for Proclaimeth {
  fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
      let value = &self.value;
      let args = &self.args;
      tokens.extend(quote!{println!(#value, #(#args),*);})
  }
}

#[derive(Clone)]
pub struct Function {
    conjure: conjure,
    ident: Ident,
    _first_comma: Token![,],
    args: Option<Vec<(Ident, Type)>>,
    rtrn: Option<(Type, that, bestows)>,
    _containing: containing,
    stmts: Vec<Box<Statement>>
}
impl Parse for Function {
    fn parse(input: ParseStream) -> Result<Self> { 
        let mut args;
        let mut stmts: Vec<Box<Statement>> = Vec::new();
        let mut rtrn: Option<(Type, that, bestows)> = None;
        let conjure = input.parse::<conjure>()?;
        the_magic_of(input)?;
        let ident = input.parse::<Ident>()?;
        let first_comma = input.parse::<Token![,]>()?;
        input.parse::<which>()?;
        input.parse::<requires>()?;
        if input.peek(nothing) {
            input.parse::<nothing>()?;
            comma_or_and(input)?;
            args = None;
        } else {
            args = Some(Vec::new());
            while !input.peek(thusly) && !input.peek(that) && !input.peek2(grants) {
                let ty: Type;
                ty = find_type(input)?;
                input.parse::<Option<Token![as]>>()?;
                let arg_ident = input.parse::<Ident>()?;
                comma_or_and(input)?;
                args.as_mut().unwrap().push((arg_ident, ty));
            }
        
        }
        if input.peek(that) && input.peek2(bestows) {
            let that = input.parse::<that>()?;
            let bestows = input.parse::<bestows>()?;
            rtrn = Some((find_type(input)?, that, bestows));
            input.parse::<Token![,]>()?;
        }
        input.parse::<thusly>()?;
        let containing = input.parse::<containing>()?;
        input.parse::<Token![,]>()?;
        while !input.peek(conclude) {
            let stmt = input.parse::<Statement>()?;
            stmts.push(Box::new(stmt));
        }
        input.parse::<conclude>()?;
        input.parse::<Token![.]>()?;
        Ok(Self {
            conjure,
            ident,
            _first_comma: first_comma,
            args,
            stmts,
            rtrn,
            _containing: containing
        })
    }
}

impl ToTokens for Function {
  fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
      let conjure: Fn = self.conjure.clone().into();
      let ident = self.ident.clone();
      let stmts = self.stmts.clone();
      let retrn;
      if let Some(rtrn) =  self.rtrn.clone() {
          let x = rtrn.0;
          retrn = quote!{ -> #x}
      } else {
          retrn = quote!{ }
      }
      if self.args.is_some() {
          let args2 = self.args.clone().unwrap();
          let mut args_values: Vec<Type> = Vec::new();
          let mut args_keys: Vec<Ident> = Vec::new();
          for (key, value) in args2.into_iter() {
              args_values.push(value);
              args_keys.push(key);
          }
          tokens.extend(quote!{#conjure #ident(#(#args_keys: #args_values),*)#retrn {#(#stmts)*}});
          //println!("{}", tokens.to_string());
      } else {
          tokens.extend(quote!{#conjure #ident()#retrn {#(#stmts)*}});
          //println!("{}", tokens.to_string());
      }
  }
}

#[derive(Clone)]
pub struct Assignment {
  assignment_token: AssignmentToken,
  ident: Ident,
  ty: Option<Type>,
  expr: Expression,
  dot: FullStop
}

impl Parse for Assignment {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
      let assignment_token = input.parse::<AssignmentToken>()?;
      the_magic_of(input)?;
      the_essence_of(input)?;
      let ident = input.parse::<Ident>()?;
      input.parse::<Option<Token![as]>>()?;
      let expr = input.parse::<Expression>()?;
      let dot = input.parse::<FullStop>()?;
      let ty = match expr.clone() {
        Expression::FunctionCall(_) => None,
        Expression::Operation(_) => None,
        Expression::LitOrIdent(x) => match x {
            crate::expr::LitOrIdent::Lit(y) => match y {
              Lit::Str(_) => Some(syn::parse_str::<Type>("&'static str")?),
                Lit::ByteStr(str) => Some(syn::parse_str::<Type>(format!("&[u8; {}]", str.value().len()).as_str())?),
                Lit::Byte(_) => Some(syn::parse_str::<Type>("u8")?),
                Lit::Char(_) => Some(syn::parse_str::<Type>("char")?),
                Lit::Int(_) => Some(syn::parse_str::<Type>("i64")?),
                Lit::Float(_) => Some(syn::parse_str::<Type>("f64")?),
                Lit::Bool(_) => Some(syn::parse_str::<Type>("bool")?),
                Lit::Verbatim(_) => panic!("bruh"),
            },
            crate::expr::LitOrIdent::Ident(_) => None,
        },
        Expression::UnaryOperation(_) => None,
      };
      Ok(Assignment {
        assignment_token,
        ident,
        ty,
        expr,
        dot
      })
    }
}

impl ToTokens for Assignment {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
      let assignment_token: Let = self.assignment_token.let_;
      let ident: Ident = self.ident.clone();
      let expr: Expression = self.expr.clone();
      let dot: Semi = self.dot.clone().into();
      match expr.clone() {
        Expression::FunctionCall(_) => {
          if let Some(mutability) = self.assignment_token.mutability {
            tokens.extend(quote!{#assignment_token #mutability #ident = #expr #dot})
          } else {
            tokens.extend(quote!{#assignment_token #ident = #expr #dot})
          }
        },
        Expression::Operation(_) => {
          if let Some(mutability) = self.assignment_token.mutability {
            tokens.extend(quote!{#assignment_token #mutability #ident = #expr #dot})
          } else {
            tokens.extend(quote!{#assignment_token #ident = #expr #dot})
          }
        }
        Expression::LitOrIdent(x) => {
          match x {
            crate::expr::LitOrIdent::Lit(_) => {
              let ty = self.ty.clone().unwrap();
              if let Some(mutability) = self.assignment_token.mutability {
                tokens.extend(quote!{#assignment_token #mutability #ident: #ty = #expr #dot})
              } else {
                tokens.extend(quote!{#assignment_token #ident: #ty = #expr #dot})
              }
            },
            crate::expr::LitOrIdent::Ident(_) => {
              if let Some(mutability) = self.assignment_token.mutability {
                tokens.extend(quote!{#assignment_token #mutability #ident = #expr #dot})
              } else {
                tokens.extend(quote!{#assignment_token #ident = #expr #dot})
              }
            },
          }
        },
        Expression::UnaryOperation(_) => {
          if let Some(mutability) = self.assignment_token.mutability {
            tokens.extend(quote!{#assignment_token #mutability #ident = #expr #dot})
          } else {
            tokens.extend(quote!{#assignment_token #ident = #expr #dot})
          }
        },
      }
    }
}

#[derive(Clone)]
pub struct If {
  should: should,
  expr: Expression,
  _first_comma: Comma,
  then: Option<then>,
  stmts: Vec<Statement>,
  conclude: conclude
}

impl Parse for If {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        todo!()
    }
}

impl ToTokens for If {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        todo!()
    }
}

#[derive(Clone)]
pub struct ReturnStatement {
    b_return: Return,
    var: Option<Expression>
}
impl Parse for ReturnStatement {
    fn parse(input: ParseStream) -> Result<Self> {
      let b_return = input.parse::<Return>()?;
      let var = {
        if input.fork().parse::<Expression>().is_ok() {
          Some(input.parse::<Expression>()?)
        } else {
          None
        }
        
      };
      input.parse::<Option<Token![.]>>()?;
        Ok(Self {
            b_return,
            var
        })
        
    }
}
impl ToTokens for ReturnStatement {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let ret: SynReturn = self.b_return.clone().into();
        let var = self.var.clone();
        tokens.extend(quote!{#ret #var})
    }
}


fn the_essence_of(input: ParseStream) -> Result<()> {
  if input.peek(the) && input.peek(essence) && input.peek(of) {
    input.parse::<the>()?;
    input.parse::<essence>()?;
    input.parse::<of>()?;
  }
  Ok(())
}