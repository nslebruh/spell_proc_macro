use std::ops::{DerefMut, Deref};

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{ToTokens, quote};
use syn::{custom_keyword, parse::{Parse, ParseStream, Result}, token::{Return as SynReturn, Fn, Dot, Semi, Mut, Let}, Ident, Lit};

custom_keyword!(thy);
custom_keyword!(the);
custom_keyword!(thine);
custom_keyword!(to);
custom_keyword!(of);
custom_keyword!(with);
custom_keyword!(and);
custom_keyword!(a);
custom_keyword!(an);
custom_keyword!(that);
custom_keyword!(then);
custom_keyword!(takes);
custom_keyword!(essence);
custom_keyword!(proclaimeth);
custom_keyword!(concatenated);
custom_keyword!(should);
custom_keyword!(otherwise);
custom_keyword!(conjure);
custom_keyword!(magic);
custom_keyword!(which);
custom_keyword!(containing);
custom_keyword!(thusly);
custom_keyword!(requires);
custom_keyword!(nothing);
custom_keyword!(boolean);
custom_keyword!(integer);
custom_keyword!(float);
custom_keyword!(string);
custom_keyword!(conclude);
custom_keyword!(grants);
custom_keyword!(invoke);
custom_keyword!(bestow);
custom_keyword!(bestows);
custom_keyword!(invocation);
custom_keyword!(plus);
custom_keyword!(added);
custom_keyword!(minus);
custom_keyword!(opposite);
custom_keyword!(inverse);
custom_keyword!(divided);
custom_keyword!(by);
custom_keyword!(over);
custom_keyword!(times);
custom_keyword!(multiplied);
custom_keyword!(is);
custom_keyword!(equal);
custom_keyword!(equivalent);
custom_keyword!(be);

#[derive(Clone)]
pub struct Return(pub bestow);
impl Deref for Return {
    type Target = bestow;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for Return {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
impl Parse for Return {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self(input.parse()?))
    }
}
impl From<Return> for SynReturn {
    fn from(value: Return) -> Self {
        SynReturn { span: value.span }
    }
}

impl From<conjure> for Fn {
    fn from(value: conjure) -> Self {
        Fn {
            span: value.span
        }
    }
}
#[derive(Clone, Copy)]
pub struct FullStop(pub Dot);
impl Deref for FullStop {
    type Target = Dot;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for FullStop {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
impl Parse for FullStop {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self(input.parse()?))
    }
}
impl From<Semi> for FullStop {
    fn from(value: Semi) -> Self {
        FullStop(Dot { spans: value.spans })
    }
}

impl From<FullStop> for Semi {
    fn from(value: FullStop) -> Self {
        Semi { spans: value.spans }
    }
}

pub enum FunctionArg {
  Lit(Lit),
  Ident(Ident),
}

impl Parse for FunctionArg {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(Lit) {
          return Ok(FunctionArg::Lit(input.parse()?))
        } else if input.peek(Ident) {
          return Ok(FunctionArg::Ident(input.parse()?))
        } else {
          return Err(input.error("that is not a function arg ☝🤓"))
        }
    }
}

impl ToTokens for FunctionArg {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            FunctionArg::Lit(x) => tokens.extend(quote!{#x}),
            FunctionArg::Ident(x) => tokens.extend(quote!{#x}),
        }
    }
}

#[derive(Clone)]
pub struct AssignmentToken {
  pub mutability: Option<Mut>,
  pub let_: Let
}

impl Parse for AssignmentToken {
    fn parse(input: ParseStream) -> Result<Self> {
        let token = input.parse::<Ident>()?;
        match token.to_string().as_str() {
          "Imbue" | "imbue" => {
            return Ok(AssignmentToken { mutability: Some(Mut {span: token.span()}), let_: Let {span: token.span()} })
          },
          "Declare" | "declare" => {
            return Ok(AssignmentToken { mutability: None, let_: Let {span: token.span()} })
          },
          _ => {
            return Err(input.error("No assignment token 💀"))
          }
        }

    }
}
impl ToTokens for AssignmentToken {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
      let l = self.let_;
        if let Some(mutability) = self.mutability {
          tokens.extend(quote!{#l #mutability})
        } else {
          tokens.extend(quote!{#l})
        }
    }
}