extern crate proc_macro;
mod stmts;
mod expr;
mod custom_tokens;

use custom_tokens::{with, the, essence, of, to, proclaimeth, concatenated, and, should, conjure, magic, which, requires, thusly, nothing, a, integer, an, float, boolean, string, containing, conclude, that, grants, invoke, takes, bestow, bestows, thy, thine, Return, FullStop};
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, ToTokens, TokenStreamExt};
use syn::parse::{Parse, ParseStream, Result};
use syn::token::{Let, Dot, Semi, Mut, Comma, Return as SynReturn, Fn};
use syn::{parse_macro_input, Ident, Token, Type, LitStr, Lit};


#[derive(Clone, Copy)]
struct AssignmentToken {
    span: Span,
    mutability: Option<Mut>
}
impl Parse for AssignmentToken {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident = input.parse::<Ident>()?;
        let string = ident.to_string();
        let mutability: Option<Mut>;
        match string.as_str() {
            "Declare" | "declare" => {
                mutability = None;
            },
            "Imbue" | "imbue" => {
                mutability = Some(Mut {span: ident.span()});
            },
            _ => return Err(syn::Error::new(ident.span(), "Cannot find assignment token"))
        }
        if input.peek(the) && input.peek2(essence) && input.peek3(of){
            input.parse::<the>()?;
            input.parse::<essence>()?;
            input.parse::<of>()?;
        } else if input.peek(thy) {
            input.parse::<thy>()?;
        } else if input.peek(the) {
            input.parse::<the>()?;
        } else if input.peek(thine) {
            input.parse::<thine>()?;
        }
        return Ok(AssignmentToken {span: ident.span(), mutability})
    }
}
impl From<AssignmentToken> for Let {
    fn from(value: AssignmentToken) -> Self {
        Let {
            span: value.span
        }
    }
}

#[derive(Clone)]
enum Value {
    LitValue(LitValue),
    FunctionCall(FunctionCall)
}
impl Parse for Value {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(invoke) {
            return Ok(Value::FunctionCall(input.parse()?))
        } else {
            return Ok(Value::LitValue(input.parse()?))
        }
    }
}
impl ToTokens for Value {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Value::LitValue(x) => tokens.extend(quote!{#x}),
            Value::FunctionCall(x) => tokens.extend(quote!{#x}),
        }
    }
}

#[derive(Clone)]
struct LitValue {
    lit: Lit,
    ty: Type
}

impl Parse for LitValue {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(to) {
            input.parse::<to>()?;
        } else if input.peek(with) {
            input.parse::<with>()?;
            if input.peek(the) && input.peek2(essence) && input.peek3(of){
                input.parse::<the>()?;
                input.parse::<essence>()?;
                input.parse::<of>()?;
            }
        } else if input.peek(Token![as]) {
            input.parse::<Token![as]>()?;
        }
        if input.peek(Lit) {
            let lit = input.parse::<Lit>()?;
            let ty = match lit.clone() {
                Lit::Str(_) => syn::parse_str::<Type>("&'static str")?,
                Lit::ByteStr(str) => syn::parse_str::<Type>(format!("&[u8; {}]", str.value().len()).as_str())?,
                Lit::Byte(_) => syn::parse_str::<Type>("u8")?,
                Lit::Char(_) => syn::parse_str::<Type>("char")?,
                Lit::Int(_) => syn::parse_str::<Type>("i64")?,
                Lit::Float(_) => syn::parse_str::<Type>("f64")?,
                Lit::Bool(_) => syn::parse_str::<Type>("bool")?,
                Lit::Verbatim(_) => panic!("bruh"),
            };
            return Ok(Self {
                lit,
                ty
            });
        } else {
            return Err(input.error("not a valid value"))
        }   
    }
}

impl ToTokens for LitValue {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let value = self.lit.clone();
        tokens.extend(quote!{#value})
    }
}


#[derive(Clone)]
struct Assignment {
    assignment_token: AssignmentToken,
    ident: Ident,
    value: Value,
    dot: FullStop
    
}
impl Parse for Assignment {
    fn parse(input: ParseStream) -> Result<Self> {
        let assignment_token = input.parse()?;
        let ident = input.parse()?;
        input.parse::<Option<Token![,]>>()?;
        if input.peek(Token![as]) {
            input.parse::<Token![as]>()?;
        }
        let value  = input.parse::<Value>()?;
        let dot = input.parse::<FullStop>()?;
        Ok(
            Self {
                assignment_token,
                ident,
                value,
                dot
            }
        )
    }
}
impl ToTokens for Assignment {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let assignment_token: Let = self.assignment_token.clone().into();
        let ident: Ident = self.ident.clone();
        let value: Value = self.value.clone();
        let dot: Semi = self.dot.clone().into();
        match value {
            Value::LitValue(x) => {
                let ty = x.ty;
                let lit = x.lit;
                if let Some(mutability) = self.assignment_token.mutability {
                    tokens.extend(quote!{#assignment_token #mutability #ident: #ty = #lit #dot})
                } else {
                    tokens.extend(quote!{#assignment_token #ident: #ty = #lit #dot})
                }
            },
            Value::FunctionCall(x) => {
                if let Some(mutability) = self.assignment_token.mutability {
                    tokens.extend(quote!{#assignment_token #mutability #ident = #x #dot})
                } else {
                    tokens.extend(quote!{#assignment_token #ident = #x #dot})
                }
            },
        }
        

        //let local: Local = self.into();
        //tokens.extend(quote!{#local});
    }
}

#[derive(Clone)]
enum FunctionArgs {
    Lit(Lit),
    Ident(Ident),
    FunctionCall(FunctionCall)
}
impl Parse for FunctionArgs {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(Ident) {
            return Ok(FunctionArgs::Ident(input.parse()?))
        } else if input.peek(Lit) {
            return Ok(FunctionArgs::Lit(input.parse()?))
        } else if input.peek(invoke) {
            return Ok(FunctionArgs::FunctionCall(input.parse()?))
        } else {
            return Err(input.error("No proclaimeth args"))
        }
    }
}
impl ToTokens for FunctionArgs {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            FunctionArgs::Ident(x) => tokens.extend(quote!{#x}),
            FunctionArgs::Lit(x) => tokens.extend(quote!{#x}),
            FunctionArgs::FunctionCall(x) => tokens.extend(quote!{#x})
        }
    }
}

#[derive(Clone)]
struct Proclaimeth {
    value: LitStr,
    _first_comma: Comma,
    args: Vec<FunctionArgs>,
}
impl Parse for Proclaimeth {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<proclaimeth>()?;
        let first_comma = input.parse::<Token![,]>()?;
        let value = input.parse::<LitStr>()?;
        let mut args = Vec::new();
        let mut iterated = 0;
        while !input.peek(Token![.]) {
            if iterated == 0 {
                iterated += 1;
                input.parse::<Token![,]>()?;
                if input.peek(concatenated) {
                    input.parse::<concatenated>()?;
                } else {
                    return Err(input.error("no concatenated"))
                }
                if input.peek(with) {
                    input.parse::<with>()?;
                } else if input.peek(to) {
                    input.parse::<to>()?;
                }
            }
            if input.peek(Token![,]) {
                input.parse::<Token![,]>()?;
            } else if input.peek(and) {
                input.parse::<and>()?;
            }
            if input.peek(Lit) || input.peek(Ident) {
                args.push(input.parse()?)
            } else {
                return Err(input.error("comma but nothing after bozo"))
            }
        }
        input.parse::<Token![.]>()?;
        Ok(
            Self {
                _first_comma: first_comma,
                value,
                args,
            }
        )
    }
}
impl ToTokens for Proclaimeth {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let value = &self.value;
        let args = &self.args;
        tokens.extend(quote!{println!(#value, #(#args),*);})
    }
}

fn the_magic_of(input: ParseStream) -> Result<()> {
    if input.peek(the) && input.peek2(magic) && input.peek3(of) {
        input.parse::<the>()?;
        input.parse::<magic>()?;
        input.parse::<of>()?;
    }
    Ok(())
}

fn find_type(input: ParseStream) -> Result<Type> {
    let ty;
    if input.peek(integer) || input.peek2(integer) {
        if input.peek(an) && input.peek2(integer) {
            input.parse::<an>()?;
            input.parse::<integer>()?;
            ty = syn::parse_str::<Type>("i64")?; 
        } else if input.peek(a) && input.peek2(integer) {
            return Err(input.error("erm aktually you have to use an instead of a"))
        } else if input.peek(integer) {
            input.parse::<integer>()?;
            ty = syn::parse_str::<Type>("i64")?;
        } else {
            return Err(input.error("not an integer?"))
        }
    } else if input.peek(float) || input.peek2(float) {
        if input.peek(a) && input.peek2(float) {
            input.parse::<a>()?;
            input.parse::<float>()?;
            ty = syn::parse_str::<Type>("f64")?;
        } else if input.peek(an) && input.peek2(float) {
            return Err(input.error("erm aktually you have to use a instead of an"))
        } else if input.peek(float) {
            input.parse::<float>()?;
            ty = syn::parse_str::<Type>("f64")?;
        } else {
            return Err(input.error("not a float?"))
        }
    } else if input.peek(boolean) || input.peek2(boolean) {
        if input.peek(a) && input.peek2(boolean) {
            input.parse::<a>()?;
            input.parse::<boolean>()?;
            ty = syn::parse_str::<Type>("bool")?;
        } else if input.peek(an) && input.peek2(boolean) {
            return Err(input.error("erm aktually you have to use a instead of an"))
        } else if input.peek(boolean) {
            input.parse::<float>()?;
            ty = syn::parse_str::<Type>("bool")?;
        } else {
            return Err(input.error("not a boolean?"))
        }
    } else if input.peek(string) || input.peek2(string) {
        if input.peek(a) && input.peek2(string) {
            input.parse::<a>()?;
            input.parse::<string>()?;
            ty = syn::parse_str::<Type>("&'static str")?;
        } else if input.peek(an) && input.peek2(string) {
            return Err(input.error("erm aktually you have to use a instead of an"))
        } else if input.peek(string) {
            input.parse::<string>()?;
            ty = syn::parse_str::<Type>("&'static str")?;
        } else {
            return Err(input.error("not a string?"))
        }
    } else {
        return Err(input.error("not a valid type"))
    }
    return Ok(ty)
}

fn comma_or_and(input: ParseStream) -> Result<()> {
    if input.peek(and) {
        input.parse::<and>()?;
        return Ok(())
    } else if input.peek(Token![,]) {
        input.parse::<Token![,]>()?;
        return Ok(())
    } else {
        return Err(input.error("huh no comma or and 💀"))
    }

}

#[derive(Clone)]
struct Function {
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
            input.parse::<Token![,]>()?;
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
//impl From<Function> for syn::ItemFn {
//    fn from(value: Function) -> Self {
//        Self {
//            attrs: Vec::new(),
//            vis: syn::Visibility::Inherited,
//            sig: Signature {
//                constness: None,
//                asyncness: None,
//                unsafety: None,
//                abi: None,
//                fn_token: Fn {
//                    span: value.conjure.span
//                },
//                ident: value.ident,
//                generics: Generics::default(),
//                paren_token: Paren {span: value.first_comma.span},
//                inputs: match value.args {
//                    None => Punctuated::default(),
//                    Some(args) => {
//                        let mut new_args: Vec<FnArg> = Vec::new();
//                        for arg in args {
//                            let fn_arg = FnArg::Typed(PatType { attrs: Vec::new(), pat: Box::new(Pat::Ident(PatIdent { attrs: Vec::new(), by_ref: None, mutability: None, ident: arg.0, subpat: None })), colon_token: Colon {spans: arg.2.spans}, ty: Box::new(arg.1) });
//                            new_args.push(fn_arg)
//                        }
//                        Punctuated::from_iter(new_args.into_iter())
//                    }
//                },
//                variadic: None,
//                output: match value.rtrn {
//                    None => syn::ReturnType::Default,
//                    Some(ty) => syn::ReturnType::Type(syn::token::RArrow { spans: [ty.1.span, ty.2.span] }, Box::new(ty.0))
//                }, 
//            },
//            block: Box::new(Block {
//                brace_token: syn::token::Brace { span: value.containing.span },
//                stmts: {
//                    let mut stmts = Vec::new();
//                    for stmt in value.stmts.into_iter() {
//                        let x = *stmt;
//                        stmts.push(x.into());
//                    }
//                    stmts
//                }
//            }),
//        }
//    }
//}
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

#[derive(Clone, Copy)]
/// should (expr), ?then. should (expr), ?then, ,  otherwise, . should (expr), ?then, , otherwise, should (expr) ?then, .
struct If {

}
impl Parse for If {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<should>()?;
        todo!()
    }
}

#[derive(Clone)]
struct FunctionCall {
    ident: Ident,
    fullstop: Option<FullStop>,
    args: Vec<FunctionArgs>
}
impl Parse for FunctionCall {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<invoke>()?;
        the_magic_of(input)?;
        let ident = input.parse::<Ident>()?;
        if input.peek(Token![.]) {
            //let dot: Dot = input.parse::<Token![.]>()?;
            //println!("functioncall no args");
            return Ok(Self {
                    ident,
                    fullstop: None,
                    args: Vec::new()
            })
        } else if input.peek(Token![,]) {
            let mut i = 0;
            input.parse::<Token![,]>()?;
            input.parse::<which>()?;
            input.parse::<takes>()?;
            let mut args = Vec::new();
            while !input.peek(Token![.]) {
                if i > 0 {
                    comma_or_and(input)?;
                }
                i += 1;
                let arg = input.parse::<FunctionArgs>()?;
                args.push(arg);
            }
            return Ok(Self {
                ident,
                fullstop: None,
                args
            })
            
        } else {
            Err(input.error("end the statement dum dum"))
        }

    }
}
impl ToTokens for FunctionCall {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let ident = self.ident.clone();
        let args = self.args.clone();
        if let Some(fullstop) = self.fullstop {
            let semi: Semi = fullstop.into();
            tokens.extend(quote!{#ident(#(#args),*)#semi})
        } else {
            tokens.extend(quote!{#ident(#(#args),*)})
        }
        
    }
}

#[derive(Clone)]
struct ReturnStatement {
    b_return: Return,
    var: FunctionArgs
}
impl Parse for ReturnStatement {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self {
            b_return: input.parse()?,
            var: input.parse()?
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

struct Expression {

}
impl Parse for Expression {
    fn parse(input: ParseStream) -> Result<Self> {
        todo!()
    }
}
impl ToTokens for Expression {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        todo!()
    }
}

#[derive(Clone)]
enum Statement {
    Assignment(Assignment),
    Proclaimeth(Proclaimeth),
    FunctionAssignment(Function),
    FunctionCall((FunctionCall, FullStop)),
    Return(ReturnStatement)
    //If(If),
    //ForLoop,
}
impl Parse for Statement {
    fn parse(input: ParseStream) -> Result<Self> {
        if {
            let fork = input.fork();
            fork.parse::<AssignmentToken>().is_ok()
        } {
            return Ok(Statement::Assignment(input.parse::<Assignment>()?))
        } else if input.peek(proclaimeth) {
            return Ok(Statement::Proclaimeth(input.parse::<Proclaimeth>()?))
        } else if input.peek(conjure) {
            return Ok(Statement::FunctionAssignment(input.parse::<Function>()?))
        } else if input.peek(invoke) {
            return Ok(Statement::FunctionCall((input.parse::<FunctionCall>()?, input.parse()?)))
        } else if input.peek(bestow) {
            return Ok(Statement::Return(input.parse::<ReturnStatement>()?))
        } else {
            return Err(input.error("Cannot find a valid statement"))
        }
        
    }
}
impl ToTokens for Statement {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Statement::Assignment(x) => {
                //let ret: Local = x.into();
                tokens.extend(quote!{#x})
            },
            Statement::Proclaimeth(x) => {
                tokens.extend(quote!{#x});
            },
            Statement::FunctionAssignment(x) => {
                tokens.extend(quote!{#x})
            },
            Statement::FunctionCall((x, y)) => {
                let xy = Semi {spans: y.spans};
                tokens.extend(quote!{#x #xy})
            }
            Statement::Return(x) => {
                tokens.extend(quote!{#x})
            }
            //_ => {}
        }
    }
}

struct Wizlang {
    statements: Vec<Statement>
}
impl Parse for Wizlang {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut statements = Vec::new();
        while !input.is_empty() {
            let statement = input.parse::<Statement>()?;
            statements.push(statement);
        }
        Ok(Self {
            statements
        })
    }
}
impl ToTokens for Wizlang {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        tokens.append_all(&self.statements);
    }
}

#[proc_macro]
pub fn wizlang(input: TokenStream) -> TokenStream {
    let test_input = input.clone();
    let wizlang = parse_macro_input!(test_input as Wizlang);
    //println!("{}", wizlang.to_token_stream().to_string());
    quote!(#wizlang).into()
    //input
}

struct Wizlang2 {
    stmts: Vec<stmts::Statement>
}

impl Parse for Wizlang2 {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut stmts = Vec::new();
        while !input.is_empty() {
            let stmt = input.parse::<stmts::Statement>()?;
            stmts.push(stmt);
        }
        Ok(Self {
            stmts
        })
    }
}

impl ToTokens for Wizlang2 {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        tokens.append_all(&self.stmts)
    }
}

#[proc_macro]
pub fn wizlang2(input: TokenStream) -> TokenStream {
    let wizlang2 = parse_macro_input!(input as Wizlang2);
    quote!{#wizlang2}.into()
}