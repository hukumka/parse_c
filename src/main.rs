#![feature(slice_patterns)]
#![feature(advanced_slice_patterns)]
#![feature(str_checked_slicing)]
#![feature(box_patterns)]


#[macro_use]
extern crate derive_error;

#[macro_use]
extern crate cata;

mod parse;
mod build;

use std::fs::File;
use std::io;
use std::fmt;
use std::fmt::Write;
use std::io::prelude::*;

const TYPE_KEYWORDS: &[&str] = &[
    "int",
    "unsigned",
    "float",
    "double"
];

use cata::lexer::{
    tokenize,
    Token,
    TokenData
};
use cata::parser::parse_operators;
use cata::parser::brackets::{
    BracketParseError, 
    parse_brackets, 
    BracketTree, 
    BracketTreeItem
};
use cata::parser::parse_items::{
    test_for_operator, 
    get_token,
    get_tree
};


use parse::{FunctionDef, ParseFromBracketTree};
use build::{BuildJs, Formatable};


fn main() {
    /*let tree = get_bracket_tree("/home/hukumka/0/diplodoc/test1.cpp").unwrap();
    let func = parse_function_def(&tree.nodes[9..13]);
    println!("{:?}", func);
    println!(" ");
    print_body(&func.unwrap().body, 0);*/
    let file_path = "/home/hukumka/src/parse_c/test.cpp";
    let file = File::open(file_path).unwrap();
    let mut bufread = io::BufReader::new(file);
    let mut contents = String::new();
    bufread.read_to_string(&mut contents).unwrap();

    let mut out = String::new();
    generate_js(&contents, &mut out).unwrap();
    println!("var block = {};", out);
}


fn generate_js<T: fmt::Write>(c_code: &str, formatter: &mut T)->fmt::Result{
    // skip includes
    let skip_includes: Vec<_> = c_code.split("\n").filter(|x| !x.starts_with("#")).collect();
    let skip = &skip_includes.join("\n");
    // build tree
    let tokens = tokenize(&skip);
    let tree = parse_brackets(tokens).unwrap();
    let (func_def, tree) = FunctionDef::parse(&tree.nodes).unwrap();
    write!(formatter, "{}", Formatable::new(&|f| func_def.write(f)))

    // parse functions
    /*let functions: Vec<_> = tree.nodes.chunks(4).map(|x| parse_function_def(x).unwrap()).collect();

    write!(formatter, "{}", generate_function_js(&functions[0]).unwrap())?;
    Ok(())
    //panic!();*/
}

fn generate_function_js(func: &Function)->Result<String, String>{
    generate_block(&func.body)
}

fn generate_block(body: &[Statement])->Result<String, String>{
    {
        use std::fmt::Write;
        let mut res = String::new();
        write!(&mut res, "function(){{var block = new Block();\n").unwrap();
        for statement in body{
            write!(&mut res, "block.add({});\n", generate_statement_object(statement)?).unwrap();
        }
        write!(&mut res, "return block;}}()").unwrap();
        Ok(res)
    }
}


fn generate_statement_object(statement: &Statement)->Result<String, String>{
    match statement{
        &Statement::VarDef(VariableDefine{ref type_, ref name, ref tail, ..}) => {
            let mut code = String::new();
            write!(code, "{} {} = {}", type_, name, operator_node_into_string(tail)).unwrap();
            let mut res = String::new();
            write!(res, 
                "new Action('{}', function(namespace){{namespace.define('{}', {}(namespace));}})", 
                code, 
                name, 
                generate_operator_node_function(tail)
            ).unwrap();
            Ok(res.to_string())
        },
        &Statement::Assignment((ref name, ref op, ref tail, _)) => {
            let mut code = String::new();
            write!(code, "{} {} {}", name, op, operator_node_into_string(tail)).unwrap();
            let mut res = String::new();
            if op == &"="{
                write!(res, "new Action('{}', function(namespace){{namespace.set('{}', {}(namespace));}})", code, name, generate_operator_node_function(tail)).unwrap();
            }else{
                write!(res, "new Action('{}', function(namespace){{namespace.set('{}', namespace.get('{1}'){}({}(namespace)));}})", code, name, op.get(0..1).unwrap(), generate_operator_node_function(tail)).unwrap();
            }
            Ok(res.to_string())
        },
        &Statement::Cond(ref cond) => generate_condition_object(&cond),
        &Statement::For(ref for_) => generate_for_loop_object(&for_),
        &Statement::Return((ref tail, _)) => {
            let mut code = String::new();
            write!(code, "return {}", operator_node_into_string(tail)).unwrap();
            let mut res = String::new();
            write!(res, "new Return('{}', {})", code, generate_operator_node_function(tail)).unwrap();
            Ok(res)
        }
        _ => Err("fn generate_statement_object(): not implemented yet".to_string())
    }
}


fn operator_node_into_string(node: &parse_operators::Node)->String{
    match node{
        &parse_operators::Node::Leaf(ref x) => into_string(x, false),
        &parse_operators::Node::Binary(ref x) => {
            operator_node_into_string(&x.left) + " " + &x.operator.op + " " + &operator_node_into_string(&x.right)
        }
        &parse_operators::Node::Prefix(ref x) => {
            x.operator.op.clone() + &operator_node_into_string(&x.node)
        }
        &parse_operators::Node::Syffix(ref x) => {
            operator_node_into_string(&x.node) + &x.operator.op
        }
    }
}

fn generate_operator_node_function(node: &parse_operators::Node)->String{
    generate_operator_node_function_r(node).0
}

fn generate_operator_node_function_r(node: &parse_operators::Node)->(String, Option<String>){
    let mut res = String::new();
    match *node{
        parse_operators::Node::Leaf(ref x) => {
            write!(res, "function(namespace){{return {};}}", into_string(x, true)).unwrap();
            (res, Some(into_string(x, false)))
        },
        parse_operators::Node::Prefix(ref x) => {
            if x.operator.op == "++"{
                let (func, var) = generate_operator_node_function_r(&x.node);
                let var = var.unwrap();
                write!(res, 
                    "function(namespace){{ var res = {0}; res(namespace); var x = namespace.get('{1}'); ++x; namespace.set('{1}', x); return x;}}",
                    func,
                    var
                ).unwrap();
                (res, Some(var))
            }else if x.operator.op == "--"{
                let (func, var) = generate_operator_node_function_r(&x.node);
                let var = var.unwrap();
                write!(res, 
                    "function(namespace){{ var res = {0}; res(namespace); var x = namespace.get('{1}'); --x; namespace.set('{1}', x); return x;}}",
                    func,
                    var
                ).unwrap();
                (res, Some(var))
            }else{
                write!(res, 
                    "function(namespace){{return {}{}(namespace);}}",
                    x.operator.op,
                    generate_operator_node_function(&x.node)
                ).unwrap();
                (res, None)
            }
        },
        parse_operators::Node::Syffix(ref x) => {
            if x.operator.op == "++"{
                let (func, var) = generate_operator_node_function_r(&x.node);
                let var = var.unwrap();
                write!(res, 
                    "function(namespace){{ var res = {0}; res(namespace); var x = namespace.get('{1}'); namespace.set('{1}', x+1); return x;}}",
                    func,
                    var
                ).unwrap();
                (res, Some(var))
            }else if x.operator.op == "--"{
                let (func, var) = generate_operator_node_function_r(&x.node);
                let var = var.unwrap();
                write!(res, 
                "function(namespace){{ var res = {0}; res(namespace); var x = namespace.get('{1}'); namespace.set('{1}', x-1); return x;}}",
                    func,
                    var
                ).unwrap();
                (res, Some(var))
            }else{
                write!(res, 
                    "function(namespace){{return {}{}(namespace);}}",
                    x.operator.op,
                    generate_operator_node_function(&x.node)
                ).unwrap();
                (res, None)
            }
        },
        parse_operators::Node::Binary(ref x) => {
            write!(res, 
                "function(namespace){{return {}(namespace) {} {}(namespace)}}",
                generate_operator_node_function(&x.left),
                x.operator.op,
                generate_operator_node_function(&x.right)
            ).unwrap();
            (res, None)
        }
    }
}


fn generate_condition_object(condition: &Condition)->Result<String, String>{
    match condition{
        &Condition::Simple((ref cond, ref then)) => {
            let mut res = String::new();  
            let cond_text = into_string(&cond.nodes, false);
            let cond = into_string(&cond.nodes, true);
            let block = generate_block(&then)?;
            write!(res, "new Condition('{}', function(namespace){{return {}}}, {}, undefined)", cond_text, cond, block).unwrap();
            Ok(res)
        },
        &Condition::WithElse((ref cond, ref then, ref else_b)) => {
            let mut res = String::new();  
            let cond_text = into_string(&cond.nodes, false);
            let cond = into_string(&cond.nodes, true);
            let block = generate_block(&then)?;
            let else_block = generate_block(&else_b)?;
            write!(res, "new Condition('{}', function(namespace){{return {}}}, {}, {})", cond_text, cond, block, else_block).unwrap();
            Ok(res)
        },
        &Condition::Complex((ref cond, ref then, ref else_cond)) => {
            let mut res = String::new();  
            let cond_text = into_string(&cond.nodes, false);
            let cond = into_string(&cond.nodes, true);
            let block = generate_block(&then)?;
            let else_block = generate_condition_object(&else_cond)?;
            write!(res, "new Condition('{}', function(namespace){{return {}}}, {}, {})", cond_text, cond, block, else_block).unwrap();
            Ok(res)
        },
    }
}


fn generate_for_loop_object<'a>(for_loop: &(Vec<Statement<'a>>, Vec<Statement<'a>>))->Result<String, String>{
    match for_loop.0.as_slice(){
        &[ref a, ref b, ref c] => {
            let mut string = String::new(); 
            write!(string, 
                   "for({}; {}; {}){{", 
                   statement_into_string(a, false), 
                   statement_into_string(b, false), 
                   statement_into_string(c, false)
            ).unwrap();

            let mut res = String::new();
            write!(res, "new ForLoop('{}', {}, {}, {}, {})", 
                   string, statement_into_string(a, true), 
                   statement_into_string(b, true), 
                   statement_into_string(c, true), 
                   generate_block(&for_loop.1)?
            ).unwrap();
            Ok(res)
        },
        _ => Err("Invalid for loop".to_string())
    }
}


fn statement_into_string(code: &Statement, use_namespace: bool)->String{
    match code{
        &Statement::VarDef(VariableDefine{ref name, ref tail, ref type_, ..}) => {
            let mut res = String::new();
            if use_namespace{
                write!(res, "function(namespace){{namespace.define('{}', {}(namespace));}}", name, generate_operator_node_function(tail)).unwrap();
            }else{
                write!(res, "{} {} = {}", type_, name, operator_node_into_string(tail)).unwrap();
            }
            res
        },
        &Statement::Assignment((ref name, ref op, ref tail, _)) => {
            let mut res = String::new();
            if use_namespace{
                if op == &"="{
                    write!(res, 
                        "function(namespace){{namespace.set('{}', {}(namespace));}}",
                        name,
                        generate_operator_node_function(tail)
                    ).unwrap();
                }else{
                    write!(res, 
                        "function(namespace){{namespace.set('{}', namespace.get('{0}'){}{}(namespace));}}", 
                        name, 
                        op.get(0..1).unwrap(), 
                        generate_operator_node_function(tail)
                    ).unwrap();
                }
            }else{
                write!(res, "{} {} {}(namespace)", name, op, generate_operator_node_function(tail)).unwrap();
            }
            res
        },
        &Statement::Sem((ref op_node, _len)) => {
            let mut res = String::new();
            if use_namespace{
                write!(res, "{}", generate_operator_node_function(op_node)).unwrap();
            }else{
                write!(res, "{}", operator_node_into_string(op_node)).unwrap();
            }
            res
        }
        _ => panic!("fn statement_into_string(): not implemented")
    }
}


fn into_string(code: &[BracketTreeItem], use_namespace: bool)->String{
    let mut res = String::new();
    if code.len() == 0{
        return "".to_string();
    }
    let code = if test_for_operator(code.last().unwrap(), ";"){
        &code[0..code.len()-1]
    }else{
        code
    };
    for i in code{
        match i{
            &BracketTreeItem::Tree(ref a) => {
                let (ref l, ref r) = a.brackets.clone().unwrap();
                let (l, r) = (l.token.str(), r.token.str());
                write!(res, "{}{}{}", l, into_string(&a.nodes, use_namespace), r).unwrap();
            },
            &BracketTreeItem::Token(ref a) => {
                match &a.token{
                    &Token::Name(ref n) => if use_namespace{
                        write!(res, "namespace.get('{}')", n).unwrap()
                    }else{
                        write!(res, "{}", n).unwrap()
                    },
                    _ => write!(res, "{}", a.token.str()).unwrap()
                }
            }
        }
    }
    res
}


#[derive(Debug, Error)]
enum Error{
    IoError(io::Error),
    BracketError(BracketParseError)
}


fn parse_function_def<'a>(items: &'a [BracketTreeItem])->Option<Function<'a>>{
    if items.len() == 4{
        match (
            get_token(&items[0]), 
            get_token(&items[1]),
            get_tree(&items[2]),
            get_tree(&items[3])
        ){
            (
                Some(Token::Name(ret_type)), 
                Some(Token::Name(name)), 
                Some(arguments),
                Some(body)
            ) => Some(Function{ret_type, name, arguments, body: parse_body(&body.nodes)}),
            _ => None

        }
    }else{
        None
    }
}


#[derive(Debug)]
struct Function<'a>{
    ret_type: String,
    name: String,
    arguments: &'a BracketTree,
    body: Vec<Statement<'a>>
}


fn parse_body(items: &[BracketTreeItem])->Vec<Statement>{
    let mut offset = 0;
    let mut res = vec![];
    while let Some(statement) = parse_statement(&items[offset..]){
        offset += statement.len();
        res.push(statement);
    }
    res
}


fn parse_statement(items: &[BracketTreeItem])->Option<Statement>{
    if !items.is_empty(){
        if is_name(&items[0], "if"){
            parse_condition(items).map(|x| Statement::Cond(x))
        }else if is_name(&items[0], "for"){
            match &items[1..3]{
                &[BracketTreeItem::Tree(ref a), BracketTreeItem::Tree(ref b)] => {
                    Some(Statement::For((parse_body(&a.nodes), parse_body(&b.nodes))))
                },
                _ => None
            }
        }else{
            get_until_semicolon(items)
        }
    }else{
        None
    }
}

fn is_name(item: &BracketTreeItem, name: &str)->bool{
    match get_token(item){
        Some(Token::Name(s)) => s == name,
        _ => false
    }
}

fn get_until_semicolon(items: &[BracketTreeItem])->Option<Statement>{
    let first_semicolon = items.iter().enumerate().find(|&(_, x)| test_for_operator(x, ";")).map(|(x, _)| x).unwrap_or(items.len()-1);

    let levels = parse_operators::PrecedenceLevels::new();
    let onb = parse_operators::NodeBuilder::new(&levels, |x| ["++", "--"].iter().any(|a| a == &x));

    match &items[0..first_semicolon+1]{
        &[BracketTreeItem::Token(TokenData{token: Token::Name(ref x), ..}), ref tail..] if x == &"return" => {
            let x = if test_for_operator(tail.last().unwrap(), ";"){
                &tail[0..tail.len()-1]
            }else{
                tail
            };
            Some(Statement::Return((onb.build_tree(x), tail.len())))
        },
        &[BracketTreeItem::Token(TokenData{token: Token::Name(ref x), ..}), BracketTreeItem::Token(TokenData{token: Token::Name(ref s), ..}), ref op, ref tail..] if is_basic_type(x) && test_for_operator(op, "=") => {
            let y = if test_for_operator(tail.last().unwrap(), ";"){
                &tail[0..tail.len()-1]
            }else{
                tail
            };
            Some(Statement::VarDef(VariableDefine{type_: &x, name: &s, tail: onb.build_tree(y), len: tail.len()}))
        },
        &[BracketTreeItem::Token(TokenData{token: Token::Name(ref s), ..}), BracketTreeItem::Token(TokenData{token: Token::Operator(ref op), ..}), ref tail..] if op == "=" || op.get(1..2) == Some("=")  => {
            let x = if test_for_operator(tail.last().unwrap(), ";"){
                &tail[0..tail.len()-1]
            }else{
                tail
            };
            Some(Statement::Assignment((&s, &op, onb.build_tree(x), tail.len())))
        },
        &[ref tail..] => {
            let x = if test_for_operator(tail.last().unwrap(), ";"){
                &tail[0..tail.len()-1]
            }else{
                tail
            };
            Some(Statement::Sem((onb.build_tree(x), tail.len())))
        }
    }
}


fn is_basic_type(x: &str)->bool{
    TYPE_KEYWORDS.iter().any(|t| &x == t)
}

fn parse_condition(items: &[BracketTreeItem])->Option<Condition>{
    match items{
        &[
            ref if_, 
            BracketTreeItem::Tree(ref cond), 
            BracketTreeItem::Tree(ref body), 
            ref else_, 
            ref elif_, 
        ..] if is_name(&if_, "if") && is_name(&else_, "else") && is_name(&elif_, "if") => {
            if let Some(elif_cond) = parse_condition(&items[4..]){
                Some(Condition::Complex((&cond, parse_body(&body.nodes), Box::new(elif_cond))))
            }else{
                None
            }
        },

        &[
            ref if_, 
            BracketTreeItem::Tree(ref cond), 
            BracketTreeItem::Tree(ref body), 
            ref else_, 
            BracketTreeItem::Tree(ref else_body), 
        ..] if is_name(&if_, "if") && is_name(&else_, "else") => {
            Some(Condition::WithElse((&cond, parse_body(&body.nodes), parse_body(&else_body.nodes))))
        },

        &[
            ref if_, 
            BracketTreeItem::Tree(ref cond), 
            BracketTreeItem::Tree(ref body), 
        ..] if is_name(&if_, "if")  => {
            Some(Condition::Simple((&cond, parse_body(&body.nodes))))
        },

        _ => None
    }
}


#[derive(Debug)]
enum Statement<'a>{
    Sem((parse_operators::Node<'a>, usize)),
    VarDef(VariableDefine<'a>),
    Assignment((&'a str, &'a str, parse_operators::Node<'a>, usize)),
    Cond(Condition<'a>),
    For((Vec<Statement<'a>>, Vec<Statement<'a>>)),
    Return((parse_operators::Node<'a>, usize))
}


impl<'a> Statement<'a>{
    fn len(&'a self)->usize{
        match self{
            &Statement::Sem(ref s) => s.1,
            &Statement::VarDef(VariableDefine{ref len, ..}) => 3 + len, // var a = *rest*
            &Statement::Assignment((_, _, _, a)) => 2 + a, // var a = *rest*
            &Statement::Cond(ref s) => s.len(),
            &Statement::For(_) => 3,
            &Statement::Return(ref s) => 1 + s.1
        }
    }
}


#[derive(Debug)]
struct VariableDefine<'a>{
    type_: &'a str,
    name: &'a str,
    tail: parse_operators::Node<'a>,
    len: usize
}


#[derive(Debug)]
enum Condition<'a>{
    Simple((&'a BracketTree, Vec<Statement<'a>>)), // condition and block
    WithElse((&'a BracketTree, Vec<Statement<'a>>, Vec<Statement<'a>>)), // condition and block and else block
    Complex((&'a BracketTree, Vec<Statement<'a>>, Box<Condition<'a>>)) // condition and block and else if condition motherfucker
}


impl<'a> Condition<'a>{
    fn len(&'a self)->usize{
        match self{
            &Condition::Simple(_) => 3, // if b1 b2
            &Condition::WithElse(_) => 5, // if b1 b2 else b3
            &Condition::Complex((_, _, ref cb)) => 4 + &cb.len() // if b1 b2 else nestedcond
        }
    }
}
