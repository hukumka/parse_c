use std::fmt::Write;

use cata::parser::brackets::{
    BracketTreeItem as BTI,
    BracketTree as BT,
    parse_brackets,
    BracketParseError
};
use cata::lexer::{tokenize, Token, TokenData};
use cata::parser::parse_operators;
use cata::parser::parse_items::{
    test_for_operator, 
    get_token,
    get_tree
};
use cata::misc::split_by;


struct FunctionDef{
    ret_type: TypeName,
    name: String,
    arguments: Vec<(TypeName, String)>,
    block: Block
}

struct TypeName{
    base: String,
    pointer_count: usize
}

struct Block{
    statements: Vec<Statement>
}

struct Operator{
    name: String
}


enum Statement{
    Expression(Expression),
    Condition(Condition),
    ForLoop(ForLoop),
    VarDef(VarDef)
}


enum Expression{
    FuncCall(Box<FuncCall>),
    BinOp(Box<BinOp>),
    PrefixOp(Box<PrefixOp>),
    SyffixOp(Box<SyffixOp>),
    Variable(String),
    Constant(String)
}


struct VarDef{
    type_: TypeName,
    assignments: Vec<(String, Expression)>
}


struct ForLoop{
    init: ForLoopInit,
    step: Expression,
    cond: Expression,
    block: Block
}


enum ForLoopInit{
    Expression(Expression),
    VarDef(VarDef)
}


struct Condition{
    ifs: Vec<(Expression, Block)>,
    else_block: Option<Block>
}


struct FuncCall{
    name: String,
    arguments: Vec<Expression>
}


struct BinOp{
    op: String,
    left: Expression,
    right: Expression
}


struct PrefixOp{
    op: String,
    right: Expression
}


struct SyffixOp{
    op: String,
    left: Expression
}


#[derive(Debug)]
struct ParseError{
    msg: String,
    where_: Option<TokenData>
}

macro_rules! parse_error_fmt{
    (#in $where_: expr; $($e: tt)*) => {
        {
            let mut msg = String::new();
            write!(msg, $($e)*).unwrap();
            ParseError{msg, where_: $where_}
        }
    };
    ($($e: tt)*) => {
        {
            let mut msg = String::new();
            write!(msg, $($e)*).unwrap();
            ParseError{msg, where_: None}
        }
    };
}


trait ParseFromBracketTree: Sized{
    fn parse(&[BTI])->Result<(Self, &[BTI]), ParseError>;

    fn parse_from_chain<T>(from: Result<(T, &[BTI]), ParseError>)->Result<((Self, T), &[BTI]), ParseError>{
        from.and_then(|(base, tree)|{
            Self::parse(tree).map(|(new, tree)| ((new, base), tree))
        })
    }
}

impl ParseFromBracketTree for FunctionDef{
    fn parse(tree: &[BTI])->Result<(Self, &[BTI]), ParseError>{
        let (type_, tree) = TypeName::parse(tree)?;
        let (name, tree) = String::parse(tree)?;
        let (arguments, tree) = parse_arguments(tree)?;
        let (block, tree) = Block::parse(tree)?;
        Ok((Self{ret_type: type_, name, arguments, block}, tree))
    }
}


impl ParseFromBracketTree for TypeName{
    fn parse(tree: &[BTI])->Result<(Self, &[BTI]), ParseError>{
        let (base, tree) = String::parse(tree)?;

        // count all the reference symbols (*)
        let mut pointer_count = 0;
        let mut tree = tree;
        while let Some((first, tail)) = tree.split_first(){
            match get_operator(first){
                // rest must be * (may be separated by spaces)
                Some(op) => {
                    if op.chars().all(|x| x == '*'){
                        tree = tail;
                        pointer_count += op.len();
                    }else{
                        break;
                    }
                },
                _ => {break;}
            }
        }

        Ok((TypeName{base, pointer_count}, tree))
    }
}


fn get_operator(t: &BTI)->Option<String>{
    match get_token(t){
        Some(Token::Operator(x)) => Some(x),
        _ => None,
    }
}

fn get_name(t: &BTI)->Option<String>{
    match get_token(t){
        Some(Token::Name(x)) => Some(x),
        _ => None,
    }
}

impl ParseFromBracketTree for String{
    fn parse(tree: &[BTI])->Result<(Self, &[BTI]), ParseError>{
        if let Some((x, rest)) = tree.split_first(){
            if let Some(x) = get_name(x){
                Ok((x.to_string(), rest))
            }else{
                Err(parse_error_fmt!(#in x.first_token(); "String::parse: Expected string, but found {:?}", x))
            }
        }else{
            Err(parse_error_fmt!("String::parse: Expected string, but tree is empty"))
        }
    }
}

impl ParseFromBracketTree for Operator{
    fn parse(tree: &[BTI])->Result<(Self, &[BTI]), ParseError>{
        if let Some((x, rest)) = tree.split_first(){
            if let Some(x) = get_operator(x){
                Ok((Operator{name: x.to_string()}, rest))
            }else{
                Err(parse_error_fmt!(#in x.first_token(); "Operator::parse: Expected operator, but found {:?}", x))
            }
        }else{
            Err(parse_error_fmt!("String::parse: Expected operator, but tree is empty"))
        }
    }
}


fn parse_arguments(t: &[BTI])->Result<(Vec<(TypeName, String)>, &[BTI]), ParseError>{
    if let Some((tree, rest)) = t.split_first(){
        if let &BTI::Tree(ref tree) = tree{
            let args_iter = split_by(&tree.nodes, |x| test_for_operator(x, ","))
                .map(|arg_tree| {
                    let (type_, arg_tree) = TypeName::parse(arg_tree)?;
                    let (name, arg_tree) = String::parse(arg_tree)?;
                    if arg_tree.is_empty(){
                        Ok(((type_, name), arg_tree))
                    }else{
                        Err(parse_error_fmt!(
                            #in arg_tree[0].first_token(); 
                            "parse_arguments: Expected , or ) but found {:?}", 
                            arg_tree[0].first_token()
                        ))
                    }
                });
            let mut x = Vec::new();
            for i in args_iter{
                x.push(i?.0);
            }
            Ok((x, rest))
        }else{
            Err(parse_error_fmt!(#in tree.first_token(); "parse_arguments: Expected inner tree but found {:?}", tree))
        }
    }else{
        Err(parse_error_fmt!("parse_arguments: Expected inner tree but tree is empty"))
    }
}


impl ParseFromBracketTree for Block{
    fn parse(tree: &[BTI])->Result<(Self, &[BTI]), ParseError>{
        if let Some((tree, rest)) = tree.split_first(){
            if let &BTI::Tree(ref tree) = tree{
                let mut nodes: &[_] = &tree.nodes;
                let mut x = Vec::new();
                while !nodes.is_empty(){
                    let (statement, rest) = Statement::parse(nodes)?;
                    nodes = rest;
                    x.push(statement);
                }
                Ok((Self{statements: x}, rest))
            }else{
                Err(parse_error_fmt!(#in tree.first_token(); "parse_arguments: Expected inner tree but found {:?}", tree))
            }
        }else{
            Err(parse_error_fmt!("Block::parse: Expected inner tree but tree is empty"))
        }
    }
}


impl Block{
    fn from_statement(s: Statement)->Block{
        Block{statements: vec![s]}
    }
}


impl ParseFromBracketTree for Statement{
    fn parse(tree: &[BTI])->Result<(Self, &[BTI]), ParseError>{
        if tree.is_empty(){
            return Err(parse_error_fmt!("Statement::parse: Expected for if type or expression, but tree is empty"));
        }
        match get_name(&tree[0]){
            Some(ref x) if x == "if" => Condition::parse(tree).map(|(a, b)| (Statement::Condition(a), b)),
            Some(ref x) if x == "for" => ForLoop::parse(tree).map(|(a, b)| (Statement::ForLoop(a), b)),
            _ => {
                if let Ok((var_def, rest)) = VarDef::parse(tree){
                    Ok((Statement::VarDef(var_def), rest))
                }else{
                    let (expr, tree) = Expression::parse(tree)?;
                    let (semicolon, tree) = tree.split_first().ok_or(parse_error_fmt!("Statement::parse: expected ';', but found nothing"))?;
                    Ok((Statement::Expression(expr), tree))
                }
            }
        }
    }
}


impl ParseFromBracketTree for Condition{
    fn parse(tree: &[BTI])->Result<(Self, &[BTI]), ParseError>{
        // condition consist of 
        // name(if) expr(*) (block(*)|statement(*)) [name(else) (block(*)|statement(*))]
        let (if_token, tree) = tree.split_first()
            .ok_or(parse_error_fmt!("Condition::parse: Expected 'if' keyword, but found nothing"))?;
        expect_keyword(if_token, &["if"], "Condition::parse")?;
        let (expr, tree) = Expression::parse(tree)?;
        let (block, tree) = parse_statement_or_block(tree)?;
        if let Some((else_token, new_tree)) = tree.split_first(){
            if test_name(else_token, &|n| n == "else"){
                let (else_block, tree) = parse_statement_or_block(new_tree)?;
                Ok((Condition{ifs: vec![(expr, block)], else_block: Some(else_block)}, tree))
            }else{
                Ok((Condition{ifs: vec![(expr, block)], else_block: None}, tree))
            }
        }else{
            Ok((Condition{ifs: vec![(expr, block)], else_block: None}, tree))
        }
    }
}


impl ParseFromBracketTree for ForLoop{
    fn parse(tree: &[BTI])->Result<(Self, &[BTI]), ParseError>{
        let (for_keyword, tree) = tree.split_first()
            .ok_or(parse_error_fmt!("ForLoop::parse: Expected for, but found nothing"))?;
        expect_keyword(for_keyword, &["for"], "ForLoop::parse")?;

        let (loop_setup, tree) = tree.split_first()
            .ok_or(parse_error_fmt!("ForLoop::parse: Expected for(*; *; *) but found nothing"))?;
        if let &BTI::Tree(ref ls) = loop_setup{
            // parse initialization
            let ls = &ls.nodes;
            let var_def_wrapped = VarDef::parse(ls);
            let (init, ls) = if let Ok((var_def, ls)) = var_def_wrapped{
                (ForLoopInit::VarDef(var_def), ls)
            }else{
                let (expr, ls) = Expression::parse(ls)?;
                (ForLoopInit::Expression(expr), ls)
            };
            let (semicolon, ls) = ls.split_first()
                .ok_or(parse_error_fmt!("ForLoop::parse: Expected ';' but found nothing"))?;
            // parse increment
            let (condition, ls) = Expression::parse(ls)?;
            let (semicolon, ls) = ls.split_first()
                .ok_or(parse_error_fmt!("ForLoop::parse: Expected ';' but found nothing"))?;
            let (increment, ls) = Expression::parse(ls)?;
            if tree.is_empty(){
                let (block, tree) = parse_statement_or_block(tree)?;
                Ok((ForLoop{init, step: increment, cond: condition, block}, tree))
            }else{
                Err(parse_error_fmt!("ForLoop::parse: Expected ')' but '{:?}' found", tree))
            }
        }else{
            Err(parse_error_fmt!("ForLoop::parse: Expected for(*; *; *) but found nothing"))
        }
    }
}


impl ParseFromBracketTree for VarDef{
    fn parse(tree: &[BTI])->Result<(Self, &[BTI]), ParseError>{
        let (type_, tree) = TypeName::parse(tree)?;
        let mut tree = tree;
        let mut assignments = vec![];
        loop{
            // parse 'name operator expression' as 'name = expr'
            if let Ok((assignment, tr)) = Self::parse_assignment(tree){
                tree = tr;
                assignments.push(assignment);
            }else{
                break;
            }
        }
        if assignments.is_empty(){
            Err(parse_error_fmt!("VarDef::parse: expected assignment"))
        }else{
            Ok((VarDef{assignments, type_}, tree))
        }
    }
}

impl VarDef{
    fn parse_assignment(tree: &[BTI])->Result<((String, Expression), &[BTI]), ParseError>{
        let (name, tree) = String::parse(tree)?;
        let (op, tree) = Operator::parse(tree)?;
        if op.name == "="{
            let (expr, tree) = Expression::parse_until(tree, ",")?;
            Ok(((name, expr), tree))
        }else{
            Err(parse_error_fmt!("VarDef::parse_assignment: expected '=' operator, but {} found", op.name))
        }
    }
}


fn parse_statement_or_block(tree: &[BTI])->Result<(Block, &[BTI]), ParseError>{
    let statement_wrapped = Statement::parse(tree);
    let (block, tree) = if let Ok((statement, _)) = statement_wrapped{
        (Block::from_statement(statement), tree)
    }else{
        Block::parse(tree)?
    };
    Ok((block, tree))
}


fn expect_keyword(item: &BTI, expected: &[&str], input: &str)->Result<(), ParseError>{
    if test_name(item, &|n| expected.contains(&n)){
        Ok(())
    }else{
        Err(parse_error_fmt!("{}: Expected {:?}, but {:?} found.", input, expected, item))
    }
}

fn test_name<T: Fn(&str)->bool>(item: &BTI, predicate: &T)->bool{
    let name_wrapped = get_name(item);
    if let Some(name) = name_wrapped{
        predicate(&name)
    }else{
        false
    }
}


impl ParseFromBracketTree for Expression{
    fn parse(tree: &[BTI])->Result<(Self, &[BTI]), ParseError>{
        Self::parse_until(tree, ";")
    }
}


impl Expression{
    fn parse_until<'a>(tree: &'a [BTI], stop_operator: &str)->Result<(Self, &'a [BTI]), ParseError>{
        Err(parse_error_fmt!("no implemented"))
    }
}
