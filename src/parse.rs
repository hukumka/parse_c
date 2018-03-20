use std::fmt::Write;

use cata::parser::brackets::{
    BracketTreeItem as BTI,
    BracketTree as BT
};
use cata::lexer::{Token, TokenData};
use cata::parser::parse_operators;
use cata::parser::parse_items::{
    test_for_operator, 
    get_token
};
use cata::misc::split_by;


#[derive(Debug, Eq, PartialEq)]
pub struct FunctionDef{
    pub ret_type: TypeName,
    pub name: String,
    pub arguments: Vec<(TypeName, String)>,
    pub block: Block
}

#[derive(Debug, Eq, PartialEq)]
pub struct TypeName{
    pub base: String,
    pub pointer_count: usize
}

#[derive(Debug, Eq, PartialEq)]
pub struct Block{
    pub statements: Vec<Statement>
}

#[derive(Debug, Eq, PartialEq)]
pub struct Operator{
    pub name: String
}


#[derive(Debug, Eq, PartialEq)]
pub enum Statement{
    Expression(Expression),
    Condition(Condition),
    ForLoop(ForLoop),
    VarDef(VarDef),
    Return(Expression)
}


#[derive(Debug, Eq, PartialEq)]
pub enum Expression{
    FuncCall(Box<FuncCall>),
    BinOp(Box<BinOp>),
    PrefixOp(Box<PrefixOp>),
    SyffixOp(Box<SyffixOp>),
    Variable(String),
    Constant(String),
    New(Box<(TypeName, Expression)>),
    Index(Box<(Expression, Expression)>)
}


#[derive(Debug, Eq, PartialEq)]
pub struct VarDef{
    pub type_: TypeName,
    pub assignments: Vec<(String, Expression)>
}


#[derive(Debug, Eq, PartialEq)]
pub struct ForLoop{
    pub init: ForLoopInit,
    pub step: Expression,
    pub cond: Expression,
    pub block: Block
}


#[derive(Debug, Eq, PartialEq)]
pub enum ForLoopInit{
    Expression(Expression),
    VarDef(VarDef)
}


#[derive(Debug, Eq, PartialEq)]
pub struct Condition{
    pub ifs: Vec<(Expression, Block)>,
    pub else_block: Option<Block>
}


#[derive(Debug, Eq, PartialEq)]
pub struct FuncCall{
    pub name: String,
    pub arguments: Vec<Expression>
}


#[derive(Debug, Eq, PartialEq)]
pub struct BinOp{
    pub op: String,
    pub left: Expression,
    pub right: Expression
}


#[derive(Debug, Eq, PartialEq)]
pub struct PrefixOp{
    pub op: String,
    pub right: Expression
}


#[derive(Debug, Eq, PartialEq)]
pub struct SyffixOp{
    pub op: String,
    pub left: Expression
}


#[derive(Debug)]
pub struct ParseError{
    msg: String,
    where_: Option<TokenData>
}


impl ParseError{
    fn extend_where(self, where_: Option<TokenData>)->Self{
        ParseError{msg: self.msg, where_: self.where_.or(where_)}
    }
}

trait AddWhereTrait{
    fn add_where(self, where_: Option<TokenData>)->Self;
}
impl<T> AddWhereTrait for Result<T, ParseError>{
    fn add_where(self, where_: Option<TokenData>)->Self{
        self.or_else(|x| Err(x.extend_where(where_)))
    }
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


pub trait ParseFromBracketTree: Sized{
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
        let initial_tree = tree;
        if tree.is_empty(){
            return Err(parse_error_fmt!("Statement::parse: Expected for if type or expression, but tree is empty"));
        }
        match get_name(&tree[0]){
            Some(ref x) if x == "if" => Condition::parse(tree).map(|(a, b)| (Statement::Condition(a), b)),
            Some(ref x) if x == "for" => ForLoop::parse(tree).map(|(a, b)| (Statement::ForLoop(a), b)),
            Some(ref x) if x == "return" => {
                let tree = &tree[1..];
                let (expr, tree) = Expression::parse(tree).add_where(first_token(initial_tree))?;
                let (_semicolon, tree) = tree.split_first().ok_or(parse_error_fmt!("Statement::parse: expected ';', but found nothing"))?;

                Ok((Statement::Return(expr), tree))
            }
            _ => {
                if let Ok((var_def, rest)) = VarDef::parse(tree){
                    let (_semicolon, rest) = rest.split_first().ok_or(parse_error_fmt!("Statement::parse: expected ';', but found nothing"))?;
                    Ok((Statement::VarDef(var_def), rest))
                }else{
                    let (expr, tree) = Expression::parse(tree).add_where(first_token(initial_tree))?;
                    let (_semicolon, tree) = tree.split_first().ok_or(parse_error_fmt!("Statement::parse: expected ';', but found nothing"))?;
                    Ok((Statement::Expression(expr), tree))
                }
            }
        }
    }
}


fn first_token(tree: &[BTI])->Option<TokenData>{
    tree.first().and_then(|x| {
        match x{
            &BTI::Tree(ref tree) => tree.brackets.clone().map(|a|a.0),
            &BTI::Token(ref x) => Some(x.clone())
        }
    })
}


impl ParseFromBracketTree for Condition{
    fn parse(tree: &[BTI])->Result<(Self, &[BTI]), ParseError>{
        Self::parse_basic(tree).map(|(mut x, tree)|{
            x.shrink();
            (x, tree)
        })
    }
}

impl Condition{
    fn shrink(&mut self){
        let mut e = self.else_block.take();
        if let Some(ref mut e) = e{
            if e.statements.len() == 1{
                if let Statement::Condition(ref mut x) = e.statements[0]{
                    self.ifs.append(&mut x.ifs);
                    self.else_block = x.else_block.take();
                    return;
                }
            }
        }
        self.else_block = e;
    }

    fn parse_basic(tree: &[BTI])->Result<(Self, &[BTI]), ParseError>{
        // condition consist of 
        // name(if) expr(*) (block(*)|statement(*)) [name(else) (block(*)|statement(*))]
        let (if_token, tree) = tree.split_first()
            .ok_or(parse_error_fmt!("Condition::parse: Expected 'if' keyword, but found nothing"))?;
        expect_keyword(if_token, &["if"], "Condition::parse")?;
        let (cond, tree) = tree.split_first()
            .ok_or(parse_error_fmt!("Condition::parse: Expected condition expression, but found nothing"))?;
        if let &BTI::Tree(ref cond) = cond{
            let expr = Expression::parse_from_slice(&cond.nodes)?;
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
        }else{
            Err(parse_error_fmt!("Condition::parse: Expected ('condition'), but {:?} found.", cond))
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
            let (_semicolon, ls) = ls.split_first()
                .ok_or(parse_error_fmt!("ForLoop::parse: Expected ';' but found nothing"))?;
            // parse increment
            let (condition, ls) = Expression::parse(ls)?;
            let (_semicolon, ls) = ls.split_first()
                .ok_or(parse_error_fmt!("ForLoop::parse: Expected ';' but found nothing"))?;
            let (increment, ls) = Expression::parse(ls)?;
            if ls.is_empty(){
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
                assignments.push(assignment);
                match Operator::parse(tr){
                    Ok((ref op, ref tr)) if op.name == "," => {
                        tree = tr;
                    },
                    _ => {
                        tree = tr;
                        break;
                    }
                }
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
    let (block, tree) = if let Ok((statement, tree)) = statement_wrapped{
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
        let to = tree.iter()
            .enumerate()
            .filter(|&(_, a)| test_for_operator(a, stop_operator) || test_for_operator(a, ";"))
            .next();
        let (expr_tree, tree) = if let Some(bt) = to{
            tree.split_at(bt.0)
        }else{
            tree.split_at(tree.len())
        };
        // parse expression
        let expr = Self::parse_from_slice(expr_tree)?;
        Ok((expr, tree))
    }

    fn parse_from_slice(tree: &[BTI])->Result<Self, ParseError>{
        if tree.is_empty(){
            Err(parse_error_fmt!("Expression::parse_from_slice: trying to parse empty expression"))
        }else{
            Self::parse_constant(tree)
                .or_else(|_| Self::parse_name(tree))
                .or_else(|_| Self::parse_function_call(tree))
                .or_else(|_| Self::parse_new(tree))
                .or_else(|_| Self::parse_operators(tree))
                .or_else(|_| Self::parse_index(tree))
        }
    }

    fn parse_new(tree: &[BTI])->Result<Self, ParseError>{
        let (new, tree) = String::parse(tree)?;
        if new == "new"{
            let (type_, tree) = TypeName::parse(tree)?;
            match tree{
                &[BTI::Tree(ref tree)] if check_tree_brackets(tree, "[", "]") => {
                    let expr = Self::parse_from_slice(&tree.nodes)?;
                    Ok(Expression::New(Box::new((type_, expr))))
                },
                _ => {
                    Err(parse_error_fmt!("Expression::parse_new: expected [..] but {:?} found", tree))
                }
            }
        }else{
            Err(parse_error_fmt!("Expression::parse_new: expected 'new' but {:?} found", new))
        }
    }

    fn parse_index(tree: &[BTI])->Result<Self, ParseError>{
        if let Some((&BTI::Tree(ref t), from)) = tree.split_last(){
            if check_tree_brackets(t, "[", "]"){
                let from = Expression::parse_from_slice(from).add_where(t.brackets.clone().map(|a|a.1))?;
                let index = Expression::parse_from_slice(&t.nodes).add_where(t.brackets.clone().map(|a|a.1))?;
                Ok(Expression::Index(Box::new((from, index))))
            }else{
                Err(parse_error_fmt!("Expression::parse_new: expected [..] but {:?} found", t))
            }
        }else{
            Err(parse_error_fmt!("Expression::parse_new: expected [..] but nothing found"))
        }
    }

    fn parse_constant(tree: &[BTI])->Result<Self, ParseError>{
        if tree.len() == 1{
            if let BTI::Token(TokenData{token: Token::Value(ref value), ..}) = tree[0]{
                Ok(Expression::Constant(value.clone()))
            }else{
                Err(parse_error_fmt!("Expression::parse_constant: Expected constant, found {:?}", tree[0]))
            }
        }else{
            Err(parse_error_fmt!("Expression::parse_constant: Expected constant, found {:?}", tree))
        }
    }

    fn parse_name(tree: &[BTI])->Result<Self, ParseError>{
        if tree.len() == 1{
            if let Some(name) = get_name(&tree[0]){
                Ok(Expression::Variable(name))
            }else{
                Err(parse_error_fmt!("Expression::parse_name: Expected name, found {:?}", tree[0]))
            }
        }else{
            Err(parse_error_fmt!("Expression::parse_name: Expected name, found {:?}", tree))
        }
    }

    fn parse_function_call(tree: &[BTI])->Result<Self, ParseError>{
        let (name, tree) = String::parse(tree)?;
        match tree{
            &[BTI::Tree(ref inner)] => {
                if check_tree_brackets(inner, "(", ")"){
                    let arguments = Self::parse_call_arguments(&inner.nodes)?;
                    Ok(Expression::FuncCall(
                        Box::new(FuncCall{name, arguments})
                    ))
                }else{
                    Err(parse_error_fmt!("Expression::parse_function_call: Expected function call, but args wrapped in {:?}", inner.brackets))
                }
            },
            _ => {
                Err(parse_error_fmt!("Expression::parse_function_call: Expected argument list, found {:?}", tree))
            }
        }
    }

    fn parse_call_arguments(tree: &[BTI])->Result<Vec<Expression>, ParseError>{
        let mut args = vec![];
        let mut tree = tree;
        if tree.len() == 0{
            Ok(args)
        }else{
            loop{
                let (arg, tr) = Expression::parse_until(tree, ",")?;
                args.push(arg);
                if tr.len() == 0{
                    break;
                }
                if let Some((_coma, tr)) = tr.split_first(){
                    tree = tr;
                }else{
                    return Err(parse_error_fmt!("Expression::parse_call_arguments: Expected ',', found nohting"))
                }
            }
            Ok(args)
        }
    }

    fn parse_operators(tree: &[BTI])->Result<Self, ParseError>{
        let comparator = parse_operators::PrecedenceLevels::new();
        let op_tree = parse_operators::NodeBuilder::new(
            &comparator, 
            Self::can_be_syffix
        ).build_tree(tree);
        if let parse_operators::Node::Leaf(x) = op_tree{
            Err(parse_error_fmt!("Expression::parse_operators: expected complex expression, but {:?} found", x))
        }else{
            Self::operator_tree_to_expression(&op_tree)
        }
    }

    fn can_be_syffix(operator: &str)->bool{
        ["++", "--"].contains(&operator)
    }

    fn operator_tree_to_expression<'a>(op_tree: &parse_operators::Node<'a>)->Result<Self, ParseError>{
        match op_tree{
            &parse_operators::Node::Binary(ref bin) => {
                let op = bin.operator.op.clone();
                let left = Self::operator_tree_to_expression(&bin.left)?;
                let right = Self::operator_tree_to_expression(&bin.right)?;
                Ok(Expression::BinOp(Box::new(BinOp{op, left, right})))
            },
            &parse_operators::Node::Prefix(ref bin) => {
                let op = bin.operator.op.clone();
                let right = Self::operator_tree_to_expression(&bin.node)?;
                Ok(Expression::PrefixOp(Box::new(PrefixOp{op, right})))
            },
            &parse_operators::Node::Syffix(ref bin) => {
                let op = bin.operator.op.clone();
                let left = Self::operator_tree_to_expression(&bin.node)?;
                Ok(Expression::SyffixOp(Box::new(SyffixOp{op, left})))
            },
            &parse_operators::Node::Leaf(tree) => {
                match tree{
                    &[BTI::Tree(ref tree)] if check_tree_brackets(&tree, "(", ")")  => {
                        Expression::parse_from_slice(&tree.nodes)
                    }
                    _ => Expression::parse_from_slice(tree)
                }
                
            }
        }
    }
}


fn check_tree_brackets(tree: &BT, left: &str, right: &str)->bool{
    // then brackets not specified, assumed that they correct
    match tree.brackets{
        Some((ref l, ref r)) => {
            match (&l.token, &r.token){
                (&Token::Bracket(ref l), &Token::Bracket(ref r)) => {
                    l == left && r == right
                },
                _ => {
                    panic!("check_tree_brackets: Incorrect bracket tokens");
                }
            }
        },
        _ => true
    }
}


#[cfg(test)]
mod tests{
    use std::fmt::Debug;
    use super::*;
    use cata::parser::brackets::{
        BracketTree, 
        BracketTreeItem
    };

    #[test]
    fn test_block_parse(){

    }

    #[test]
    fn test_name_parse(){
        // test for success
        let nodes = bracket_tree!(
            to[Name, "x"]
            to[Operator, "*"]
            br[
                to[Value, "11"]
            ]
        );

        let (s, nodes) = String::parse(&nodes.nodes).unwrap();
        assert_eq!(s, "x");
        assert_eq!(nodes.len(), 2);

        // test for fail on empty tree
        let nodes = bracket_tree!(
        );

        let s_wrapped = String::parse(&nodes.nodes);
        match s_wrapped{
            Err(ref e) if e.msg.contains("String::parse") => {},
            _ => {
                panic!("Expected String::parse error, but get {:?}", s_wrapped);
            }
        }

        // test for fail on unappropriate token
        let nodes = bracket_tree!(
            to[Operator, "*"]
            br[
                to[Value, "11"]
            ]
        );
        let s_wrapped = String::parse(&nodes.nodes);
        test_for_error(s_wrapped, "String::parse");

        let nodes = bracket_tree!(
            br[
                to[Value, "11"]
            ]
            to[Operator, "*"]
        );
        let s_wrapped = String::parse(&nodes.nodes);
        test_for_error(s_wrapped, "String::parse");
    }

    #[test]
    fn test_typename_parse(){
        // test for success
        let tree = &(bracket_tree!(
            to[Name, "int"]
            to[Operator, "*"]
            to[Operator, "***"]
            to[Operator, "**"]
            to[Name, "x"]
            to[Operator, "="]
            to[Name, "y"]
        ).nodes);
        let (type_, tree) = TypeName::parse(tree).unwrap();
        assert_eq!(type_.base, "int");
        assert_eq!(type_.pointer_count, 6);
        assert_eq!(tree.len(), 3);

        let tree = &(bracket_tree!(
            to[Name, "int"]
            to[Operator, "*"]
            to[Operator, "***"]
            to[Operator, "**"]
        ).nodes);
        let (type_, tree) = TypeName::parse(tree).unwrap();
        assert_eq!(type_.base, "int");
        assert_eq!(type_.pointer_count, 6);
        assert_eq!(tree.len(), 0);
        
        let tree = &(bracket_tree!(
            to[Name, "int"]
        ).nodes);
        let (type_, tree) = TypeName::parse(tree).unwrap();
        assert_eq!(type_.base, "int");
        assert_eq!(type_.pointer_count, 0);
        assert_eq!(tree.len(), 0);

        let tree = &(bracket_tree!(
            to[Name, "int"]
            to[Name, "x"]
        ).nodes);
        let (type_, tree) = TypeName::parse(tree).unwrap();
        assert_eq!(type_.base, "int");
        assert_eq!(type_.pointer_count, 0);
        assert_eq!(tree.len(), 1);

        // test for fail on empty tree
        let tree = &(bracket_tree!().nodes);
        let type_wrapped = TypeName::parse(tree);
        test_for_error(type_wrapped, "String::parse");

        // test for fail on unapropriate token
        let tree = &(bracket_tree!(to[Operator, "+"]).nodes);
        let type_wrapped = TypeName::parse(tree);
        test_for_error(type_wrapped, "String::parse");
    }

    #[test]
    fn test_expression_parse_constant(){
        // success
        let tree = &(bracket_tree!(
            to[Value, "some string"]
        ).nodes);
        let expr = Expression::parse_constant(tree).unwrap();
        match expr{
            Expression::Constant(ref value) if value == "some string" => {},
            _ => {panic!("Expected Expression::Constant, but got {:?}", expr);}
        }

        // fail because of extra tokens
        let tree = &(bracket_tree!(
            to[Value, "1"]
            to[Value, "2"]
        ).nodes);
        let expr_wrapped = Expression::parse_constant(tree);
        test_for_error(expr_wrapped, "Expression::parse_constant");

        // fail because of empty tree
        let tree = &(bracket_tree!(
        ).nodes);
        let expr_wrapped = Expression::parse_constant(tree);
        test_for_error(expr_wrapped, "Expression::parse_constant");
        
        // fail because of wrong token
        let tree = &(bracket_tree!(
            to[Name, "x"]
        ).nodes);
        let expr_wrapped = Expression::parse_constant(tree);
        test_for_error(expr_wrapped, "Expression::parse_constant");
    }

    #[test]
    fn test_expression_parse_name(){
        // success
        let tree = &(bracket_tree!(
            to[Name, "some string"]
        ).nodes);
        let expr = Expression::parse_name(tree).unwrap();
        match expr{
            Expression::Variable(ref value) if value == "some string" => {},
            _ => {panic!("Expected Expression::Variable, but got {:?}", expr);}
        }

        // fail because of extra tokens
        let tree = &(bracket_tree!(
            to[Name, "1"]
            to[Name, "2"]
        ).nodes);
        let expr_wrapped = Expression::parse_name(tree);
        test_for_error(expr_wrapped, "Expression::parse_name");

        // fail because of empty tree
        let tree = &(bracket_tree!(
        ).nodes);
        let expr_wrapped = Expression::parse_name(tree);
        test_for_error(expr_wrapped, "Expression::parse_name");
        
        // fail because of wrong token
        let tree = &(bracket_tree!(
            to[Value, "x"]
        ).nodes);
        let expr_wrapped = Expression::parse_name(tree);
        test_for_error(expr_wrapped, "Expression::parse_name");
    }

    #[test]
    fn test_expression_parse_call_arguments(){
        // success
        let tree = &(bracket_tree!(
            to[Name, "x"]
            to[Operator, ","]
            to[Name, "y"]
        ).nodes);
        let call = Expression::parse_call_arguments(tree).unwrap();
        match call.as_slice(){
            &[
                Expression::Variable(ref x), 
                Expression::Variable(ref y)
            ] if x == "x" && y == "y" => {},
            _ => {
                panic!("Unexpected result {:?}", call);
            }
        }
        // zero arguments
        let tree = &(bracket_tree!(
        ).nodes);
        let call = Expression::parse_call_arguments(tree).unwrap();
        match call.as_slice(){
            &[] => {},
            _ => {
                panic!("Unexpected result {:?}", call);
            }
        }

        // unexpected ,
        let tree = &(bracket_tree!(
            to[Name, "x"]
            to[Operator, ","]
            to[Name, "y"]
            to[Operator, ","]
        ).nodes);
        let call = Expression::parse_call_arguments(tree);
        test_for_error(call, "Expression::parse_from_slice");
    }

    #[test]
    fn test_expression_parse_function_call(){
        // success
        let tree = &(bracket_tree!(
            to[Name, "fn"]
            br(
                to[Name, "x"]
            )
        ).nodes);
        let fn_call = Expression::parse_function_call(tree).unwrap();
        match fn_call{
            Expression::FuncCall(box FuncCall{ref name, ref arguments}) => {
                assert_eq!(name, "fn");
                assert_eq!(arguments.as_slice(), &[Expression::Variable("x".to_string())]);
            },
            _ => {
                panic!("Unexpected result: {:?}", fn_call);
            }
        }
        // fail because of wrong first token
        let tree = &(bracket_tree!(
            to[Value, "fn"]
            br(
                to[Name, "x"]
            )
        ).nodes);
        let call_wrapped = Expression::parse_function_call(tree);
        test_for_error(call_wrapped, "String::parse");

        // fail because of wrong second token tree
        let tree = &(bracket_tree!(
            to[Name, "fn"]
            to[Name, "rr"]
            br(
                to[Name, "x"]
            )
        ).nodes);
        let call_wrapped = Expression::parse_function_call(tree);
        test_for_error(call_wrapped, "Expression::parse_function_call");
        
        let tree = &(bracket_tree!(
            to[Name, "fn"]
            br(
                to[Name, "x"]
                to[Operator, ","]
            )
        ).nodes);
        let call_wrapped = Expression::parse_function_call(tree);
        test_for_error(call_wrapped, "Expression::parse_from_slice");

        // fail because of wrong bracket type
        let tree = &(bracket_tree!(
            to[Name, "fn"]
            br{
                to[Name, "x"]
            }
        ).nodes);
        let call_wrapped = Expression::parse_function_call(tree);
        test_for_error(call_wrapped, "Expression::parse_function_call");
    }

    #[test]
    fn test_expression_parse_operators(){
        // success
        let tree = &(bracket_tree!(
            to[Name, "x"]
            to[Operator, "++"]
            to[Operator, "+"]
            to[Operator, "--"]
            to[Name, "y"]
        ).nodes);
        let expr = Expression::parse_operators(tree).unwrap();
        match expr{
            Expression::BinOp(box BinOp{
                ref op,
                left: Expression::SyffixOp(box SyffixOp{op: ref s_op, left: Expression::Variable(ref x)}),
                right: Expression::PrefixOp(box PrefixOp{op: ref p_op, right: Expression::Variable(ref y)})
            }) => {
                assert_eq!(op, "+");
                assert_eq!(s_op, "++");
                assert_eq!(x, "x");
                assert_eq!(p_op, "--");
                assert_eq!(y, "y");
            },
            _ => {
                panic!("Unxepected result: {:?}", expr)
            }
        }
        // success inner
        let tree = &(bracket_tree!(
            to[Name, "x"]
            to[Operator, "*"]
            br(
                to[Name, "y"]
                to[Operator, "+"]
                to[Name, "z"]
            )
        ).nodes);
        let expr = Expression::parse_operators(tree).unwrap();
        match expr{
            Expression::BinOp(box BinOp{
                ref op,
                left: Expression::Variable(ref x),
                right: Expression::BinOp(box BinOp{
                    op: ref op2,
                    left: Expression::Variable(ref y),
                    right: Expression::Variable(ref z)
                })
            }) => {
                assert_eq!(op, "*");
                assert_eq!(x, "x");
                assert_eq!(y, "y");
                assert_eq!(op2, "+");
                assert_eq!(z, "z");

            },
            _ => {
                panic!("Unxepected result: {:?}", expr)
            }
        }
        // success new
        let tree = &(bracket_tree!(
            to[Name, "new"]
            to[Name, "int"]
            to[Operator, "**"]
            to[Operator, "*"]
            br[
                to[Value, "10"]
            ]
            to[Operator, ";"]
        ).nodes);
        let (expr, tree) = Expression::parse(tree).unwrap();
        match expr{
            Expression::New(box (
                ref type_, 
                Expression::Constant(ref s)
            )) => {
                assert_eq!(s, "10");
                assert_eq!(type_.base, "int");
                assert_eq!(type_.pointer_count, 3);
            },
            _ => {
                panic!("Unxepected result: {:?}", expr)
            }
        }
        assert_eq!(tree.len(), 1);
        // success index
        let tree = &(bracket_tree!(
            to[Name, "x"]
            br[
                to[Value, "10"]
            ]
            br[
                to[Value, "12"]
            ]
            to[Operator, ";"]
        ).nodes);
        let (expr, tree) = Expression::parse(tree).unwrap();
        assert_eq!(tree.len(), 1);
        match expr{
            Expression::Index(box (
                Expression::Index(box (
                    Expression::Variable(ref x),
                    Expression::Constant(ref ten)
                )),
                Expression::Constant(ref twelve)
            )) => {
                assert_eq!(x, "x");
                assert_eq!(ten, "10");
                assert_eq!(twelve, "12");
            },
            _ => {
                panic!("Unxepected result: {:?}", expr)
            }
        }
        let tree = &(bracket_tree!(
            to[Name, "x"]
            to[Operator, "+"]
            to[Name, "y"]
            br[
                to[Value, "10"]
            ]
            to[Operator, ";"]
        ).nodes);
        let (expr, tree) = Expression::parse(tree).unwrap();
        assert_eq!(tree.len(), 1);
        match expr{
            Expression::BinOp(box BinOp{
                ref op,
                left: Expression::Variable(ref x),
                right: Expression::Index(box (
                    Expression::Variable(ref y),
                    Expression::Constant(ref ten)
                )),
            }) => {
                assert_eq!(op, "+");
                assert_eq!(x, "x");
                assert_eq!(y, "y");
                assert_eq!(ten, "10");
            },
            _ => {
                panic!("Unxepected result: {:?}", expr)
            }
        }
    }

    #[test]
    fn test_expression_parse_before_separator(){
        let tree = &(bracket_tree!(
            to[Name, "x"]
            to[Operator, ";"]
            to[Name, "y"]
        ).nodes);
        let (expr, tree) = Expression::parse(tree).unwrap();
        assert_eq!(expr, Expression::Variable("x".to_string()));
        assert_eq!(tree.len(), 2);
    }

    #[test]
    fn test_vardef_parse(){
        let tree = &(bracket_tree!(
            to[Name, "int"]
            to[Name, "x"]
            to[Operator, "="]
            to[Name, "y"]
            to[Operator, ","]
            to[Name, "z"]
            to[Operator, "="]
            to[Value, "2"]
            to[Operator, ";"]
        ).nodes);

        let (var_def, tree) = VarDef::parse(tree).unwrap();
        assert_eq!(tree.len(), 1);
        assert_eq!(var_def, VarDef{
            type_: TypeName{
                base: "int".to_string(),
                pointer_count: 0
            },
            assignments: vec![
                ("x".to_string(), Expression::Variable("y".to_string())),
                ("z".to_string(), Expression::Constant("2".to_string()))
            ]
        });
    }

    #[test]
    fn test_condition_parse(){
        // success
        let tree = &(bracket_tree!(
            to[Name, "if"]
            br(
                to[Name, "x"]
            )
            to[Name, "y"]
            to[Operator, ";"]
        ).nodes);

        let (cond, tree) = Condition::parse(tree).unwrap();
        assert_eq!(cond, Condition{
            ifs: vec![(
                Expression::Variable("x".to_string()), 
                Block{statements: vec![Statement::Expression(Expression::Variable("y".to_string()))]}
            )],
            else_block: None
        });
        assert_eq!(tree.len(), 0);
        
        // success embedded
        let tree = &(bracket_tree!(
            to[Name, "if"]
            br(
                to[Name, "x"]
            )
            to[Name, "if"]
            br(
                to[Name, "x"]
            )
            to[Name, "y"]
            to[Operator, ";"]
        ).nodes);

        let (cond, tree) = Condition::parse(tree).unwrap();
        assert_eq!(cond, Condition{
            ifs: vec![(
                Expression::Variable("x".to_string()), 
                Block{statements: vec![Statement::Condition(
                    Condition{
                        ifs: vec![(
                            Expression::Variable("x".to_string()), 
                            Block{statements: vec![Statement::Expression(Expression::Variable("y".to_string()))]}
                        )],
                        else_block: None
                    }
                )]}
            )],
            else_block: None
        });
        assert_eq!(tree.len(), 0);

        // success else
        let tree = &(bracket_tree!(
            to[Name, "if"]
            br(
                to[Name, "x"]
            )
            to[Name, "y"]
            to[Operator, ";"]
            to[Name, "else"]
            to[Name, "z"]
            to[Operator, ";"]
        ).nodes);
        
        let (cond, tree) = Condition::parse(tree).unwrap();
        assert_eq!(cond, Condition{
            ifs: vec![(
                Expression::Variable("x".to_string()), 
                Block{statements: vec![Statement::Expression(Expression::Variable("y".to_string()))]}
            )],
            else_block: Some(Block{statements: vec![Statement::Expression(Expression::Variable("z".to_string()))]})
        });
        assert_eq!(tree.len(), 0);
    }

    #[test]
    fn test_forloop_parse(){
        let tree = &(bracket_tree!(
            to[Name, "for"]
            br(
                to[Name, "int"]
                to[Name, "x"]
                to[Operator, "="]
                to[Name, "y"]
                to[Operator, ";"]
                to[Name, "x"]
                to[Operator, ";"]
                to[Operator, "--"]
                to[Name, "x"]
            )
            to[Name, "y"]
            to[Operator, ";"]
        ).nodes);
        let (forloop, tree) = ForLoop::parse(tree).unwrap();
        assert_eq!(forloop, ForLoop{
            init: ForLoopInit::VarDef(VarDef{
                type_: TypeName{
                    base: "int".to_string(),
                    pointer_count: 0
                },
                assignments: vec![
                    ("x".to_string(), Expression::Variable("y".to_string()))
                ]
            }),
            step: Expression::PrefixOp(Box::new(PrefixOp{
                op: "--".to_string(),
                right: Expression::Variable("x".to_string())
            })),
            cond: Expression::Variable("x".to_string()),
            block: Block{
                statements: vec![
                    Statement::Expression(Expression::Variable("y".to_string()))
                ]
            }
        });
        assert_eq!(tree.len(), 0);
    }

    fn test_for_error<T: Debug>(res: Result<T, ParseError>, expected: &str){
        match res{
            Err(ref e) if e.msg.contains(expected) => {},
            _ => {panic!("Expected error {} but get {:?}", expected, res)}
        }
    }
}
