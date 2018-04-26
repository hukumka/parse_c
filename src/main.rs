#![feature(str_checked_slicing)]
#![feature(box_patterns)]


#[macro_use]
extern crate cata;

#[macro_use]
mod parse;
mod build;

use std::fmt::Write;

use std::fs::File;
use std::io;
use std::fmt;
use std::io::prelude::*;

use cata::lexer::tokenize;
use cata::parser::brackets::parse_brackets;

use parse::{FunctionDef, Statement, ParseFromBracketTree};
use parse::{ResultBuilder, ParseError};
use build::{BuildJs, Formatable};


fn main() {
    let file_path = "/home/hukumka/src/parse_c/test.cpp";
    let file = File::open(file_path).unwrap();
    let mut bufread = io::BufReader::new(file);
    let mut contents = String::new();
    bufread.read_to_string(&mut contents).unwrap();

    let mut out = String::new();
    generate_js(&contents, &mut out).unwrap();
    println!("{}", out);
}


fn generate_js<T: fmt::Write>(c_code: &str, formatter: &mut T)->fmt::Result{
    // skip includes
    let skip_includes: Vec<_> = c_code.split("\n").filter(|x| !x.starts_with("#")).collect();
    let skip = &skip_includes.join("\n");
    // build tree
    write!(formatter, "functions = [];")?;
    write!(formatter, "globals = [];")?;
    let tokens = tokenize(&skip);
    let tree = parse_brackets(tokens).unwrap();
    let mut tree = tree.nodes.as_slice();
    while !tree.is_empty(){
        let res = FunctionDef::parse(tree);
        match res{
            Ok((func_def, tr)) => {
                write!(formatter, "functions[\"{}\"] = {};", func_def.name, Formatable::new(&|f| func_def.write(f)))?;
                tree = tr;
            },
            Err(e1) => {
                match Statement::parse(tree){
                    Ok((stat, tr)) => {
                        write!(formatter, "globals[globals.length] = {};", Formatable::new(&|f| stat.write(f)))?;
                        tree = tr;
                    },
                    Err(e2) => {
                        if(e1.msg == "Expected FunctionDef"){
                            eprintln!("{}", fancy_parse_error(c_code, e2));
                        }else{
                            eprintln!("{}", fancy_parse_error(c_code, e1));
                        }
                        panic!();
                    }
                }
            }
        }
    }
    Ok(())
}


fn fancy_parse_error(c_code: &str, error: ParseError)->String{
    //let (error, _) = deepest_error(&error);
    let mut error_message = String::new();
    write!(error_message, "{}\n", error.msg).unwrap();
    if let Some(ref where_) = error.where_{
        write!(error_message, "{:width$}\n", "V", width=where_.line_offset).unwrap();
        write!(error_message, "{}\n", c_code.lines().skip(where_.line).next().unwrap()).unwrap();
    }
    if !error.derive_from.is_empty() && !error.msg.starts_with("Expression::"){
        write!(error_message, "Error originates from: <<<\n").unwrap();
        for e in &error.derive_from{
            write!(error_message, "{}", fancy_parse_error(c_code, e.clone())).unwrap();
        }
        write!(error_message, ">>>\n").unwrap();
    }
    error_message
    
}


fn deepest_error(error: &ParseError)->(&ParseError, usize){
    error.derive_from.iter()
        .map(|e| {
            let (e, d) = deepest_error(e);
            (e, d+1)
        })
        .fold((error, 0), |a, b|{
            if a.1 >= b.1{
                a
            }else{
                b
            }
        })
}
