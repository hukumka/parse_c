#![feature(slice_patterns)]
#![feature(advanced_slice_patterns)]
#![feature(str_checked_slicing)]
#![feature(box_patterns)]


#[macro_use]
extern crate cata;

mod parse;
mod build;

use std::fs::File;
use std::io;
use std::fmt;
use std::io::prelude::*;

use cata::lexer::tokenize;
use cata::parser::brackets::parse_brackets;

use parse::{FunctionDef, Statement, ParseFromBracketTree};
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
        if let Ok((func_def, tr)) = res{
            write!(formatter, "functions[\"{}\"] = {};", func_def.name, Formatable::new(&|f| func_def.write(f)))?;
            tree = tr;
        }else{
            let (stat, tr) = Statement::parse(tree).unwrap();
            write!(formatter, "globals[globals.length] = {};", Formatable::new(&|f| stat.write(f)))?;
            tree = tr;
        }
    }
    Ok(())
}

