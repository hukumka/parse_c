use std::fmt;


use parse::{
    FunctionDef,
    TypeName,
    Block,
    Statement,
    Expression,
    Condition,
    ForLoopInit,
    VarDef
};


// Formatable
pub struct Formatable<'a, Func: 'a + Fn(&mut fmt::Formatter)->fmt::Result>{
    func: &'a Func
}

impl<'a, Func: 'a + Fn(&mut fmt::Formatter)->fmt::Result> fmt::Display for Formatable<'a, Func>{
    fn fmt(&self, f: &mut fmt::Formatter)->fmt::Result{
        (self.func)(f)
    }
}

impl<'a, Func: 'a + Fn(&mut fmt::Formatter)->fmt::Result> Formatable<'a, Func>{
    pub fn new(func: &'a Func)->Self{
        Self{func}
    }
}


// Build Js
pub trait BuildJs{
    fn write(&self, fmt: &mut fmt::Formatter)->fmt::Result;
}


impl BuildJs for FunctionDef{
    fn write(&self, fmt: &mut fmt::Formatter)->fmt::Result{
        write!(fmt, 
            "new FunctionDef({}, \"{}\", {}, {})",
            Formatable::new(&|f| self.ret_type.write(f)),
            self.name,
            Formatable::new(&|f| self.format_arguments(f)),
            Formatable::new(&|f| self.block.write(f))
        )
    }
}
    
impl FunctionDef{
    fn format_arguments(&self, fmt: &mut fmt::Formatter)->fmt::Result{
        write!(fmt, "[")?;
        for &(ref type_, ref name) in &self.arguments{
            write!(fmt, "{{type: {}, name: \"{}\"}},",
                Formatable::new(&|f| type_.write(f)),
                name
            )?;
        }
        write!(fmt, "]")
    }
}


impl BuildJs for TypeName{
    fn write(&self, fmt: &mut fmt::Formatter)->fmt::Result{
        write!(fmt, "new Type(\"{}\", {})", self.base, self.pointer_count)

    }
}


impl BuildJs for Block{
    fn write(&self, fmt: &mut fmt::Formatter)->fmt::Result{
        write!(fmt, "new CppBody([")?;
        for statement in &self.statements{
            write!(fmt, "{},", Formatable::new(&|f| statement.write(f)))?;
        }
        write!(fmt, "])")
    }
}


impl BuildJs for Statement{
    fn write(&self, fmt: &mut fmt::Formatter)->fmt::Result{
        match *self{
            Statement::Expression(ref e) => {
                write!(fmt, "new StatementExpression({})", 
                    Formatable::new(&|f| e.write(f))
                )
            },
            Statement::Condition(Condition{ref ifs, else_block: Some(ref else_)}) => {
                write!(fmt, "new StatementCondition({}, {})",
                    Formatable::new(&|f| format_ifs(f, ifs)),
                    Formatable::new(&|f| else_.write(f))
                )
            },
            Statement::Condition(Condition{ref ifs, else_block: None}) => {
                write!(fmt, "new StatementCondition({})",
                    Formatable::new(&|f| format_ifs(f, ifs))
                )
            },
            Statement::VarDef(ref vd) => {
                vd.write(fmt)
            },
            Statement::ForLoop(ref for_loop) => {
                write!(fmt, "new StatementForLoop({}, {}, {}, {})",
                    Formatable::new(&|f| for_loop.init.write(f)),
                    Formatable::new(&|f| for_loop.step.write(f)),
                    Formatable::new(&|f| for_loop.cond.write(f)),
                    Formatable::new(&|f| for_loop.block.write(f))
                )
            },
            Statement::Return(ref expr) => {
                write!(fmt, "new StatementReturn({})",
                    Formatable::new(&|f| expr.write(f))
                )
            }
        }
    }
}

impl BuildJs for VarDef{
    fn write(&self, fmt: &mut fmt::Formatter)->fmt::Result{
        write!(fmt, "new StatementVarDef({}, {})",
            Formatable::new(&|f| self.type_.write(f)),
            Formatable::new(&|f| format_assignments(f, &self.assignments)),
        )
    }
}

fn format_ifs(fmt: &mut fmt::Formatter, ifs: &[(Expression, Block)])->fmt::Result{
    write!(fmt, "[")?;
    for &(ref expr, ref block) in ifs{
    write!(fmt, "{{condition: {}, block: {}}},", 
            Formatable::new(&|f| expr.write(f)),
            Formatable::new(&|f| block.write(f))
        )?;
    }
    write!(fmt, "]")
}


fn format_assignments(fmt: &mut fmt::Formatter, assignments: &[(String, Expression)])->fmt::Result{
    write!(fmt, "[")?;
    for &(ref name, ref expr) in assignments{
        write!(fmt, "{{name: \"{}\", expression: {}}},",
            name,
            Formatable::new(&|f| expr.write(f))
        )?;
    }
    write!(fmt, "]")
}


impl BuildJs for ForLoopInit{
    fn write(&self, fmt: &mut fmt::Formatter)->fmt::Result{
        match self{
            &ForLoopInit::Expression(ref e) => {
                e.write(fmt)
            },
            &ForLoopInit::VarDef(ref v) => {
                v.write(fmt)
            }
        }
    }
}


impl BuildJs for Expression{
    fn write(&self, fmt: &mut fmt::Formatter)->fmt::Result{
        match *self{
            Expression::FuncCall(box ref func_call) => {
                write!(fmt, "new ExpressionFuncCall(\"{}\", [", func_call.name)?;
                for e in &func_call.arguments{
                    write!(fmt, "{}, ", Formatable::new(&|f| e.write(f)))?;
                }
                write!(fmt, "])")
            },
            Expression::BinOp(box ref bin_op) => {
                write!(fmt, "new ExpressionBinOp(\"{}\", {}, {})",
                    bin_op.op,
                    Formatable::new(&|f| bin_op.left.write(f)),
                    Formatable::new(&|f| bin_op.right.write(f)),
                )
            },
            Expression::PrefixOp(box ref op) => {
                write!(fmt, "new ExpressionPrefixOp(\"{}\", {})",
                    op.op,
                    Formatable::new(&|f| op.right.write(f))
                )
            },
            Expression::SyffixOp(box ref op) => {
                write!(fmt, "new ExpressionPrefixOp(\"{}\", {})",
                    op.op,
                    Formatable::new(&|f| op.left.write(f))
                )
            },
            Expression::Variable(ref s) => {
                write!(fmt, "new ExpressionVariable(\"{}\")", s)
            },
            Expression::Constant(ref s) => {
                write!(fmt, "new ExpressionConstant(\"{}\", {})", s, get_constant_type(s))
            },
            Expression::New(box (ref type_, ref expr)) => {
                write!(fmt, "new ExpressionNew({}, {})",
                    Formatable::new(&|f| type_.write(f)),
                    Formatable::new(&|f| expr.write(f)),
                )
            },
            Expression::Index(box (ref from, ref index)) => {
                write!(fmt, "new ExpressionIndex({}, {})",
                    Formatable::new(&|f| from.write(f)),
                    Formatable::new(&|f| index.write(f)),
                )
            }

        }
    }
}

fn get_constant_type(s: &str)->String{
    if s.chars().next() == Some('"'){
        "new Type(\"char\", 1)"
    }else if s.contains("."){
        "new Type(\"float\", 0)"
    }else{
        "new Type(\"int\", 0)"
    }.to_string()
}
