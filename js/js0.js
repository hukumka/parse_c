var block = new FunctionDef(new Type("int", 0), "func", [{type: new Type("int", 0), name: "x"},], new CppBody([new StatementVarDef(new Type("float", 0), [{name: "res", expression: new ExpressionConstant("0", new Type("int", 0))},]),new StatementCondition([{condition: new ExpressionBinOp(">", new ExpressionConstant("3", new Type("int", 0)), new ExpressionVariable("x")), block: new CppBody([new StatementExpression(new ExpressionBinOp("=", new ExpressionVariable("res"), new ExpressionConstant("3", new Type("int", 0)))),new StatementExpression(new ExpressionBinOp("+=", new ExpressionVariable("res"), new ExpressionFuncCall("sqrt", [new ExpressionBinOp("/", new ExpressionVariable("x"), new ExpressionConstant("2", new Type("int", 0))), ]))),])},{condition: new ExpressionBinOp("<", new ExpressionVariable("i"), new ExpressionVariable("x")), block: new CppBody([new StatementExpression(new ExpressionBinOp("-=", new ExpressionVariable("res"), new ExpressionFuncCall("sqrt", [new ExpressionBinOp("/", new ExpressionVariable("x"), new ExpressionConstant("2", new Type("int", 0))), ]))),])},], new CppBody([new StatementExpression(new ExpressionBinOp("-=", new ExpressionVariable("res"), new ExpressionBinOp("/", new ExpressionVariable("x"), new ExpressionConstant("2", new Type("int", 0))))),])),new StatementReturn(new ExpressionVariable("res")),]));