function Namespace(par){
	this.par = par;
	this.data = [];

	this.set = function(name, value){
        if(name in this.data){
			this.data[name].value = value;
        }else{
			this.par.set(name, value);
        }
	}

	this.has = function(name){
		return this.data[name] !== undefined || (this.par !== undefined && this.par.has(name));
	}

	this.get = function(name){
		if(this.data[name] !== undefined){
			return this.data[name]
		}else if(this.par !== undefined){
			return this.par.get(name);
		}else{
			return undefined;
		}
	}
	
	this.define = function(name, type, value){
		this.data[name] = {value: value, type: type, reference: {namespace: this, name: name}};
	}

	this.log = function(){
		for(var key in this.data){
			console.log(this.data[key].type.get_name() + " " + key + " = " + build_representation(this.data[key]));
		}
		if(this.par !== undefined){
			this.par.log();
		}
	}
}

function build_representation(data){
    if(data.value instanceof Array){
        return data.value.map(function(x){return build_representation(x)});
    }else{
        return data.value;
    }
}

function NamespaceHolder(globals){
    if(globals === undefined){
        this.globals = new Namespace();
    }else{
        this.globals = globals;
    }
	this.namespace = new Namespace(this.globals);
    this.function_stack = [];
	this.push = function(){
		this.namespace = new Namespace(this.namespace);
	};
	this.pop = function(){
		this.namespace = this.namespace.par;
	}
	this.log = function(){
		this.namespace.log();
	}
    this.function_call = function(){
        this.function_stack.push(this.namespace);
        this.namespace = new Namespace(this.globals);
    }
    this.function_ret = function(){
        this.namespace = this.function_stack.pop();
    }
}


function CodeLineHolder(){
    this.lines = [];
    this.tab = 0;
    this.current_line = undefined;

    this.create_view = function(){
        this.view = document.createElement("table");
    this.view.setAttribute("class", "code-line-holder");
        for(var i=0; i<this.lines.length; ++i){
            var l = this.lines[i];
            this.view.appendChild(l.view);
        }
    };

    this.highlight = function(line){
        if(this.current_line !== undefined){
            this.current_line.number_view.setAttribute("class", "cpp-line-number");
        }
        line.number_view.setAttribute("class", "cpp-line-number current");
        this.current_line = line;
    }
}


function CodeLine(line_holder, inner){
    this.line_holder = line_holder;
    this.line_id = line_holder.lines.length;
    line_holder.lines[this.line_id] = this;
    this.inner = inner;

    this.create_view = function(){
        var e = document.createElement("tr");
        e.setAttribute("class", "cpp-line");
        var line_num = document.createElement("td");
        this.number_view = line_num;
        line_num.setAttribute("class", "cpp-line-number");
        line_num.appendChild(document.createTextNode(this.line_id+1));
        e.appendChild(line_num);
        for(var i=0; i<this.line_holder.tab; ++i){
            var tab = document.createElement("span");
            tab.setAttribute("class", "cpp-tab");
            e.appendChild(tab);
        }
        e.appendChild(this.inner);
        this.view = e;
        return e;
    }

    this.create_view();

    function make_current(){
        this.view.setAttribute("class", "cpp-line current");
    }

    function make_not_current(){
        this.view.setAttribute("class", "cpp-line");
    }
}


function Type(name, pointer_count){
    this.name = name;
    this.pointer_count = pointer_count;

    this.get_name = function(){
        return this.name + "*".repeat(this.pointer_count);
    }

    this.test = function(str){
        return str == this.name && this.pointer_count == 0;
    }

    this.test_pointer = function(str, depth){
        return str == this.name && this.pointer_count == depth;
    }
    
    this.create_view = function(){
        this.view = document.createElement("span");
        this.view.setAttribute("class", "cpp-type");
        this.view.appendChild(document.createTextNode(this.name + "*".repeat(this.pointer_count) + " "));
        return this.view;
    }
}


function FunctionDef(ret_type, name, args, body){
    this.ret_type = ret_type;
    this.name = name;
    this.args = args;
    this.body = body;

    this.step = function(namespace_holder){
        try{
            return this.body.step(namespace_holder);
        }catch(err){
            if("result" in err){
                this.result = err.result;
                this.body.reset();
                return true;
            }else{
                throw err;
            }
        }
    }

    this.reset = function(){
        this.body.reset();
    }

    this.get = function(namespace_holder, args){
        try{
            namespace_holder.push();
            if(this.args.length != args.length){
                throw "Function '" + this.name + "' require " + this.args.length + " arguments, but only " + args.length + " supplied.";
            }
            for(var i=0; i<args.length; ++i){
                namespace_holder.namespace.define(this.args[i].name, this.args[i].type, args[i].value);
            }
            namespace_holder.log();
            var result = this.body.get(namespace_holder);
            namespace_holder.pop();
        }catch(err){
            if("result" in err){
                return err.result;
            }else{
                throw err;
            }
        }
    }

    this.get_current_line = function(){
        return this.body.get_current_line();
    }

    this.create_lines = function(line_holder){
        var func_def_line = document.createElement("span");
        func_def_line.appendChild(this.ret_type.create_view());
        var func_name = document.createElement("span");
        func_name.setAttribute("class", "cpp-function-name");
        func_name.appendChild(document.createTextNode(this.name));
        func_def_line.appendChild(func_name);
        func_def_line.appendChild(document.createTextNode("("));
        var args = document.createElement("span");
        for(var i=0; i<this.args.length; ++i){
            var type = this.args[i].type;
            var name = this.args[i].name;
            args.appendChild(type.create_view());
            args.appendChild(document.createTextNode(name));
        }
        func_def_line.appendChild(args);
        func_def_line.appendChild(document.createTextNode("){"));
        func_def_line = new CodeLine(line_holder, func_def_line);
        this.start = func_def_line;
        this.body.create_lines(line_holder);
        this.end = new CodeLine(line_holder, document.createTextNode("}"));
    };
}


function CppBody(statements){
    this.statements = statements;

    this.current_line = undefined;
    this.step = function(namespace_holder){
        if(this.current_line === undefined){
            namespace_holder.push();
            this.current_line = 0;
            return false;
        }else{
            if(this.statements[this.current_line].step(namespace_holder)){
                this.current_line++;
            }
            if(this.current_line >= this.statements.length){
                this.current_line = undefined;
                namespace_holder.pop();
                return true;
            }else{
                return false;
            }
        }
    }

    this.get = function(namespace_holder){
        for(var i=0; i<this.statements.length; ++i){
            this.statements[i].get(namespace_holder);
        }
    }

    this.reset = function(){
        this.current_line = undefined;
        for(var i=0; i<this.statements.length; ++i){
            this.statements[i].reset();
        }
    }

    this.get_current_line = function(){
        if(this.current_line === undefined){
            if(this.statements.length != 0){
                return this.statements[0].get_current_line();
            }
            return undefined;
        }else{
            return this.statements[this.current_line].get_current_line();
        }
    }

    this.create_lines = function(line_holder){
        line_holder.tab += 1;
        for(var i=0; i<this.statements.length; ++i){
            this.statements[i].create_lines(line_holder);
        }
        line_holder.tab--;
    }
}


function StatementReturn(expression){
    this.expression = expression;

    this.step = function(namespace_holder){
        throw {result: this.expression.get(namespace_holder)};
    }

    this.get = function(namespace_holder){
        throw {result: this.expression.get(namespace_holder)};
    }

    this.reset = function(){}
    this.get_current_line = function(){return this.line;}

    this.create_lines = function(line_holder){
        var e = document.createElement("span");
        e.appendChild(create_element_cls("span", "return ", "cpp-keyword"));
        e.appendChild(this.expression.create_view(e));
        e.appendChild(document.createTextNode(";"));
        this.line = new CodeLine(line_holder, e);
    }

}


function StatementExpression(expression){
    this.expression = expression;

    this.step = function(namespace_holder){
        this.expression.get(namespace_holder);
        return true;
    };

    this.get = function(namespace_holder){
        return this.expression.get(namespace_holder);
    }

    this.reset = function(){};
    this.get_current_line = function(){
        return this.line;
    }
    
    this.create_lines = function(line_holder){
        var e = document.createElement("span");
        e.setAttribute("class", "cpp-statement");
        e.appendChild(this.expression.create_view());
        e.appendChild(document.createTextNode(";"));
        this.line = new CodeLine(line_holder, e);
    }
}


function ExpressionFuncCall(name, args){
    this.name = name;
    this.args = args;

    this.get = function(namespace_holder){
        var fn = namespace_holder.namespace.get(this.name).value;
        var args = this.args.map(function(x){return x.get(namespace_holder);})
            .map(function(x){return {value: x.value, type: x.type};}); // shallow dereference
        namespace_holder.function_call();
        var res = fn.get(namespace_holder, args);
        namespace_holder.function_ret();
        return res;
    }

    this.create_view = function(){
        this.view = document.createElement("span");
        var func_name = document.createElement("span");
        func_name.setAttribute("class", "cpp-function-name");
        func_name.appendChild(document.createTextNode(this.name));
        this.view.appendChild(func_name);
        this.view.appendChild(document.createTextNode("("));
        for(var i=0; i<this.args.length; ++i){
            this.view.appendChild(this.args[i].create_view());
            if(i < this.args.length - 1){
                this.view.appendChild(document.createTextNode(", "));
            }
        }
        this.view.appendChild(document.createTextNode(")"));
        return this.view;
    }
}


function ExpressionConstant(value, type){
    this.value = value;
    this.type = type

    this.get = function(){
        if(this.type.test("int") ||  this.type.test("float")){
            return {value: Number(this.value), type: this.type};
        }else{
            return {value: this.value, type: this.type};
        }
    }

    this.create_view = function(){
        this.view = document.createElement("span");
        this.view.setAttribute("class", "cpp-constant");
        this.view.appendChild(document.createTextNode(this.value));
        return this.view;
    }
}


function ExpressionVariable(name){
    this.name = name;

    this.get = function(namespace_holder){
        var res = namespace_holder.namespace.get(this.name);
        return res;
    }

    this.create_view = function(){
        this.view = document.createElement("span");
        this.view.setAttribute("class", "cpp-variable");
        this.view.appendChild(document.createTextNode(this.name));
        return this.view;
    }
}


function get_operator_level(op){
    var operators = [
        ["++", "--"],
        ["*", "/", "%"],
        ["+", "-"],
        ["<=", "<", ">=", ">"],
        ["==", "!="],
        ["&&"],
        ["||"],
        ["=", "+=", "*=", "%=", "-=", "/="],
        [","]
    ];
    for(var i=0; i<operators.length; ++i){
        if(operators[i].includes(op)){
            return i;
        }
    }
    return -1;
}


function ExpressionBinOp(op, left, rigth){
	this.op = op;
	this.left = left;
	this.right = rigth;
	
	this.get = function(namespace_holder){
	    var left = this.left.get(namespace_holder);
	    var right = this.right.get(namespace_holder);
	    var type = new Type("int", 0);
	    if(left.type.test("float") || right.type.test("float")){
	        type.name = "float";
	    }
	    if(this.op == "*"){
	        return {value: left.value * right.value, type: type};
	    }else if(this.op == "/"){
	        if(type.test("int")){
	            return {value: Math.floor(left.value / right.value), type: type};
	        }else{
	            return {value: left.value / right.value, type: type};
	        }
	    }else if(this.op == "%"){
	        return {value: left.value % right.value, type: type};
	    }else if(this.op == "+"){
	        return {value: left.value + right.value, type: type};
	    }else if(this.op == "-"){
	        return {value: left.value - right.value, type: type};
	    }else if(this.op == "<="){
	        return {value: left.value <= right.value, type: new Type("bool", 0)};
	    }else if(this.op == "<"){
	        return {value: left.value < right.value, type: new Type("bool", 0)};
	    }else if(this.op == ">="){
	        return {value: left.value >= right.value, type: new Type("bool", 0)};
	    }else if(this.op == ">"){
	        return {value: left.value > right.value, type: new Type("bool", 0)};
	    }else if(this.op == "=="){
	        return {value: left.value == right.value, type: new Type("bool", 0)};
	    }else if(this.op == "!="){
	        return {value: left.value != right.value, type: new Type("bool", 0)};
	    }else if(this.op == "&&"){
	        return {value: left.value && right.value, type: new Type("bool", 0)};
	    }else if(this.op == "||"){
	        return {value: left.value || right.value, type: new Type("bool", 0)};
	    }else if(this.op == "="){
	        var ref = left.reference;
	        if(left.type.test("int")){
	            left.value = Math.floor(right.value);
	        }else{
	            left.value = right.value;
	        }
	        return left;
	    }else if(this.op == "+="){
	        var ref = left.reference;
	        if(left.type.test("int")){
	            left.value = Math.floor(left.value + right.value);
	        }else{
	            left.value = left.value + right.value;
	        }
	        return left;
	    }else if(this.op == "-="){
            console.log(left, this.right);
	        var ref = left.reference;
	        if(left.type.test("int")){
	            left.value = Math.floor(left.value - right.value);
	        }else{
	            left.value = left.value - right.value;
	        }
	        return left;
	    }else if(this.op == "/="){
	        var ref = left.reference;
	        if(left.type.test("int")){
	            left.value = Math.floor(left.value / right.value);
	        }else{
	            left.value = left.value / right.value;
	        }
	        return left;
	    }else if(this.op == "%="){
	        var ref = left.reference;
	        ref.namespace.set(ref.name, left.value - right.value);
	        if(left.type.test("int")){
	            left.value = Math.floor(left.value % right.value);
	        }else{
	            left.value = left.value % right.value;
	        }
	        return left;
	    }else if(this.op == "*="){
	        if(left.type.test("int")){
	            left.value = Math.floor(left.value * right.value);
	        }else{
	            left.value = left.value * right.value;
	        }
	        return left;
	    }else if(this.op == ","){
	        return right;
	    }
	}
	
	this.create_view = function(prev_operator){
	    this.view = document.createElement("span");
	    var need_par = false;
	    if(prev_operator !== undefined){
	        var op_level = get_operator_level(this.op);
	        var prev_level = get_operator_level(prev_operator);
	        if(op_level > prev_level || op_level == -1 || prev_level == -1){
	            need_par = true;
	        }
	    }
	
	    if(need_par){
	        this.view.appendChild(document.createTextNode("("));
	    }
	    this.view.appendChild(this.left.create_view(this.op));
	    this.view.appendChild(document.createTextNode(" " + this.op + " "));
	    this.view.appendChild(this.right.create_view(this.op));
	    if(need_par){
	        this.view.appendChild(document.createTextNode(")"));
	    }
	    return this.view;
	}
}


function ExpressionPrefixOp(op, right, need_par){
	this.op = op;
	this.right = right;
	
	if(need_par === undefined){
	    need_par = false;
	}
	this.need_par = need_par;
	
	this.get = function(namespace_holder){
	    if(this.op == "++"){
	        var val = this.right.get(namespace_holder);
	        val.value += 1;
	        return val;
	    }else if(this.op == "--"){
	        var val = this.right.get(namespace_holder);
	        val.value -= 1;
	        return val;
	    }else if(this.op == "!"){
	        var val = this.right.get(namespace_holder);
	        return {value: !val.value, type: new Type("bool", 0)};
	    }
	}
	
	this.create_view = function(){
	    this.view = document.createElement("span");
	    if(this.need_par){
	        this.view.appendChild(document.createTextNode("("));
	    }
	    this.view.appendChild(document.createTextNode(this.op));
	    this.view.appendChild(this.right.create_view(this.op));
	    if(this.need_par){
	        this.view.appendChild(document.createTextNode(")"));
	    }
	    return this.view;
	}

}


function ExpressionSyffixOp(op, left, need_par){
	this.op = op;
	this.left = left;
	
	if(need_par === undefined){
	    need_par = false;
	}
	this.need_par = need_par;
	
	this.get = function(namespace_holder){
	    if(this.op == "--"){
	        var val = this.left.get(namespace_holder);
	        var res = {value: val.value, type: val.type};
	        val.value -= 1;
	        return res;
	    }if(this.op == "++"){
	        var val = this.left.get(namespace_holder);
	        var res = {value: val.value, type: val.type};
	        val.value -= 1;
	        return res;
	    }
	}
	
	this.create_view = function(){
	    this.view = document.createElement("span");
	    if(this.need_par){
	        this.view.appendChild(document.createTextNode("("));
	    }
	    this.view.appendChild(this.left.create_view(this.op));
	    this.view.appendChild(document.createTextNode(this.op));
	    if(this.need_par){
	        this.view.appendChild(document.createTextNode(")"));
	    }
	    return this.view;
	}
}


function ExpressionNew(type, size){
	this.type = type;
	this.size = size;
	
	this.get = function(namespace_holder){
	    var res = {value: [], type: new Type(this.type.name, this.type.pointer_count+1)};
	    var size = this.size.get(namespace_holder);
	    if(!size.type.test("int")){
	        throw "Size must be integer";
	    }
	    for(var i=0; i<size.value; ++i){
	        res.value[i] = {value: undefined, type: this.type};
	    }
	    return res;
	}
	
	this.create_view = function(){
	    this.view = document.createElement("span");
	    var kw_new = document.createElement("span");
	    kw_new.setAttribute("class", "cpp-keyword");
	    kw_new.appendChild(document.createTextNode("new "));
	    this.view.appendChild(kw_new);
	    this.view.appendChild(this.type.create_view());
	    this.view.appendChild(document.createTextNode("["));
	    this.view.appendChild(this.size.create_view());
	    this.view.appendChild(document.createTextNode("]"));
	    return this.view;
	}
}


function ExpressionIndex(from, index){
	this.from = from;
	this.index = index;
	
	this.get = function(namespace_holder){
	    var from = this.from.get(namespace_holder);
	    var index = this.index.get(namespace_holder);
	    return from.value[index.value];
	}
	
	this.create_view = function(){
	    this.view = document.createElement("span");
	    this.view.appendChild(this.from.create_view());
	    this.view.appendChild(document.createTextNode("["));
	    this.view.appendChild(this.index.create_view());
	    this.view.appendChild(document.createTextNode("]"));
	    return this.view;
	}
}


function StatementCondition(ifs, else_block){
    this.ifs = ifs;
    this.else_block = else_block;
    this.current_if = undefined;
    this.inside_if = false;

    this.get = function(namespace_holder){
        for(var i=0; i<this.ifs.length; ++i){
            if(ifs[i].condition.get(namespace_holder).value){
                return ifs[i].block.get(namespace_holder);
            }
        }
        if(this.else_block !== undefined){
            return this.else_block.get(namespace_holder);
        }
    }

    this.step = function(namespace_holder){
        if(this.current_if === undefined){
            this.current_if = 0;
            return this.step(namespace_holder);
        }else if(this.current_if == this.ifs.length){
            if(this.else_block !== undefined){
                return this.else_block.step(namespace_holder);
            }else{
                return true;
            }
        }else{
            if(this.inside_if || this.ifs[this.current_if].condition.get(namespace_holder).value){
                this.inside_if = true;
                return this.ifs[this.current_if].block.step(namespace_holder);
            }else{
                this.current_if++;
                return (this.current_if == this.ifs.length && this.else_block === undefined)
            }
        }
    }

    this.get_current_line = function(){
        if(this.current_if !== undefined){
            if(this.current_if < this.ifs.length){
                if(this.inside_if){
                    var line = this.ifs[this.current_if].block.get_current_line();
                    if(line === undefined){
                        return this.lines[this.current_if];
                    }else{
                        return line;
                    }
                }else{
                    return this.lines[this.current_if];
                }
            }else{
                var res = this.else_block.get_current_line();
                if(res === undefined){
                    return this.else_line;
                }else{
                    return res;
                }
            }
        }else{
            return this.lines[0]
        }
    }

    this.reset = function(namespace_holder){
        this.current_if = undefined;
        this.inside_if = false;
        for(var i=0; i<this.ifs.length; ++i){
            this.ifs[i].block.reset();
        }
        if(this.else_block !== undefined){
            this.else_block.reset();
        }
    }
    
    this.create_lines = function(line_holder){
        if(ifs.length == 0){
            console.log("Warning: no ifs in if block");
            return;
        }
        var first_line = document.createElement("span");
        first_line.appendChild(create_element_cls("span", "if", "cpp-keyword"));
        first_line.appendChild(document.createTextNode("("));
        first_line.appendChild(this.ifs[0].condition.create_view());
        first_line.appendChild(document.createTextNode("){"));

        this.lines = [];
        
        this.lines[0] = new CodeLine(line_holder, first_line);
        this.ifs[0].block.create_lines(line_holder);

        for(var i=1; i<this.ifs.length; ++i){
            var line = document.createElement("span");
            line.appendChild(document.createTextNode("}"));
            line.appendChild(create_element_cls("span", "else if", "cpp-keyword"));
            line.appendChild(document.createTextNode("("));
            line.appendChild(this.ifs[i].condition.create_view());
            line.appendChild(document.createTextNode("){"));
            
            this.lines[i] = new CodeLine(line_holder, line);
            this.ifs[i].block.create_lines(line_holder);
        }
        if(this.else_block !== undefined){
            var line = document.createElement("span");
            line.appendChild(document.createTextNode("}"));
            line.appendChild(create_element_cls("span", "else", "cpp-keyword"));
            line.appendChild(document.createTextNode("{"));
            
            this.else_line = new CodeLine(line_holder, line);
            this.else_block.create_lines(line_holder);
        }
        var line = document.createElement("span");
        line.appendChild(document.createTextNode("}"));
        new CodeLine(line_holder, line);
    }
}


function StatementForLoop(init, step, cond, block){
    this.init = init;
    this.step_act = step;
    this.cond = cond;
    this.block = block;

    this.state = undefined;

    this.get = function(namespace_holder){
        namespace_holder.push();
        this.init.get(namespace_holder);
        while(this.cond.get(namespace_holder).value){
            this.block.get(namespace_holder);
            this.step_act.get(namespace_holder);
        }
        namespace_holder.pop();
    }

    this.step = function(namespace_holder){
        if(this.state === undefined){
            namespace_holder.push();
            this.init.get(namespace_holder);
            var val = this.cond.get(namespace_holder);
            if(val.value){
                this.state = 1;
            }else{
                namespace_holder.pop();
                return true;
            }
            this.step(namespace_holder)
        }else if(this.state === 0){
            var val = this.cond.get(namespace_holder);
            if(val.value){
                this.state = 1;
            }else{
                namespace_holder.pop();
                return true;
            }
            this.step(namespace_holder)
        }else if(this.state === 1){
            var res = this.block.step(namespace_holder);
            if(res){
                this.state = 0;
                this.block.reset();
                this.step_act.get(namespace_holder);
            }
        }
        return false;
    }

    this.get_current_line = function(namespace_holder){
        if(this.state === undefined || this.state == 0){
            return this.start_line;
        }else{
            var line = this.block.get_current_line();
            if(line === undefined){
                return this.start_line;
            }else{
                return line;
            }
        }
    }

    this.reset = function(){
        this.state = undefined;
        this.block.reset();
    }

    this.create_lines = function(line_holder){
        var line = document.createElement("span");
        line.appendChild(create_element_cls("span", "for", "cpp-keyword"));
        line.appendChild(document.createTextNode("("));
        line.appendChild(this.init.create_view());
        line.appendChild(document.createTextNode("; "));
        line.appendChild(this.cond.create_view());
        line.appendChild(document.createTextNode("; "));
        line.appendChild(this.step_act.create_view());
        line.appendChild(document.createTextNode("){"));
        this.start_line = new CodeLine(line_holder, line);
        this.block.create_lines(line_holder);
        this.end_line = new CodeLine(line_holder, document.createTextNode("}"));
    }
}


function StatementVarDef(type, assignments){
    this.type = type;
    this.assignments = assignments;

    this.get = function(line_holder){
        for(var i=0; i<this.assignments.length; ++i){
            var a = assignments[i];
            var value = a.expression.get(namespace_holder).value;
            if(this.type.test("int")){
                value = Math.floor(value);
            }
            line_holder.namespace.define(a.name, this.type, value);
        }
    }

    this.step = function(line_holder){
        this.get(line_holder);
        return true;
    }

    this.get_current_line = function(){return this.line};

    this.reset = function(){}

    this.create_lines = function(line_holder){
        var line = this.create_view();
        line.appendChild(document.createTextNode(";"));
        this.line = new CodeLine(line_holder, line);
    }

    this.create_view = function(){
        var line = document.createElement("span");
        line.appendChild(this.type.create_view());
        for(var i=0; i<this.assignments.length; ++i){
            var a = this.assignments[i];
            line.appendChild(document.createTextNode(a.name));
            line.appendChild(document.createTextNode(" = "));
            line.appendChild(a.expression.create_view());
            if(i<this.assignments.length - 1){
                line.appendChild(document.createTextNode(", "));
            }
        }
        return line;

    }
}


function create_element_cls(elem, str, cls){
    var e = document.createElement(elem);
    e.setAttribute("class", cls);
    e.appendChild(document.createTextNode(str));
    return e;
}
