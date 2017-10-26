function Namespace(par){
	this.par = par;
	this.data = [];

	this.set = function(name, value){
		if(this.par != undefined && this.par.has(name)){
			this.par.set(name, value);
		}else{
			this.data[name] = value;
		}
	}

	this.has = function(name){
		return this.data[name] !== undefined || (this.par !== undefined && this.par.has(name));
	}

	this.get = function(name){
		if(this.data[name] != undefined){
			return this.data[name]
		}else if(this.par != undefined){
			return this.par.get(name);
		}else{
			return undefined;
		}
	}
	
	this.define = function(name, value){
		this.data[name] = value;
	}

	this.log = function(){
		for(var key in this.data){
			console.log(key + " = " + this.data[key]);
		}
		if(this.par !== undefined){
			this.par.log();
		}
	}
}


function NamespaceHolder(){
	this.namespace = new Namespace();
	this.push = function(){
		this.namespace = new Namespace(this.namespace);
	};
	this.pop = function(){
		this.namespace = this.namespace.par;
	}
	this.log = function(){
		this.namespace.log();
	}
}


function Action(code, func){
	this.func = func;
	this.evaluted = false;
	this.code = code;

	this.create_view = function(){
		this.view = document.createElement("div");
		this.view.appendChild(document.createTextNode(code));
		this.view.setAttribute("class", "code-line");
	}

	this.reset = function(){
		this.evaluted = false;
	}
	this.step = function(namespace){
		if(this.evaluted){
			this.view.setAttribute("class", "code-line");
			return {done: true}
		}else{
			this.evaluted = true;
			return {ret: this.func(namespace.namespace), done: true};
		}
	}
	this.selected = function(){
		return this.view;
	}
}


function Block(){
	this.children = [];
	this.index = -1;


	this.add = function(action){
		this.children.push(action);
	}

	this.reset = function(){
		this.index = -1;
		for(let c of this.children){
			c.reset();
		}
		if(this.namespace != undefined){
			this.namespace.pop();
		}
	}
	this.step = function(namespace){
		if(this.index >= this.children.length || this.children.lenght == 0){
			namespace.pop();
			this.namespace = undefined;
			return {ret: undefined, done: true};
		}else if(this.index == -1){
			this.index++;
			namespace.push();
			this.namespace = namespace;
			return this.step(namespace)
		}else{
			var res = this.children[this.index].step(namespace);
			if(res.ret !== undefined){
				namespace.pop();
				this.namespace = undefined;
				this.index = this.children.length;
				return {ret: res.ret, done: true}
			}else if(res.done == true){
				this.index++;
				if(this.index == this.children.length){
					namespace.pop();
					this.namespace = undefined;
					return {ret: undefined, done: true}
				}else{
					return {ret: undefined, done: false}
				}
			}else{
				return {ret: undefined, done: false}
			}
		}
	}
	this.selected = function(){
		var i = this.index;
		if(i < 0){
			i = 0;
		}
		if(i >= this.children.length){
			i = this.children.length - 1
		}
		return this.children[i].selected();
	}

	this.create_view = function(){
		this.view = document.createElement("div");
		this.view.setAttribute("class", "code-block");
		for(var c of this.children){
			c.create_view();
			this.view.appendChild(c.view)
		}
	}
}


function Condition(cond_text, cond, then_block, else_block){
	this.cond = cond;
	this.then_block = then_block;
	this.else_block = else_block;
	this.cond_res = undefined;

	this.create_view = function(){
		this.create_elif_view(false);		 
		this.view.appendChild(document.createTextNode("}"));
	}

	this.create_elif_view = function(is_elif){
		this.view = document.createElement("div");
		this.view.setAttribute("class", "code-condition");
		this.cond_div = document.createElement("div");
		if(is_elif){
			this.cond_div.appendChild(document.createTextNode("}else if(" + cond_text + "){"));
		}else{
			this.cond_div.appendChild(document.createTextNode("if(" + cond_text + "){"));
		}
		this.view.appendChild(this.cond_div);
		this.then_block.create_view();
		this.view.appendChild(this.then_block.view);

		if(this.else_block !== undefined){
			if(this.else_block.constructor == Condition){
				this.else_block.create_elif_view(true)
				this.view.appendChild(this.else_block.view);
			}else{
				this.view.appendChild(document.createTextNode("}else{"));
				this.else_block.create_view();
				this.view.appendChild(this.else_block.view);
			}
		}

	}

	this.reset = function(){
		this.then_block.reset();
		this.else_block.reset();
		this.cond_res = undefined;
	}
	this.step = function(namespace){
		if(this.cond_res === undefined){
			this.cond_res = this.cond(namespace.namespace);
			return {ret: undefined, done: false};
		}else if(this.cond_res){
			return this.then_block.step(namespace);
		}else{
			return this.else_block.step(namespace);
		}
	}
	this.selected = function(){
		if(this.cond_res === undefined){
			return this.cond_div;
		}else if(this.cond_res){
			return this.then_block.selected();
		}else{
			return this.else_block.selected();
		}
	}
}


function ForLoop(for_str, init, test, step, body){
	this.init = init;
	this.test = test;
	this.after_iter = step;
	this.str = for_str;
	this.body = body;

	this.state = 0;

	this.step = function(namespace){
		if(this.state == 0){
			this.namespace = namespace;
			namespace.push();
			this.init(namespace.namespace);
			this.state = 1;
		}
		if(this.state == 1){
			if(this.test(namespace.namespace)){
				this.state = 2;
			}else{
				this.state = 3;
				namespace.pop();
				return {ret: undefined, done: true}
			}
			return {ret: undefined, done: false}
		}else if(this.state == 2){
			var r = this.body.step(namespace);
			if(r.ret !== undefined){
				namespace.pop();
				return r;
			}else if(r.done){
				this.body.reset();
				this.after_iter(namespace.namespace);
				this.state = 1;
				return {ret: undefined, done: false}
			}else{
				return {ret: undefined, done: false}
			}
		}else{
			return {ret: undefined, done: true}
		}
	}

	this.reset = function(){
		this.body.reset();
		if(this.state != 0){
			this.state = 0;
			this.namespace.pop();
		}
	}

	this.create_view = function(){
		this.view = document.createElement("div");
		this.view.setAttribute("class", "code-for-loop");
		this.for_line = document.createElement("div");
		this.for_line.setAttribute("class", "code-for-loop-line");
		this.for_line.appendChild(document.createTextNode(this.str));
		this.view.appendChild(this.for_line);
		this.body.create_view();
		this.view.appendChild(this.body.view);
		this.view.appendChild(document.createTextNode("}"))
	}
	
	this.selected = function(){
		if(this.state == 0 || this.state == 1){
			return this.for_line;
		}else{
			return this.body.selected();
		}
	}
}


function Return(code, res){
	this.code = code;
	this.res = res;

	this.step = function(namespace){
		return {ret: this.res(namespace.namespace), done: true};
	}
	this.reset = function(){}
	this.create_view = function(){
		this.view = document.createElement("div");
		this.view.setAttribute("class", "code-line");
		this.view.appendChild(document.createTextNode(this.code));
	}

	this.selected = function(){
		return this.view;
	}
}
