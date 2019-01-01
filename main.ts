type Call = OpCall | QuoteCall | RepeatCall;

class OpCall {
	private _hasNext: boolean = true;
	public constructor(public readonly op: Op) {}
	
	public hasNext(): boolean {
		return this._hasNext;
	}
	
	public next(): Op {
		this._hasNext = false;
		return this.op;
	}
	
	public stackTrace(sb: Array<string>): void {
		// do nothing
	}
}

class QuoteCall {
	pos: number = 0;
	
	// used to generate correct stack traces after tail-call optimisation
	calls: number = 1;
	
	public constructor(public readonly q: Quote) {}
	
	public hasNext(): boolean {
		return this.pos < this.q.ops.length;
	}
	
	public next(): Op {
		return this.q.ops[this.pos++];
	}
	
	public stackTrace(sb: Array<string>): void {
		while(--this.calls >= 0) {
			sb.push('\tat (');
			let glue: string = '';
			for(let i: number = 0; i < this.q.ops.length; ++i) {
				sb.push(glue);
				glue = ' ';
				
				if(i === this.pos - 1) {
					sb.push('@');
				}
				sb.push(this.q.ops[i].repr());
			}
			sb.push(')\n');
			this.pos = this.q.ops.length;
		}
	}
}

class RepeatCall {
	private i: number = 0;
	public readonly op: Op;
	
	public constructor(q: Quote, public readonly n: number) {
		this.op = new QuotedOp(q);
	}
	
	public hasNext(): boolean {
		return this.i < this.n;
	}
	
	public next(): Op {
		++this.i;
		return this.op;
	}
	
	public stackTrace(sb: Array<string>): void {
		sb.push('\tat iteration ' + this.i + ' of ' + this.n + '\n');
	}
}

class Interpreter {
	private readonly callStack: Array<Call> = [];
	private readonly scopeStack: Array<Scope> = [new Scope()];
	private readonly valueStacks: Array<VStack> = [new VStack()];
	private exported: Scope|null = null;
	
	public constructor(public readonly out: (s: string) => void = console.log) {}
	
	public stackTrace(): string {
		let sb: Array<string> = [];
		while(this.callStack.length) {
			(this.callStack.pop() as Call).stackTrace(sb);
		}
		return sb.join('');
	}
	
	public execQuote(q: Quote) {
		this.callStack.clear();
		this.callStack.push(new QuoteCall(q));
		
		while(this.callStack.length) {
			this.step();
		}
	}
	
	private step(): void {
		let c: Call = this.callStack.peek();
		if(!c.hasNext()) {
			this.callStack.pop();
			return;
		}
		let op: Op = c.next();
		
		let vs: VStack = this.valueStacks.peek();
		
		if(op instanceof NativeOp) {
			switch(op.name) {
				case 'true':
					vs.push(BoolValue.TRUE);
					break;
				
				case 'false':
					vs.push(BoolValue.FALSE);
					break;
				
				case 'import':
				case 'import_as':
				case 'export':
					// TODO
					throw new Error('Not implemented');
				
				case '[':
					this.valueStacks.push(new VStack());
					break;
				
				case '.[':
					this.valueStacks.push(vs.pop('stack'));
					break;
				
				case ']':
					if(this.valueStacks.length === 1) { _ERROR.ascendFromGlobalStack(); }
					this.valueStacks.pop();
					break;
				
				case '].':
					if(this.valueStacks.length === 1) { _ERROR.ascendFromGlobalStack(); }
					this.valueStacks.pop();
					this.valueStacks.peek().push(vs);
					break;
				
				case '{':
					this.scopeStack.push(new Scope());
					break;
				
				case '.{':
					this.scopeStack.push(vs.pop('scope'));
					break;
				
				case '}':
					if(this.scopeStack.length === 1) { _ERROR.ascendFromGlobalScope(); }
					this.scopeStack.pop();
					break;
				
				case '}.':
					if(this.scopeStack.length === 1) { _ERROR.ascendFromGlobalScope(); }
					vs.push(this.scopeStack.pop() as Scope);
					break;
				
				case '!':
					let q: Quote = vs.pop('quote');
					if(c instanceof QuoteCall && !c.hasNext() && c.q === q) {
						// tail recursion
						c.pos = 0;
						++c.calls;
					} else {
						this.callStack.push(new QuoteCall(q));
					}
					break;
				
				case 'print':
					this.out(vs.popAny().toString());
					break;
				
				case 'println':
					this.out(vs.popAny().toString() + '\n');
					break;
				
				case 'del':
					vs.popAny();
					break;
				
				case 'push':
					let v: Value = vs.popAny();
					vs.peek('stack').push(v);
					break;
				
				case 'pop':
					let s: VStack = vs.peek('stack');
					vs.push(s.popAny());
					break;
				
				case 'len':
					s = vs.pop('stack');
					vs.push(new IntValue(s.length()));
					break;
				
				case 'get':
					let i: number = this.popInt();
					s = vs.peek('stack');
					vs.push(s.getValue(i));
					break;
				
				case 'and':
					let b1: boolean = this.popBool();
					let b2: boolean = this.popBool();
					vs.push(b1 && b2 ? BoolValue.TRUE : BoolValue.FALSE);
					break;
				
				case 'or':
					b1 = this.popBool();
					b2 = this.popBool();
					vs.push(b1 || b2 ? BoolValue.TRUE : BoolValue.FALSE);
					break;
				
				case 'not':
					vs.push(this.popBool() ? BoolValue.FALSE : BoolValue.TRUE);
					break;
				
				case 'if':
					b1 = this.popBool();
					q = vs.pop('quote');
					if(b1) {
						this.callStack.push(new QuoteCall(q));
					}
					break;
				
				case 'repeat':
					let n: number = vs.pop('int').v;
					q = vs.pop('quote');
					this.callStack.push(new RepeatCall(q, n));
					break;
				
				case 'this':
					vs.push(this.scopeStack.peek());
					break;
				
				case 'stack':
					vs.push(vs);
					break;
				
				case '+':
					vs.push(new IntValue(this.popInt() + this.popInt()));
					break;
				
				case '-':
					vs.push(new IntValue((-this.popInt()) + this.popInt()));
					break;
				
				case '*':
					vs.push(new IntValue(_NATIVE.imul(this.popInt(), this.popInt())));
					break;
				
				case '**':
					let i2: number = this.popInt();
					let i1: number = this.popInt();
					vs.push(new IntValue(_NATIVE.ipow(i1, i2)));
					break;
				
				case '/':
					i2 = this.popInt();
					i1 = this.popInt();
					vs.push(new IntValue(_NATIVE.idiv(i1, i2)));
					break;
				
				case '%':
					i2 = this.popInt();
					i1 = this.popInt();
					vs.push(new IntValue(_NATIVE.imod(i1, i2)));
					break;
				
				case '&':
					vs.push(new IntValue(this.popInt() & this.popInt()));
					break;
				
				case '|':
					vs.push(new IntValue(this.popInt() | this.popInt()));
					break;
				
				case '~':
					vs.push(new IntValue(~this.popInt()));
					break;
				
				// TODO: allow comparisons of other types
				case '=':
					vs.push(_NATIVE.boolean(this.popInt() === this.popInt()));
					break;
				
				// operands popped in reverse order
				case '<':
					vs.push(_NATIVE.boolean(this.popInt() > this.popInt()));
					break;
				
				case '>':
					vs.push(_NATIVE.boolean(this.popInt() < this.popInt()));
					break;
				
				case '<=':
					vs.push(_NATIVE.boolean(this.popInt() >= this.popInt()));
					break;
				
				case '>=':
					vs.push(_NATIVE.boolean(this.popInt() <= this.popInt()));
					break;
				
				default:
					throw new Error('Illegal state: unknown operator ' + op);
				}
		} else if(op instanceof QuotedOp) {
			this.callStack.push(new QuoteCall(op.q));
		} else if(op instanceof PushOp) {
			// TODO: check read-only status of imported stacks
			vs.push(op.v);
		} else if(op instanceof AssignOp) {
			// TODO: check read-only status of imported scopes
			this.scopeStack.peek().doAssignment(op, vs.popAny());
		} else if(op instanceof ReadOp) {
			let name: string = op.name;
			let doOp: Op|null = null;
			let i: number = this.scopeStack.length;
			while(doOp === null) {
				if(--i >= 0) {
					doOp = this.scopeStack[i].load(name);
				} else {
					// TODO: builtins only need to be dynamically resolved if they can be overloaded
					_ERROR.nameError(name);
				}
			}
			
			this.callStack.push(new OpCall(doOp));
		} else if(op instanceof LocalReadOp) {
			let name: string = op.name;
			let doOp: Op = this.scopeStack.peek().load(name) || _ERROR.nameError(name);;
			this.callStack.push(new OpCall(doOp));
		} else if(op instanceof CommentOp) {
			// do nothing
		} else {
			// typecheck to make sure all ops covered
			let ignore: never = op;
			throw new Error('Illegal state: unknown operator ' + op);
		}
	}
	
	private popBool(): boolean {
		return this.valueStacks.peek().pop('boolean').v;
	}
	
	private popInt(): number {
		return this.valueStacks.peek().pop('int').v;
	}
	
	private popDouble(): number {
		return this.valueStacks.peek().pop('double').v;
	}
	
	public toString(): string {
		let sb: Array<string|number> = [];
		
		for(let i: number = 0; i < this.valueStacks.length; ++i) {
			sb.push(i, ': ', this.valueStacks[i].repr(), '\n');
		}
		for(let i: number = 0; i < this.scopeStack.length; ++i) {
			sb.push(i, ': ', this.scopeStack[i].repr(), '\n');
		}
		
		return sb.join('');
	}
}
