type Value
	= StringValue
	| IntValue
	| BoolValue
	| DoubleValue
	| Quote
	| VStack
	| Scope
	;

type ValueTypeTags = {
	'string': StringValue,
	'int': IntValue,
	'boolean': BoolValue,
	'double': DoubleValue,
	'quote': Quote,
	'stack': VStack,
	'scope': Scope
};

function typecheck<T extends Value['type']>(v: Value, typeTag: T): ValueTypeTags[T] {
	return (v.type === typeTag || ((v.type as string) === 'js_object' && typeTag === 'scope')) ? v : _ERROR.wrongType(v.type, typeTag);
}

class StringValue {
	public readonly type: 'string' = 'string';
	
	public constructor(public readonly v: string) {}
	
	public repr(): string {
		let singleQuotes: number = 0;
		let doubleQuotes: number = 0;
		for(let i: number = 0; i < this.v.length; ++i) {
			switch(this.v[i]) {
				case '"':
					++doubleQuotes;
					break;
				case "'":
					++singleQuotes;
					break;
			}
		}
		
		let d: string = (singleQuotes <= doubleQuotes ? "'" : '"');
		let sb: Array<string> = [d];
		for(let i: number = 0; i < this.v.length; ++i) {
			let c = this.v[i];
			switch(c) {
				case '\n':
					sb.push('\\n');
					break
				case '\r':
					sb.push("\\r");
					break;
				case '\t':
					sb.push("\\t");
					break;
				case '\b':
					sb.push("\\b");
					break;
				case '\f':
					sb.push("\\f");
					break;
				case '\\':
					sb.push("\\\\");
					break;
				
				case '"':
				case "'":
					if(c === d) {
						sb.push("\\");
					}
					sb.push(c);
					break;
				
				default:
					let cc: number = c.charCodeAt(i);
					if(cc < 0x20 || cc > 0x7F) {
						sb.push('\\u', ('0000' + cc.toString(16)).substr(-4));
					} else {
						sb.push(c);
					}
			}
		}
		sb.push(d);
		return sb.join('');
	}
	
	public toString(): string {
		return this.v;
	}
}

class IntValue {
	public readonly type: 'int' = 'int';
	
	public readonly v: number;
	public constructor(v: number) { this.v = v|0; }
	
	public repr(): string {
		return this.v.toString();
	}
	
	public toString(): string {
		return this.v.toString();
	}
}

class BoolValue {
	public readonly type: 'boolean' = 'boolean';
	
	public static readonly TRUE: BoolValue = new BoolValue(true);
	public static readonly FALSE: BoolValue = new BoolValue(false);
	
	private constructor(public readonly v: boolean) {}
	
	public repr(): string {
		return this.v ? 'true' : 'false';
	}
	
	public toString(): string {
		return this.v ? 'true' : 'false';
	}
}

class DoubleValue {
	public readonly type: 'double' = 'double';
	
	public constructor(readonly v: number) {}
	
	public repr(): string {
		return this.v.toString();
	}
	
	public toString(): string {
		return this.v.toString();
	}
}

class Quote {
	public readonly type: 'quote' = 'quote';
	
	readonly ops: Array<Op> = [];
	
	private isStringing: boolean = false;
	public repr(): string {
		if(this.isStringing) {
			return "(...)";
		} else {
			this.isStringing = true;
			let sb: Array<string> = ['('];
			
			let glue: string = '';
			for(let i: number = 0; i < this.ops.length; ++i) {
				sb.push(glue, this.ops[i].repr());
				glue = ' ';
			}
			
			sb.push(')');
			this.isStringing = false;
			return sb.join('');
		}
	}
	
	public toString(): string {
		return this.repr();
	}
}

class VStack {
	public readonly type: 'stack' = 'stack';
	
	readonly v: Array<Value> = [];
	
	public push(value: Value): void {
		this.v.push(value);
	}
	
	public peek<T extends Value['type']>(typeTag: T): ValueTypeTags[T] {
		return typecheck(this.v.peek(), typeTag);
	}
	
	public pop<T extends Value['type']>(typeTag: T): ValueTypeTags[T] {
		return typecheck(this.popAny(), typeTag);
	}
	
	public peekAny(): Value {
		return this.v.peek();
	}
	
	public popAny(): Value {
		return this.v.pop() as Value || _ERROR.emptyStack();
	}
	
	public getValue(index: number): Value {
		if(index < 0 || index >= this.v.length) {
			_ERROR.indexOutOfBounds(index, this.v.length);
		}
		return this.v[index];
	}
	
	public length(): number {
		return this.v.length;
	}
	
	private isStringing: boolean = false;
	public repr(): string {
		if(this.isStringing) {
			return '[...].';
		} else {
			this.isStringing = true;
			let sb: Array<string> = ['['];
			
			let glue: string = '';
			for(let i: number = 0; i < this.v.length; ++i) {
				sb.push(glue, this.v[i].repr());
				glue = ' ';
			}
			
			sb.push('].');
			this.isStringing = false;
			return sb.join('');
		}
	}
	
	public toString(): string {
		return this.repr();
	}
}

class Scope {
	public readonly type: 'scope' = 'scope';
	
	private readonly v: { [k: string]: PushOp|QuotedOp } = Object.create(null);
	
	public store(name: string, op: PushOp|QuotedOp): void {
		this.v[name] = op;
	}
	
	public load(name: string): PushOp|QuotedOp|null {
		if(Object.prototype.hasOwnProperty.call(this.v, name)) {
			return this.v[name];
		} else {
			return null;
		}
	}
	
	public doAssignment(op: AssignOp, val: Value): void {
		if(op.doNow) {
			if(!(val instanceof Quote)) { throw new Error('Illegal state: store immediate must be quote'); }
			this.store(op.name, new QuotedOp(val));
		} else {
			this.store(op.name, new PushOp(val));
		}
	}
	
	isStringing: boolean = false;
	public repr(): string {
		if(this.isStringing) {
			return '{...}.';
		} else {
			this.isStringing = true;
			let sb: Array<string> = ['{'];
			
			let glue: string = '';
			for(let k in this.v) {
				if(Object.prototype.hasOwnProperty.call(this.v, k)) {
					sb.push(glue);
					glue = ' ';
					
					let op: Op = this.v[k];
					if(op instanceof PushOp) {
						sb.push(op.v.repr(), '>');
					} else {
						sb.push(op.q.repr(), '>!');
					}
					sb.push(k);
				}
			}
			
			sb.push('}.');
			this.isStringing = false;
			return sb.join('');
		}
	}
	
	public toString(): string {
		return this.repr();
	}
}
