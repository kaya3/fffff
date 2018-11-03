type Value
	= StringValue
	| IntValue
	| BoolValue
	| DoubleValue
	| Quote
	| VStack
	| Scope
	;

function typecheck<T>(v: any, clazz: { new(): T }) {
	if(v instanceof clazz) {
		return v;
	} else {
		// TODO: does constructor.name work?
		throw new Error('Type error: expected ' + clazz + ', was ' + v.constructor.name);
	}
}


class StringValue {
	public constructor(public readonly v: string) {}
	
	public toString(): string {
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
		let sb: Array<string> = [];
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
				
				case '\'':
				case '"':
					if(c === d) {
						sb.push("\\");
					}
					sb.push(c);
					break;
				
				default:
					let cc: number = c.charCodeAt(i);
					if(cc < 0x20 || cc > 0x7F) {
						sb.push('\\u');
						sb.push(('0000' + cc.toString(16)).substr(-4));
					} else {
						sb.push(c);
					}
			}
		}
		return sb.join('');
	}
}

class IntValue {
	public constructor(public readonly v: number) {}
	
	public toString(): string {
		return this.v.toString();
	}
}

class BoolValue {
	public static readonly TRUE: BoolValue = new BoolValue(true);
	public static readonly FALSE: BoolValue = new BoolValue(false);
	
	public static readonly PUSH_TRUE: PushOp = new PushOp(BoolValue.TRUE);
	public static readonly PUSH_FALSE: PushOp = new PushOp(BoolValue.FALSE);
	
	private constructor(public readonly v: boolean) {}
	
	public toString(): string {
		return this.v ? 'true' : 'false';
	}
}

class DoubleValue {
	public constructor(readonly v: number) {}
	
	public toString(): string {
		return this.v.toString();
	}
}

class Quote {
	readonly ops: Array<Op> = [];
}

class VStack {
	readonly v: Array<Value> = [];
	
	public peek<T extends Value>(clazz: { new(): T }): T {
		return typecheck(this.v.peek(), clazz);
	}
	
	public pop<T extends Value>(clazz: { new(): T }): T {
		if(this.v.length === 0) { throw new Error('Pop from empty stack'); }
		return typecheck(this.v.pop(), clazz);
	}
	
	private isStringing: boolean = false;
	public toString(): string {
		if(this.isStringing) {
			return '[...].';
		} else {
			this.isStringing = true;
			let sb: Array<string> = [];
			
			let glue: string = '';
			for(let i: number = 0; i < this.v.length; ++i) {
				sb.push(glue);
				glue = ' ';
				sb.push(this.v[i].toString());
			}
			
			this.isStringing = false;
			return sb.join('');
		}
	}
}

class Scope {
	private readonly v: { [k: string]: PushOp|QuotedOp } = Object.create(null);
	
	public read(name: string): Op|null {
		if(Object.prototype.hasOwnProperty.call(this.v, name)) {
			return this.v[name];
		} else {
			return null;
		}
	}
	
	public doAssignment(op: AssignOp, val: Value): void {
		if(op.doNow) {
			this.assignOp(op.name, val as Quote);
		} else {
			this.assignValue(op.name, val);
		}
	}
	
	public assignOp(name: string, q: Quote): void {
		this.v[name] = new QuotedOp(q);
	}
	
	public assignValue(name: string, val: Value): void {
		this.v[name] = new PushOp(val);
	}
	
	private isStringing: boolean = false;
	public toString(): string {
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
						sb.push(op.v.toString());
						sb.push('>');
					} else {
						sb.push(op.q.toString());
						sb.push('>!');
					}
					sb.push(k);
				}
			}
			
			sb.push('}.');
			this.isStringing = false;
			return sb.join('');
		}
	}
}

