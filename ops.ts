type Op
	= PushOp
	| ReadOp
	| LocalReadOp
	| AssignOp
	| NativeOp
	| QuotedOp
	| CommentOp
	;

class PushOp {
	public constructor(public readonly v: Value) {}
	
	public repr(): string {
		return this.v.repr();
	}
}

class QuotedOp {
	public constructor(public readonly q: Quote) {}
	
	private isStringing: boolean = false;
	public repr(): string {
		if(this.isStringing) {
			return '(...)';
		} else {
			this.isStringing = true;
			let sb: Array<string> = [];
			
			let glue: string = '';
			for(let i: number = 0; i < this.q.ops.length; ++i) {
				sb.push(glue);
				glue = ' ';
				sb.push(this.q.ops[i].repr());
			}
			
			this.isStringing = false;
			sb.push(')');
			return sb.join('');
		}
	}
}

class AssignOp {
	public constructor(public readonly name: string, public readonly doNow: boolean) {}
	
	public repr(): string {
		return '>' + this.name;
	}
}

class CommentOp {
	public constructor(public readonly s: string) {}
	
	public repr(): string {
		return '#' + this.s + '\n';
	}
}

class ReadOp {
	public constructor(public readonly name: string) {}
	
	public repr(): string {
		return this.name;
	}
}

class LocalReadOp {
	public constructor(public readonly name: string) {}
	
	public repr(): string {
		return this.name;
	}
}

class NativeOp {
	private static opsArray: Array<NativeOp> = [];
	private static opsMap: { [k: string]: NativeOp } = Object.create(null);
	
	public static getByOpCode(opCode: number): NativeOp {
		if(opCode < 1 || opCode >= NativeOp.opsArray.length) {
			throw new Error('Illegal state: unknown opcode ' + opCode);
		}
		return NativeOp.opsArray[opCode-1];
	}
	public static getByName(name: string): NativeOp|null {
		return NativeOp.opsMap[name] || null;
	}
	
	public static readonly STACK_DESCEND: NativeOp = new NativeOp('[');
	public static readonly STACK_ASCEND: NativeOp = new NativeOp(']');
	public static readonly STACK_ENTER: NativeOp = new NativeOp('.[');
	public static readonly STACK_EXIT: NativeOp = new NativeOp('].');
	
	public static readonly SCOPE_DESCEND: NativeOp = new NativeOp('{');
	public static readonly SCOPE_ASCEND: NativeOp = new NativeOp('}');
	public static readonly SCOPE_ENTER: NativeOp = new NativeOp('.{');
	public static readonly SCOPE_EXIT: NativeOp = new NativeOp('}.');
	
	public static readonly NOW: NativeOp = new NativeOp('!');
	
	public static readonly TRUE: NativeOp = new NativeOp('true');
	public static readonly FALSE: NativeOp = new NativeOp('false');
	
	// TODO: imports?
	public static readonly IMPORT: NativeOp = new NativeOp('import');
	public static readonly IMPORT_AS: NativeOp = new NativeOp('import_as');
	public static readonly EXPORT: NativeOp = new NativeOp('export');
	
	public static readonly PRINT: NativeOp = new NativeOp('print');
	public static readonly PRINTLN: NativeOp = new NativeOp('println');
	
	public static readonly DEL: NativeOp = new NativeOp('del');
	public static readonly PUSH: NativeOp = new NativeOp('push');
	public static readonly POP: NativeOp = new NativeOp('pop');
	public static readonly LEN: NativeOp = new NativeOp('len');
	public static readonly GET: NativeOp = new NativeOp('get');
	
	public static readonly AND: NativeOp = new NativeOp('and');
	public static readonly OR: NativeOp = new NativeOp('or');
	public static readonly NOT: NativeOp = new NativeOp('not');
	
	public static readonly IF: NativeOp = new NativeOp('if');
	public static readonly REPEAT: NativeOp = new NativeOp('repeat');
	
	public static readonly THIS: NativeOp = new NativeOp('this');
	public static readonly STACK: NativeOp = new NativeOp('stack');
	
	public static readonly ADD: NativeOp = new NativeOp('+');
	public static readonly SUBTRACT: NativeOp = new NativeOp('-');
	public static readonly MULTIPLY: NativeOp = new NativeOp('*');
	public static readonly POW: NativeOp = new NativeOp('**');
	public static readonly DIVIDE: NativeOp = new NativeOp('/');
	public static readonly MODULO: NativeOp = new NativeOp('%');
	
	public static readonly BIT_AND: NativeOp = new NativeOp('&');
	public static readonly BIT_OR: NativeOp = new NativeOp('|');
	public static readonly BIT_NEG: NativeOp = new NativeOp('~');
	public static readonly BIT_XOR: NativeOp = new NativeOp('^');
	
	public static readonly EQUALS: NativeOp = new NativeOp('=');
	public static readonly LESS_THAN: NativeOp = new NativeOp('<');
	public static readonly GREATER_THAN: NativeOp = new NativeOp('>');
	public static readonly LESS_THAN_OR_EQUAL: NativeOp = new NativeOp('<=');
	public static readonly GREATER_THAN_OR_EQUAL: NativeOp = new NativeOp('>=');
	
	public readonly opCode: number;
	private constructor(public readonly name: string) {
		NativeOp.opsMap[name] = this;
		NativeOp.opsArray.push(this);
		this.opCode = NativeOp.opsArray.length;
	}
	
	public repr(): string {
		return this.name;
	}
}
