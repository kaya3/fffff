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
	
	public compile(constants: Constants): string {
		if(this.v.type === 'quote') {
			return constants.addQuote(this.v);
		} else if(this.v.type === 'int' || this.v.type === 'double' || this.v.type === 'string') {
			return constants.addPrimitive(this.v.v, this.v.type);
		} else {
			throw new Error('Illegal state: compile-time constant must be int, double, string or quote');
		}
	}
	
	public repr(): string {
		return this.v.repr();
	}
}

class QuotedOp {
	public constructor(public readonly q: Quote) {}
	
	public compile(constants: Constants): never {
		throw new Error('Illegal state: QuotedOp should not occur at compile-time');
	}
	
	public repr(): string {
		return this.q.repr() + '!';
	}
}

class AssignOp {
	public constructor(public readonly name: string, public readonly doNow: boolean) {}
	
	public compile(constants: Constants): string {
		let c: string = constants.addPrimitive(this.name, 'string');
		return (this.doNow ? NativeOp.STORE_QUOTE : NativeOp.STORE).opcode + c.substring(1);
	}
	
	public repr(): string {
		return (this.doNow ? '>!' : '>') + this.name;
	}
}

class CommentOp {
	public constructor(public readonly s: string) {}
	
	public compile(constants: Constants): string {
		return '';
	}
	
	public repr(): string {
		return '#' + this.s + '\n';
	}
}

class ReadOp {
	public constructor(public readonly name: string) {}
	
	public compile(constants: Constants): string {
		let c: string = constants.addPrimitive(this.name, 'string');
		return NativeOp.LOAD_SLOW.compile(constants) + c.substring(1);
	}
	
	public repr(): string {
		return this.name;
	}
}

class LocalReadOp {
	public constructor(public readonly name: string) {}
	
	public compile(constants: Constants): string {
		let c: string = constants.addPrimitive(this.name, 'string');
		return NativeOp.LOAD_FAST.compile(constants) + c.substring(1);
	}
	
	public repr(): string {
		return this.name;
	}
}

class NativeOp {
	private static byName: { [k: string]: NativeOp } = Object.create(null);
	private static byOpCode: { [k: string]: NativeOp } = Object.create(null);
	public static getByName(name: string): NativeOp|null {
		return NativeOp.byName[name] || null;
	}
	public static getByOpCode(opcode: string): NativeOp|null {
		return NativeOp.byOpCode[opcode] || null;
	}
	
	public static readonly STACK_DESCEND: NativeOp = new NativeOp('[', 'd');
	public static readonly STACK_ASCEND: NativeOp = new NativeOp(']', 'a');
	public static readonly STACK_ENTER: NativeOp = new NativeOp('.[', 'e');
	public static readonly STACK_EXIT: NativeOp = new NativeOp('].', 'x');
	
	public static readonly SCOPE_DESCEND: NativeOp = new NativeOp('{', 'D');
	public static readonly SCOPE_ASCEND: NativeOp = new NativeOp('}', 'A');
	public static readonly SCOPE_ENTER: NativeOp = new NativeOp('.{', 'E');
	public static readonly SCOPE_EXIT: NativeOp = new NativeOp('}.', 'X');
	
	public static readonly NOW: NativeOp = new NativeOp('!', '!');
	
	public static readonly STORE: NativeOp = new NativeOp(null, 's');
	public static readonly STORE_QUOTE: NativeOp = new NativeOp(null, 'q');
	public static readonly LOAD_FAST: NativeOp = new NativeOp(null, 'l');
	public static readonly LOAD_SLOW: NativeOp = new NativeOp(null, 'L');
	
	public static readonly CONST_QUOTE: NativeOp = new NativeOp(null, 'Q');
	public static readonly CONST_INT: NativeOp = new NativeOp(null, 'j');
	public static readonly CONST_DOUBLE: NativeOp = new NativeOp(null, 'J');
	public static readonly CONST_STRING: NativeOp = new NativeOp(null, 'k');
	
	public static readonly TRUE: NativeOp = new NativeOp('true', 't');
	public static readonly FALSE: NativeOp = new NativeOp('false', 'f');
	
	// TODO: imports?
	public static readonly IMPORT: NativeOp = new NativeOp('import', 'i');
	public static readonly IMPORT_AS: NativeOp = new NativeOp('import_as', 'I');
	public static readonly EXPORT: NativeOp = new NativeOp('export', '$');
	
	public static readonly PRINT: NativeOp = new NativeOp('print', 'p');
	public static readonly PRINTLN: NativeOp = new NativeOp('println', 'P');
	
	public static readonly DEL: NativeOp = new NativeOp('del', '_');
	public static readonly PUSH: NativeOp = new NativeOp('push', 'b');
	public static readonly POP: NativeOp = new NativeOp('pop', 'B');
	public static readonly LEN: NativeOp = new NativeOp('len', 'n');
	public static readonly GET: NativeOp = new NativeOp('get', 'g');
	
	public static readonly AND: NativeOp = new NativeOp('and', 'y');
	public static readonly OR: NativeOp = new NativeOp('or', 'Y');
	public static readonly NOT: NativeOp = new NativeOp('not', 'z');
	
	public static readonly IF: NativeOp = new NativeOp('if', '?');
	public static readonly REPEAT: NativeOp = new NativeOp('repeat', 'r');
	
	public static readonly THIS: NativeOp = new NativeOp('this', 'T');
	public static readonly STACK: NativeOp = new NativeOp('stack', 'S');
	
	public static readonly ADD: NativeOp = new NativeOp('+', '+');
	public static readonly SUBTRACT: NativeOp = new NativeOp('-', '-');
	public static readonly MULTIPLY: NativeOp = new NativeOp('*', '*');
	public static readonly POW: NativeOp = new NativeOp('**', 'w');
	public static readonly DIVIDE: NativeOp = new NativeOp('/', '/');
	public static readonly MODULO: NativeOp = new NativeOp('%', '%');
	
	public static readonly BIT_AND: NativeOp = new NativeOp('&', '&');
	public static readonly BIT_OR: NativeOp = new NativeOp('|', '|');
	public static readonly BIT_NEG: NativeOp = new NativeOp('~', '~');
	public static readonly BIT_XOR: NativeOp = new NativeOp('^', '^');
	
	public static readonly EQUALS: NativeOp = new NativeOp('=', '=');
	public static readonly LESS_THAN: NativeOp = new NativeOp('<', 'c');
	public static readonly GREATER_THAN: NativeOp = new NativeOp('>', 'h');
	public static readonly LESS_THAN_OR_EQUAL: NativeOp = new NativeOp('<=', 'C');
	public static readonly GREATER_THAN_OR_EQUAL: NativeOp = new NativeOp('>=', 'H');
	
	private constructor(public readonly name: string|null, public readonly opcode: string) {
		if(name !== null) {
			NativeOp.byName[name] = this;
		}
		if(Object.prototype.hasOwnProperty.call(NativeOp.byOpCode, opcode)) {
			throw new Error('Illegal state: opcode `' + opcode + '` is already defined');
		}
		NativeOp.byOpCode[opcode] = this;
	}
	
	public compile(constants: Constants): string {
		return this.opcode;
	}
	
	public repr(): string {
		if(this.name === null) {
			throw new Error('Illegal state: opcode `' + this.opcode + '` should not exist in interactive mode');
		}
		return this.name;
	}
}
