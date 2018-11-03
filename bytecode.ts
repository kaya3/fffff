type ConstantPrimitiveType = 'int'|'double'|'string';

type Constants = CodeObject;

class ConstantPrimitive {
	public readonly constRef: string;
	constructor(public readonly v: string|number, public readonly type: ConstantPrimitiveType, constID: number) {
		this.constRef = (
			type === 'int'
			? NativeOp.CONST_INT.opcode
			: type === 'double'
			? NativeOp.CONST_DOUBLE.opcode
			: NativeOp.CONST_STRING.opcode
		) + constID;
	}
}

class ByteCode {
	public readonly compiled: string;
	public readonly constRef: string;
	public constructor(q: Quote, constants: Constants, constID: number) {
		this.constRef = NativeOp.CONST_QUOTE.opcode + constID;
		let sb: Array<string> = [];
		for(let i: number = 0; i < q.ops.length; ++i) {
			sb.push(q.ops[i].compile(constants));
		}
		this.compiled = sb.join('');
	}
}

type JSONCodeObject = {
	bytecode: Array<string>,
	constants: Array<string|number>
};

class CodeObject {
	public constructor(q: Quote) {
		this.addQuote(q);
	}
	
	private nextBCID: number = 0;
	public readonly primitives: Array<ConstantPrimitive> = [];
	public readonly byteCodes: Array<ByteCode> = [];
	
	public addQuote(q: Quote): string {
		let bcID = this.nextBCID++;
		let bc: ByteCode = new ByteCode(q, this, bcID);
		this.byteCodes[bcID] = bc;
		return bc.constRef;
	}
	
	public addPrimitive(v: number|string, typeTag: ConstantPrimitiveType): string {
		for(let i: number = 0; i < this.primitives.length; ++i) {
			let p: ConstantPrimitive = this.primitives[i];
			if(p.type === typeTag && p.v === v) {
				return p.constRef;
			}
		}
		let p: ConstantPrimitive = new ConstantPrimitive(v, typeTag, this.primitives.length);
		this.primitives.push(p);
		return p.constRef;
	}
	
	public toJSON(): JSONCodeObject {
		return {
			bytecode: this.byteCodes.map(bc => bc.compiled),
			constants: this.primitives.map(p => p.v)
		};
	}
}

function decompile(jsonCodeObject: JSONCodeObject, index: number = 0): Quote {
	let bc: string|number = jsonCodeObject.bytecode[index];
	let q: Quote = new Quote();
	let pos: number = 0;
	while(pos < bc.length) {
		let c: string = bc[pos++];
		let op: Op|null = NativeOp.getByOpCode(c);
		if(op === null) {
			throw new Error('Bytecode error: illegal opcode `' + c + '` at index ' + index + ', position ' + (pos-1));
		} else if(op.name === null) {
			let startPos = pos;
			while(/[0-9]/.test(bc[pos]) && ++pos < bc.length);
			if(startPos === pos) {
				throw new Error('Bytecode error: expected numerical index at index ' + index + ', position ' + pos);
			}
			let constID: number = parseInt(bc.substring(startPos, pos));
			if(c === NativeOp.CONST_QUOTE.opcode) {
				if(constID < index) {
					throw new Error('Bytecode error: backward reference in bytecode array');
				}
				op = new PushOp(decompile(jsonCodeObject, constID));
			} else {
				let v: string|number = jsonCodeObject.constants[constID];
				var reqString = function(): string {
					if(typeof v !== 'string') {
						throw new Error('Bytecode error: expected string constant at index ' + index + ', id ' + constID);
					}
					return v;
				}
				switch(c) {
					case NativeOp.CONST_INT.opcode:
						if(typeof v !== 'number' || v !== (v|0)) {
							throw new Error('Bytecode error: expected int constant at index ' + index + ', id ' + constID);
						}
						op = new PushOp(new IntValue(v));
						break;
					case NativeOp.CONST_DOUBLE.opcode:
						if(typeof v !== 'number') {
							throw new Error('Bytecode error: expected double constant at index ' + index + ', id ' + constID);
						}
						op = new PushOp(new DoubleValue(v));
						break;
					case NativeOp.CONST_STRING.opcode:
						op = new PushOp(new StringValue(reqString()));
						break;
					case NativeOp.STORE.opcode:
						op = new AssignOp(reqString(), false);
						break;
					case NativeOp.STORE_QUOTE.opcode:
						op = new AssignOp(reqString(), true);
						break;
					case NativeOp.LOAD_FAST.opcode:
						op = new LocalReadOp(reqString());
						break;
					case NativeOp.LOAD_SLOW.opcode:
						op = new ReadOp(reqString());
						break;
					default:
						throw new Error('Bytecode error: illegal opcode `' + c + '`');
				}
			}
		}
		q.ops.push(op);
	}
	return q;
}
