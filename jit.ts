type TmpVarName = '_tmp1'|'_tmp2'|'_tmp3';

class JITCompiler {
	private static readonly opCodesWithConstants: string = NativeOp.CONST_INT.opcode + NativeOp.CONST_DOUBLE.opcode + NativeOp.CONST_STRING.opcode + NativeOp.CONST_QUOTE.opcode + NativeOp.STORE.opcode + NativeOp.STORE_QUOTE.opcode + NativeOp.LOAD_FAST.opcode + NativeOp.LOAD_SLOW.opcode;
	
	public constructor(public readonly jsonCodeObject: JSONCodeObject) {}
	
	public compileAll(): Function {
		let sb: Array<string|number> = [
			'var _stack = [], _scope = Object.create(null), _stacks = [_stack], _scopes = [_scope], _tmp1, _tmp2, _tmp3;\n'
		];
		
		for(let i: number = 0; i < this.jsonCodeObject.bytecode.length; ++i) {
			sb.push('var _q', i, ' = function() {\n');
			this.compileByteCode(i, this.jsonCodeObject.bytecode[i], sb);
			sb.push('};\n');
		}
		
		sb.push('_q0();');
		console.log(sb.join(''));
		return new Function('_NATIVE', '_OUT', '_ERROR', sb.join(''));
	}
	
	private compileByteCode(index: number, bc: string, sb: Array<string|number>): void {
		let pos: number = 0;
		while(pos < bc.length) {
			let c: string = bc[pos++];
			let op: Op|null = NativeOp.getByOpCode(c);
			if(op === null) {
				throw new Error('Bytecode error: illegal opcode `' + c + '` at index ' + index + ', position ' + (pos-1));
			} else if(JITCompiler.opCodesWithConstants.indexOf(c) >= 0) {
				let constIDstr: string = extractID(index, bc, pos);
				let constID: number = parseInt(constIDstr);
				this.compileOpCodeWithConstant(index, c, constID, sb);
				pos += constIDstr.length;
			} else {
				this.compileOpCode(c, sb);
			}
		}
	}
	
	private compileOpCode(opCode: string, sb: Array<string|number>): void {
		sb.push('// opcode: `', opCode, '`, op: ', NativeOp.getByOpCode(opCode)!.repr(), '\n');
		switch(opCode) {
			case NativeOp.STACK_DESCEND.opcode:
				sb.push(
					'\t', '_stack = [];',
					'\t', '_stacks.push(_stack);\n'
				);
				break;
			
			case NativeOp.STACK_ASCEND.opcode:
				sb.push(
					'\t', 'if(_stacks.length === 1) { _ERROR.ascendFromGlobalStack(); }\n',
					'\t', '_stacks.pop();\n',
					'\t', '_stack = _stacks[_stacks.length-1];\n'
				);
				break;
			
			case NativeOp.STACK_ENTER.opcode:
				this.writePop(sb, '_tmp1', 'stack');
				sb.push('\t', '_stacks.push(_stack = _tmp1);\n');
				break;
			
			case NativeOp.STACK_EXIT.opcode:
				sb.push(
					'\t', 'if(_stacks.length === 1) { _ERROR.ascendFromGlobalStack(); }\n',
					'\t', '_tmp1 = _stacks.pop();\n',
					'\t', '_stack = _stacks[_stacks.length-1];\n',
					'\t', '_stack.push({ type: "stack", v: _tmp1 });\n'
				);
				break;
			
			case NativeOp.SCOPE_DESCEND.opcode:
				sb.push(
					'\t', '_scope = Object.create(null);\n',
					'\t', '_scopes.push({ type: "scope", v: _scope });\n'
				);
				break;
			
			case NativeOp.SCOPE_ASCEND.opcode:
				sb.push(
					'\t', 'if(_scopes.length === 1) { _ERROR.ascendFromGlobalScope(); }\n',
					'\t', '_scopes.pop();\n',
					'\t', '_scope = _scopes[_scopes.length-1];\n'
				);
				break;
			
			case NativeOp.SCOPE_ENTER.opcode:
				this.writePop(sb, '_tmp1', 'scope');
				sb.push('\t', '_scopes.push(_scope = _tmp1);\n');
				break;
			
			case NativeOp.SCOPE_EXIT.opcode:
				sb.push(
					'\t', 'if(_stacks.length === 1) { _ERROR.ascendFromGlobalScope(); }\n',
					'\t', '_stack.push({ type: "scope", v: _scopes.pop() });\n',
					'\t', '_scope = _scopes[_scopes.length-1];\n'
				);
				break;
			
			case NativeOp.NOW.opcode:
				this.writePop(sb, '_tmp1', 'quote');
				sb.push('\t', '_tmp1.q();');
				break;
			
			case NativeOp.TRUE.opcode:
				sb.push('\t', '_stack.push({ type: "boolean", v: true });\n');
				break;
			
			case NativeOp.FALSE.opcode:
				sb.push('\t', '_stack.push({ type: "boolean", v: false });\n');
				break;
			
			case NativeOp.IMPORT.opcode:
				// TODO
				throw new Error('Not implemented');
				break;
			
			case NativeOp.IMPORT_AS.opcode:
				// TODO
				throw new Error('Not implemented');
				break;
			
			case NativeOp.EXPORT.opcode:
				// TODO
				throw new Error('Not implemented');
				break;
			
			case NativeOp.PRINT.opcode:
				this.writePop(sb, '_tmp1');
				sb.push(
					'\t', 'if(_tmp1.type !== "int" && _tmp1.type !== "double" && _tmp1.type !== "string") { _ERROR.printNotSupported(_tmp1.type); }\n',
					'\t', '_OUT(_tmp1.v.toString());\n'
				);
				break;
			
			case NativeOp.PRINTLN.opcode:
				this.writePop(sb, '_tmp1');
				sb.push(
					'\t', 'if(_tmp1.type !== "int" && _tmp1.type !== "double" && _tmp1.type !== "string") { _ERROR.printNotSupported(_tmp1.type); }\n',
					'\t', '_OUT(_tmp1.v.toString() + "\\n");\n'
				);
				break;
			
			case NativeOp.DEL.opcode:
				sb.push('_stack.pop() || _ERROR.emptyStack();\n');
				break;
			
			case NativeOp.PUSH.opcode:
				this.writePop(sb, '_tmp1');
				sb.push('\t', '_tmp2 = _stack[_stack.length-1] || _ERROR.peekEmptyStack();\n');
				this.writeTypeCheck(sb, '_tmp1', 'stack');
				sb.push('\t', '_tmp2.push(_tmp1);\n');
				break;
			
			case NativeOp.POP.opcode:
				sb.push('\t', '_tmp1 = _stack[_stack.length-1] || _ERROR.peekEmptyStack();\n');
				this.writeTypeCheck(sb, '_tmp1', 'stack');
				sb.push('\t', '_stack.push(_tmp1.pop() || _ERROR.emptyStack());\n');
				break;
			
			case NativeOp.LEN.opcode:
				this.writePop(sb, '_tmp1', 'stack');
				sb.push(
					'\t', '_stack.push({ type: "int", v: _tmp1.v.length });\n'
				);
				break;
			
			case NativeOp.GET.opcode:
				this.writePop(sb, '_tmp1', 'int');
				sb.push(
					'\t', '_tmp1 = _tmp1.v;\n',
					'\t', 'if(_tmp1 < 0 || _tmp1 >= _stack.length) { _ERROR.indexOutOfBounds(_tmp1, _stack.length); }\n',
					'\t', '_stack.push(_stack[_tmp1]);\n'
				);
				break;
			
			case NativeOp.AND.opcode:
				this.writePop(sb, '_tmp2', 'boolean');
				this.writePop(sb, '_tmp1', 'boolean');
				sb.push('\t', '_stack.push({ type: "boolean", v: _tmp1.v && _tmp2.v });\n');
				break;
			
			case NativeOp.OR.opcode:
				this.writePop(sb, '_tmp2', 'boolean');
				this.writePop(sb, '_tmp1', 'boolean');
				sb.push('\t', '_stack.push({ type: "boolean", v: _tmp1.v || _tmp2.v });\n');
				break;
			
			case NativeOp.NOT.opcode:
				this.writePop(sb, '_tmp1', 'boolean');
				sb.push('\t', '_stack.push({ type: "boolean", v: !_tmp1.v });\n');
				break;
			
			case NativeOp.IF.opcode:
				this.writePop(sb, '_tmp1', 'boolean');
				this.writePop(sb, '_tmp2', 'quote');
				sb.push(
					'\t', 'if(_tmp1.v) { _tmp2.q(); }\n',
				);
				break;
			
			case NativeOp.REPEAT.opcode:
				this.writePop(sb, '_tmp1', 'int');
				this.writePop(sb, '_tmp2', 'quote');
				sb.push(
					'\t', '(function(n, f) {\n',
					'\t\t', 'for(var i = 0; i < n; ++i) { f(); }\n',
					'\t', '}(_tmp1.v, _tmp2.q));\n'
				);
				break;
			
			case NativeOp.THIS.opcode:
				sb.push('\t', '_stack.push({ type: "scope", v: _scope });\n');
				break;
			
			case NativeOp.STACK.opcode:
				sb.push('\t', '_stack.push({ type: "stack", v: _stack });\n');
				break;
			
			case NativeOp.ADD.opcode:
				// TODO: allow doubles
				this.writePop(sb, '_tmp2', 'int');
				this.writePop(sb, '_tmp1', 'int');
				sb.push('\t', '_stack.push({ type: "int", v: (_tmp1.v + _tmp2.v)|0 });\n');
				break;
			
			case NativeOp.SUBTRACT.opcode:
				// TODO: allow doubles
				this.writePop(sb, '_tmp2', 'int');
				this.writePop(sb, '_tmp1', 'int');
				sb.push('\t', '_stack.push({ type: "int", v: (_tmp1.v - _tmp2.v)|0 });\n');
				break;
			
			case NativeOp.MULTIPLY.opcode:
				// TODO: allow doubles
				this.writePop(sb, '_tmp2', 'int');
				this.writePop(sb, '_tmp1', 'int');
				sb.push('\t', '_stack.push({ type: "int", v: _NATIVE.imul(_tmp1.v, _tmp2.v) });\n');
				break;
			
			case NativeOp.POW.opcode:
				// TODO: allow doubles
				this.writePop(sb, '_tmp2', 'int');
				this.writePop(sb, '_tmp1', 'int');
				sb.push(
					'\t', '_stack.push({ type: "int", v: _NATIVE.ipow(_tmp1.v, _tmp2.v) });\n'
				);
				break;
			
			case NativeOp.DIVIDE.opcode:
				// TODO: allow doubles
				this.writePop(sb, '_tmp2', 'int');
				this.writePop(sb, '_tmp1', 'int');
				sb.push('\t', '_stack.push({ type: "int", v: _NATIVE.idiv(_tmp1.v, _tmp2.v) });\n');
				break;
			
			case NativeOp.MODULO.opcode:
				// TODO: allow doubles
				this.writePop(sb, '_tmp2', 'int');
				this.writePop(sb, '_tmp1', 'int');
				sb.push('\t', '_stack.push({ type: "int", v: _NATIVE.imod(_tmp1.v, _tmp2.v) });\n');
				break;
			
			case NativeOp.BIT_AND.opcode:
				// TODO: allow doubles
				this.writePop(sb, '_tmp2', 'int');
				this.writePop(sb, '_tmp1', 'int');
				sb.push('\t', '_stack.push({ type: "int", v: _tmp1.v & _tmp2.v });\n');
				break;
			
			case NativeOp.BIT_OR.opcode:
				// TODO: allow doubles
				this.writePop(sb, '_tmp2', 'int');
				this.writePop(sb, '_tmp1', 'int');
				sb.push('\t', '_stack.push({ type: "int", v: _tmp1.v | _tmp2.v });\n');
				break;
			
			case NativeOp.BIT_NEG.opcode:
				// TODO: allow doubles
				this.writePop(sb, '_tmp1', 'int');
				sb.push('\t', '_stack.push({ type: "int", v: ~_tmp1.v });\n');
				break;
			
			case NativeOp.BIT_XOR.opcode:
				// TODO: allow doubles
				this.writePop(sb, '_tmp2', 'int');
				this.writePop(sb, '_tmp1', 'int');
				sb.push(
					'\t', '_stack.push({ type: "int", v: _tmp1.v ^ _tmp2.v });\n'
				);
				break;
			
			case NativeOp.EQUALS.opcode:
				// TODO: allow doubles
				this.writePop(sb, '_tmp2', 'int');
				this.writePop(sb, '_tmp1', 'int');
				sb.push('\t', '_stack.push({ type: "boolean", v: _tmp1.v === _tmp2.v });\n');
				break;
			
			case NativeOp.LESS_THAN.opcode:
				// TODO: allow doubles
				this.writePop(sb, '_tmp2', 'int');
				this.writePop(sb, '_tmp1', 'int');
				sb.push('\t', '_stack.push({ type: "boolean", v: _tmp1.v < _tmp2.v });\n');
				break;
			
			case NativeOp.GREATER_THAN.opcode:
				// TODO: allow doubles
				this.writePop(sb, '_tmp2', 'int');
				this.writePop(sb, '_tmp1', 'int');
				sb.push('\t', '_stack.push({ type: "boolean", v: _tmp1.v > _tmp2.v });\n');
				break;
			
			case NativeOp.LESS_THAN_OR_EQUAL.opcode:
				// TODO: allow doubles
				this.writePop(sb, '_tmp2', 'int');
				this.writePop(sb, '_tmp1', 'int');
				sb.push('\t', '_stack.push({ type: "boolean", v: _tmp1.v <= _tmp2.v });\n');
				break;
			
			case NativeOp.GREATER_THAN_OR_EQUAL.opcode:
				// TODO: allow doubles
				this.writePop(sb, '_tmp2', 'int');
				this.writePop(sb, '_tmp1', 'int');
				sb.push('\t', '_stack.push({ type: "boolean", v: _tmp1.v >= _tmp2.v });\n');
				break;
			
			default:
				throw new Error('Bytecode error: illegal opcode `' + opCode + '`');
		}
	}
	
	private compileOpCodeWithConstant(index: number, opCode: string, constID: number, sb: Array<string|number>): void {
		sb.push('// opcode: `', opCode, '`, constID: ', constID, '\n');
		switch(opCode) {
			case NativeOp.CONST_QUOTE.opcode:
				if(constID < index) {
					throw new Error('Bytecode error: backward reference in bytecode array');
				}
				sb.push('\t', '_stack.push({ type: "quote", q: _q', constID, ' });\n');
				break;
			
			case NativeOp.CONST_INT.opcode:
				let v: string|number;
				v = this.jsonCodeObject.constants[constID];
				if(typeof v !== 'number' || v !== (v|0)) {
					throw new Error('Bytecode error: expected int constant at index ' + index + ', id ' + constID);
				}
				sb.push('\t', '_stack.push({ type: "int", v: ', v, ' });\n');
				break;
			
			case NativeOp.CONST_DOUBLE.opcode:
				v = this.jsonCodeObject.constants[constID];
				if(typeof v !== 'number') {
					throw new Error('Bytecode error: expected double constant at index ' + index + ', id ' + constID);
				}
				sb.push('\t', '_stack.push({ type: "double", v: ', v, ' });\n');
				break;
			
			case NativeOp.CONST_STRING.opcode:
				v = this.jsonCodeObject.constants[constID];
				if(typeof v !== 'string') {
					throw new Error('Bytecode error: expected string constant at index ' + index + ', id ' + constID);
				}
				sb.push('\t', '_stack.push({ type: "string", v: ', JSON.stringify(v), ' });\n');
				break;
			
			case NativeOp.STORE.opcode:
				let name: string = JSON.stringify(this.jsonCodeObject.names[constID]);
				sb.push('\t', '_scope[', name, '] = _stack.pop() || _ERROR.emptyStack();\n');
				break;
			
			case NativeOp.STORE_QUOTE.opcode:
				name = JSON.stringify(this.jsonCodeObject.names[constID]);
				sb.push('\t', '_scope[', name, '] = { type: "immediate_quote", q: _stack.pop() || _ERROR.emptyStack() };\n');
				break;
			
			case NativeOp.LOAD_FAST.opcode:
				name = JSON.stringify(this.jsonCodeObject.names[constID]);
				sb.push('\t', '_tmp1 = _scope[', name, '] || _ERROR.nameError(', name, ');\n',);
				this.writeImmediateQuote(sb, '_tmp1');
				break;
			
			case NativeOp.LOAD_SLOW.opcode:
				name = JSON.stringify(this.jsonCodeObject.names[constID]);
				sb.push('\t', '_tmp1 = _NATIVE.loadSlow(_scopes, ', name, ') || _ERROR.nameError(', name, ');\n',);
				this.writeImmediateQuote(sb, '_tmp1');
				break;
			
			default:
				throw new Error('Bytecode error: illegal opcode `' + opCode + '`');
		}
	}
	
	private writePop(sb: Array<string|number>, varName: TmpVarName, typeTag: Value['type']|null = null): void {
		sb.push('\t', varName, ' = _stack.pop() || _ERROR.emptyStack();\n');
		if(typeTag) {
			this.writeTypeCheck(sb, varName, typeTag);
		}
	}
	
	private writeTypeCheck(sb: Array<string|number>, varName: TmpVarName, typeTag: Value['type']): void {
		sb.push('\t', 'if(', varName, '.type !== "', typeTag, '") { _ERROR.wrongType(', varName, '.type, "', typeTag, '"); }\n');
	}
	
	private writeImmediateQuote(sb: Array<string|number>, varName: TmpVarName): void {
		sb.push('\t', 'if(_tmp1.type === "immediate_quote") { _tmp1.q(); } else { _stack.push(_tmp1); }\n');
	}
}
