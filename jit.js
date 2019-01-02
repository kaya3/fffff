"use strict";
var JITCompiler = /** @class */ (function () {
    function JITCompiler(jsonCodeObject) {
        this.jsonCodeObject = jsonCodeObject;
    }
    JITCompiler.prototype.compileAll = function () {
        var sb = [
            'var _stack = _NATIVE.stack(), _scope = _NATIVE.scope(), _stacks = [_stack], _scopes = [_scope];\n',
            '_scope.store("document", new JSObjectWrapper(document));\n',
            '_scope.store("window", new JSObjectWrapper(window));\n'
        ];
        for (var i = this.jsonCodeObject.bytecode.length - 1; i >= 0; --i) {
            sb.push('var _q', i, ' = function() {\n', '\t', 'var _tmp1, _tmp2;\n');
            this.compileByteCode(i, this.jsonCodeObject.bytecode[i], sb);
            sb.push('};\n');
        }
        sb.push('_q0();');
        console.log(sb.join(''));
        return new Function('_NATIVE', '_OUT', '_ERROR', sb.join(''));
    };
    JITCompiler.prototype.compileByteCode = function (index, bc, sb) {
        var pos = 0;
        while (pos < bc.length) {
            var c = bc[pos++];
            //sb.push('console.log("opCode: ' + c + '");\n');
            //sb.push('console.log({ stacks: _stacks, stack: _stack });\n');
            var op = NativeOp.getByOpCode(c);
            if (!op) {
                throw new Error('Bytecode error: illegal opcode `' + c + '` at index ' + index + ', position ' + (pos - 1));
            }
            else if (JITCompiler.opCodesWithConstants.indexOf(c) >= 0) {
                var constIDstr = extractID(index, bc, pos);
                var constID = parseInt(constIDstr);
                this.compileOpCodeWithConstant(index, c, constID, sb);
                pos += constIDstr.length;
            }
            else {
                this.compileOpCode(c, sb);
            }
        }
    };
    JITCompiler.prototype.compileOpCode = function (opCode, sb) {
        sb.push('// opcode: `', opCode, '`, op: ', NativeOp.getByOpCode(opCode).repr(), '\n');
        switch (opCode) {
            case NativeOp.STACK_DESCEND.opcode:
                sb.push('\t', '_stacks.push(_stack = _NATIVE.stack());\n');
                break;
            case NativeOp.STACK_ASCEND.opcode:
                sb.push('\t', 'if(_stacks.length === 1) { _ERROR.ascendFromGlobalStack(); }\n', '\t', '_stacks.pop();\n', '\t', '_stack = _stacks[_stacks.length-1];\n');
                break;
            case NativeOp.STACK_ENTER.opcode:
                this.writePop(sb, '_tmp1', 'stack');
                sb.push('\t', '_stacks.push(_stack = _tmp1);\n');
                break;
            case NativeOp.STACK_EXIT.opcode:
                sb.push('\t', 'if(_stacks.length === 1) { _ERROR.ascendFromGlobalStack(); }\n', '\t', '_tmp1 = _stacks.pop();\n', '\t', '_stack = _stacks[_stacks.length-1];\n', '\t', '_stack.push(_tmp1);\n');
                break;
            case NativeOp.SCOPE_DESCEND.opcode:
                sb.push('\t', '_scopes.push(_scope = _NATIVE.scope());\n');
                break;
            case NativeOp.SCOPE_ASCEND.opcode:
                sb.push('\t', 'if(_scopes.length === 1) { _ERROR.ascendFromGlobalScope(); }\n', '\t', '_scopes.pop();\n', '\t', '_scope = _scopes[_scopes.length-1];\n');
                break;
            case NativeOp.SCOPE_ENTER.opcode:
                this.writePop(sb, '_tmp1', 'scope');
                sb.push('\t', '_scopes.push(_scope = _tmp1);\n');
                break;
            case NativeOp.SCOPE_EXIT.opcode:
                sb.push('\t', 'if(_scopes.length === 1) { _ERROR.ascendFromGlobalScope(); }\n', '\t', '_stack.push(_scopes.pop());\n', '\t', '_scope = _scopes[_scopes.length-1];\n');
                break;
            case NativeOp.NOW.opcode:
                this.writePop(sb, '_tmp1', 'function');
                sb.push('\t', '_tmp1.q();');
                break;
            case NativeOp.TRUE.opcode:
                this.writePushNewValue(sb, 'true', 'boolean');
                break;
            case NativeOp.FALSE.opcode:
                this.writePushNewValue(sb, 'false', 'boolean');
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
                this.writePopAny(sb, '_tmp1');
                this.writePrintableTypeCheck(sb, '_tmp1');
                sb.push('\t', '_OUT(_tmp1.toString());\n');
                break;
            case NativeOp.PRINTLN.opcode:
                this.writePopAny(sb, '_tmp1');
                this.writePrintableTypeCheck(sb, '_tmp1');
                sb.push('\t', '_OUT(_tmp1.toString() + "\\n");\n');
                break;
            case NativeOp.DEL.opcode:
                sb.push('_stack.popAny();\n');
                break;
            case NativeOp.PUSH.opcode:
                this.writePopAny(sb, '_tmp1');
                this.writePeek(sb, '_tmp2', 'stack');
                sb.push('\t', '_tmp2.push(_tmp1);\n');
                break;
            case NativeOp.POP.opcode:
                this.writePeek(sb, '_tmp1', 'stack');
                sb.push('\t', '_stack.push(_tmp1.popAny());\n');
                break;
            case NativeOp.LEN.opcode:
                this.writePop(sb, '_tmp1', 'stack');
                this.writePushNewValue(sb, '_tmp1.length()', 'int');
                break;
            case NativeOp.GET.opcode:
                this.writePop(sb, '_tmp2', 'int');
                this.writePeek(sb, '_tmp1', 'stack');
                sb.push('\t', '_stack.push(_tmp1.getValue(_tmp2.v));\n');
                break;
            case NativeOp.AND.opcode:
                this.writePop(sb, '_tmp2', 'boolean');
                this.writePop(sb, '_tmp1', 'boolean');
                this.writePushNewValue(sb, '_tmp1.v && _tmp2.v', 'boolean');
                break;
            case NativeOp.OR.opcode:
                this.writePop(sb, '_tmp2', 'boolean');
                this.writePop(sb, '_tmp1', 'boolean');
                this.writePushNewValue(sb, '_tmp1.v || _tmp2.v', 'boolean');
                break;
            case NativeOp.NOT.opcode:
                this.writePop(sb, '_tmp1', 'boolean');
                this.writePushNewValue(sb, '!_tmp1.v', 'boolean');
                break;
            case NativeOp.IF.opcode:
                this.writePop(sb, '_tmp1', 'boolean');
                this.writePop(sb, '_tmp2', 'function');
                sb.push('\t', 'if(_tmp1.v) { _tmp2.q(); }\n');
                break;
            case NativeOp.REPEAT.opcode:
                this.writePop(sb, '_tmp1', 'int');
                this.writePop(sb, '_tmp2', 'function');
                sb.push('\t', '(function(n, f) {\n', '\t\t', 'for(var i = 0; i < n; ++i) { f(); }\n', '\t', '}(_tmp1.v, _tmp2.q));\n');
                break;
            case NativeOp.THIS.opcode:
                sb.push('\t', '_stack.push(_scope);\n');
                break;
            case NativeOp.STACK.opcode:
                sb.push('\t', '_stack.push(_stack);\n');
                break;
            case NativeOp.ADD.opcode:
                // TODO: allow doubles
                this.writePop(sb, '_tmp2', 'int');
                this.writePop(sb, '_tmp1', 'int');
                this.writePushNewValue(sb, '(_tmp1.v + _tmp2.v)|0', 'int');
                break;
            case NativeOp.SUBTRACT.opcode:
                // TODO: allow doubles
                this.writePop(sb, '_tmp2', 'int');
                this.writePop(sb, '_tmp1', 'int');
                this.writePushNewValue(sb, '(_tmp1.v - _tmp2.v)|0', 'int');
                break;
            case NativeOp.MULTIPLY.opcode:
                // TODO: allow doubles
                this.writePop(sb, '_tmp2', 'int');
                this.writePop(sb, '_tmp1', 'int');
                this.writePushNewValue(sb, '_NATIVE.imul(_tmp1.v, _tmp2.v)', 'int');
                break;
            case NativeOp.POW.opcode:
                // TODO: allow doubles
                this.writePop(sb, '_tmp2', 'int');
                this.writePop(sb, '_tmp1', 'int');
                this.writePushNewValue(sb, '_NATIVE.ipow(_tmp1.v, _tmp2.v)', 'int');
                break;
            case NativeOp.DIVIDE.opcode:
                // TODO: allow doubles
                this.writePop(sb, '_tmp2', 'int');
                this.writePop(sb, '_tmp1', 'int');
                this.writePushNewValue(sb, '_NATIVE.idiv(_tmp1.v, _tmp2.v)', 'int');
                break;
            case NativeOp.MODULO.opcode:
                // TODO: allow doubles
                this.writePop(sb, '_tmp2', 'int');
                this.writePop(sb, '_tmp1', 'int');
                this.writePushNewValue(sb, '_NATIVE.imod(_tmp1.v, _tmp2.v)', 'int');
                break;
            case NativeOp.BIT_AND.opcode:
                this.writePop(sb, '_tmp2', 'int');
                this.writePop(sb, '_tmp1', 'int');
                this.writePushNewValue(sb, '_tmp1.v & _tmp2.v', 'int');
                break;
            case NativeOp.BIT_OR.opcode:
                this.writePop(sb, '_tmp2', 'int');
                this.writePop(sb, '_tmp1', 'int');
                this.writePushNewValue(sb, '_tmp1.v | _tmp2.v', 'int');
                break;
            case NativeOp.BIT_NEG.opcode:
                this.writePop(sb, '_tmp1', 'int');
                this.writePushNewValue(sb, '~_tmp1.v', 'int');
                break;
            case NativeOp.BIT_LSHIFT.opcode:
                this.writePop(sb, '_tmp2', 'int');
                this.writePop(sb, '_tmp1', 'int');
                this.writePushNewValue(sb, '_tmp1.v << _tmp2.v', 'int');
                break;
            case NativeOp.BIT_RSHIFT.opcode:
                this.writePop(sb, '_tmp2', 'int');
                this.writePop(sb, '_tmp1', 'int');
                this.writePushNewValue(sb, '_tmp1.v >> _tmp2.v', 'int');
                break;
            case NativeOp.BIT_URSHIFT.opcode:
                this.writePop(sb, '_tmp2', 'int');
                this.writePop(sb, '_tmp1', 'int');
                this.writePushNewValue(sb, '_tmp1.v >>> _tmp2.v', 'int');
                break;
            case NativeOp.EQUALS.opcode:
                // TODO: allow doubles
                this.writePop(sb, '_tmp2', 'int');
                this.writePop(sb, '_tmp1', 'int');
                this.writePushNewValue(sb, '_tmp1.v === _tmp2.v', 'boolean');
                break;
            case NativeOp.LESS_THAN.opcode:
                // TODO: allow doubles
                this.writePop(sb, '_tmp2', 'int');
                this.writePop(sb, '_tmp1', 'int');
                this.writePushNewValue(sb, '_tmp1.v < _tmp2.v', 'boolean');
                break;
            case NativeOp.GREATER_THAN.opcode:
                // TODO: allow doubles
                this.writePop(sb, '_tmp2', 'int');
                this.writePop(sb, '_tmp1', 'int');
                this.writePushNewValue(sb, '_tmp1.v > _tmp2.v', 'boolean');
                break;
            case NativeOp.LESS_THAN_OR_EQUAL.opcode:
                // TODO: allow doubles
                this.writePop(sb, '_tmp2', 'int');
                this.writePop(sb, '_tmp1', 'int');
                this.writePushNewValue(sb, '_tmp1.v <= _tmp2.v', 'boolean');
                break;
            case NativeOp.GREATER_THAN_OR_EQUAL.opcode:
                // TODO: allow doubles
                this.writePop(sb, '_tmp2', 'int');
                this.writePop(sb, '_tmp1', 'int');
                this.writePushNewValue(sb, '_tmp1.v >= _tmp2.v', 'boolean');
                break;
            default:
                throw new Error('Bytecode error: illegal opcode `' + opCode + '`');
        }
    };
    JITCompiler.prototype.compileOpCodeWithConstant = function (index, opCode, constID, sb) {
        sb.push('// opcode: `', opCode, '`, constID: ', constID, '\n');
        switch (opCode) {
            case NativeOp.CONST_QUOTE.opcode:
                if (constID < index) {
                    throw new Error('Bytecode error: backward reference in bytecode array');
                }
                // can't use writePush because has q: property, not v:
                sb.push('\t', '_stack.push({ type: "function", q: _q', constID, ' });\n');
                break;
            case NativeOp.CONST_INT.opcode:
                var v = void 0;
                v = this.jsonCodeObject.constants[constID];
                if (typeof v !== 'number' || v !== (v | 0)) {
                    throw new Error('Bytecode error: expected int constant at index ' + index + ', id ' + constID);
                }
                this.writePushNewValue(sb, v, 'int');
                break;
            case NativeOp.CONST_DOUBLE.opcode:
                v = this.jsonCodeObject.constants[constID];
                if (typeof v !== 'number') {
                    throw new Error('Bytecode error: expected double constant at index ' + index + ', id ' + constID);
                }
                this.writePushNewValue(sb, v, 'double');
                break;
            case NativeOp.CONST_STRING.opcode:
                v = this.jsonCodeObject.constants[constID];
                if (typeof v !== 'string') {
                    throw new Error('Bytecode error: expected string constant at index ' + index + ', id ' + constID);
                }
                this.writePushNewValue(sb, JSON.stringify(v), 'string');
                break;
            case NativeOp.DUP.opcode:
                if (constID === 0) {
                    throw new Error('Bytecode error: cannot DUP 0 at index ' + index);
                }
                sb.push('\t', '_stack.push(_stack.getValue(_stack.length() - ', constID, '));\n');
                break;
            case NativeOp.STORE.opcode:
                var name_1 = JSON.stringify(this.jsonCodeObject.names[constID]);
                sb.push('\t', '_scope.store(', name_1, ', _stack.popAny());\n');
                break;
            case NativeOp.STORE_QUOTE.opcode:
                name_1 = JSON.stringify(this.jsonCodeObject.names[constID]);
                this.writePop(sb, '_tmp1', 'function');
                sb.push('\t', '_scope.store(', name_1, ', { type: "immediate_function", q: _tmp1.q });\n');
                break;
            case NativeOp.LOAD_FAST.opcode:
                name_1 = JSON.stringify(this.jsonCodeObject.names[constID]);
                sb.push('\t', '_tmp1 = _scope.load(', name_1, ') || _ERROR.nameError(', name_1, ');\n');
                this.writeJustLoaded(sb, '_tmp1');
                break;
            case NativeOp.LOAD_SLOW.opcode:
                name_1 = JSON.stringify(this.jsonCodeObject.names[constID]);
                sb.push('\t', '_tmp1 = _NATIVE.loadSlow(_scopes, ', name_1, ');\n');
                this.writeJustLoaded(sb, '_tmp1');
                break;
            default:
                throw new Error('Bytecode error: illegal opcode `' + opCode + '`');
        }
    };
    JITCompiler.prototype.writePushNewValue = function (sb, valueCode, typeTag) {
        sb.push('\t', '_stack.push(_NATIVE.', typeTag, '(', valueCode, '));\n');
    };
    JITCompiler.prototype.writePopAny = function (sb, varName) {
        sb.push('\t', varName, ' = _stack.popAny();\n');
    };
    JITCompiler.prototype.writePop = function (sb, varName, typeTag) {
        sb.push('\t', varName, ' = _stack.pop("', typeTag, '");\n');
    };
    JITCompiler.prototype.writePeekAny = function (sb, varName) {
        sb.push('\t', varName, ' = _stack.peekAny();\n');
    };
    JITCompiler.prototype.writePeek = function (sb, varName, typeTag) {
        sb.push('\t', varName, ' = _stack.peek("', typeTag, '");\n');
    };
    JITCompiler.prototype.writeTypeCheck = function (sb, varName, typeTag) {
        sb.push('\t', 'if(', varName, '.type !== "', typeTag, '") { _ERROR.wrongType(', varName, '.type, "', typeTag, '"); }\n');
    };
    JITCompiler.prototype.writePrintableTypeCheck = function (sb, varName) {
        sb.push('\t', 'if(', varName, '.type !== "int" && ', varName, '.type !== "double" && ', varName, '.type !== "string" && ', varName, '.type !== "boolean" && ', varName, '.type !== "stack") { _ERROR.printNotSupported(', varName, '.type); }\n');
    };
    JITCompiler.prototype.writeJustLoaded = function (sb, varName) {
        sb.push('\t', 'if(', varName, '.type === "immediate_function") {\n', '\t\t', varName, '.q();\n', '\t', '} else if(', varName, '.type === "js_function") {\n', '\t\t', '_stack.push(_NATIVE.wrapJSFunction(', varName, ', _stacks));\n', '\t', '} else {\n', '\t\t', '_stack.push(', varName, ');\n', '\t', '}\n');
    };
    JITCompiler.opCodesWithConstants = NativeOp.CONST_INT.opcode + NativeOp.CONST_DOUBLE.opcode + NativeOp.CONST_STRING.opcode + NativeOp.CONST_QUOTE.opcode + NativeOp.DUP.opcode + NativeOp.STORE.opcode + NativeOp.STORE_QUOTE.opcode + NativeOp.LOAD_FAST.opcode + NativeOp.LOAD_SLOW.opcode;
    return JITCompiler;
}());
var isArray = (Array.isArray || function (arg) {
    return Object.prototype.toString.call(arg) === '[object Array]';
});
var JSObjectWrapper = /** @class */ (function () {
    function JSObjectWrapper(v) {
        this.v = v;
        this.type = 'js_object';
    }
    JSObjectWrapper.prototype.load = function (name) {
        if (name in this.v) {
            return this.wrap(this.v[name], true);
        }
        else {
            return null;
        }
    };
    JSObjectWrapper.prototype.store = function (name, v) {
        this.v[name] = this.unwrap(v);
    };
    JSObjectWrapper.prototype.toString = function () {
        return '<native object>';
    };
    JSObjectWrapper.prototype.wrap = function (v, allowFunction) {
        if (allowFunction === void 0) { allowFunction = false; }
        if (typeof v === 'string') {
            return _NATIVE.string(v);
        }
        else if (typeof v === 'boolean') {
            return _NATIVE.boolean(v);
        }
        else if (typeof v === 'number') {
            // TODO: this is not reliable
            return v === (v | 0) ? _NATIVE.int(v) : _NATIVE.double(v);
        }
        else if (isArray(v)) {
            var s = _NATIVE.stack();
            for (var i = 0; i < v.length; ++i) {
                s.push(this.wrap(v[i]));
            }
            return s;
        }
        else if (v && v.constructor && v.call && v.apply && v.bind) {
            if (allowFunction) {
                return { type: 'js_function', q: v, wrapper: this };
            }
            else {
                throw new Error('Illegal state: cannot wrap JS function');
            }
        }
        else {
            return new JSObjectWrapper(v);
        }
    };
    JSObjectWrapper.prototype.unwrap = function (v) {
        var _this = this;
        switch (v.type) {
            case 'boolean':
            case 'int':
            case 'double':
            case 'string':
            case 'js_object':
                return v.v;
            case 'function':
                return v.q;
            case 'stack':
                return v.v.map(function (vv) { return _this.unwrap(vv); });
            case 'scope':
            // TODO?
            default:
                throw new Error("Illegal state: can't unwrap " + v.type);
        }
    };
    return JSObjectWrapper;
}());
