"use strict";
var ConstantPrimitive = /** @class */ (function () {
    function ConstantPrimitive(v, type, constID) {
        this.v = v;
        this.type = type;
        this.constRef = (type === 'int'
            ? NativeOp.CONST_INT.opcode
            : type === 'double'
                ? NativeOp.CONST_DOUBLE.opcode
                : NativeOp.CONST_STRING.opcode) + constID;
    }
    return ConstantPrimitive;
}());
var ByteCode = /** @class */ (function () {
    function ByteCode(q, constants, constID) {
        this.constRef = NativeOp.CONST_QUOTE.opcode + constID;
        var sb = [];
        for (var i = 0; i < q.ops.length; ++i) {
            sb.push(q.ops[i].compile(constants));
        }
        this.compiled = sb.join('');
    }
    return ByteCode;
}());
var CodeObject = /** @class */ (function () {
    function CodeObject(q) {
        this.nextBCID = 0;
        this.names = [];
        this.primitives = [];
        this.byteCodes = [];
        this.addQuote(q);
    }
    CodeObject.prototype.addQuote = function (q) {
        var bcID = this.nextBCID++;
        var bc = new ByteCode(q, this, bcID);
        this.byteCodes[bcID] = bc;
        return bc.constRef;
    };
    CodeObject.prototype.addPrimitive = function (v, typeTag) {
        for (var i = 0; i < this.primitives.length; ++i) {
            var p_1 = this.primitives[i];
            if (p_1.type === typeTag && p_1.v === v) {
                return p_1.constRef;
            }
        }
        var p = new ConstantPrimitive(v, typeTag, this.primitives.length);
        this.primitives.push(p);
        return p.constRef;
    };
    CodeObject.prototype.addName = function (name) {
        var i = this.names.indexOf(name);
        if (i === -1) {
            i = this.names.length;
            this.names.push(name);
        }
        return i;
    };
    CodeObject.prototype.toJSON = function () {
        return {
            bytecode: this.byteCodes.map(function (bc) { return bc.compiled; }),
            constants: this.primitives.map(function (p) { return p.v; }),
            names: this.names
        };
    };
    return CodeObject;
}());
function extractID(index, bc, startPos) {
    var pos = startPos;
    while (/[0-9]/.test(bc[pos]) && ++pos < bc.length)
        ;
    if (startPos === pos) {
        throw new Error('Bytecode error: expected numerical index at index ' + index + ', position ' + pos);
    }
    return bc.substring(startPos, pos);
}
function decompile(jsonCodeObject, index) {
    if (index === void 0) { index = 0; }
    var bc = jsonCodeObject.bytecode[index];
    var q = new Quote();
    var pos = 0;
    while (pos < bc.length) {
        var c = bc[pos++];
        var op = NativeOp.getByOpCode(c);
        if (op === null) {
            throw new Error('Bytecode error: illegal opcode `' + c + '` at index ' + index + ', position ' + (pos - 1));
        }
        else if (op.name === null) {
            var constIDstr = extractID(index, bc, pos);
            pos += constIDstr.length;
            var constID = parseInt(constIDstr);
            if (c === NativeOp.CONST_QUOTE.opcode) {
                if (constID < index) {
                    throw new Error('Bytecode error: backward reference in bytecode array');
                }
                op = new PushOp(decompile(jsonCodeObject, constID));
            }
            else if (c === NativeOp.CONST_INT.opcode) {
                var v = jsonCodeObject.constants[constID];
                if (typeof v !== 'number' || v !== (v | 0)) {
                    throw new Error('Bytecode error: expected int constant at index ' + index + ', id ' + constID);
                }
                op = new PushOp(new IntValue(v));
            }
            else if (c === NativeOp.CONST_DOUBLE.opcode) {
                var v = jsonCodeObject.constants[constID];
                if (typeof v !== 'number') {
                    throw new Error('Bytecode error: expected double constant at index ' + index + ', id ' + constID);
                }
                op = new PushOp(new DoubleValue(v));
            }
            else if (c === NativeOp.CONST_STRING.opcode) {
                var v = jsonCodeObject.constants[constID];
                if (typeof v !== 'string') {
                    throw new Error('Bytecode error: expected string constant at index ' + index + ', id ' + constID);
                }
                op = new PushOp(new StringValue(v));
            }
            else if (c === NativeOp.DUP.opcode) {
                if (constID === 0) {
                    throw new Error('Bytecode error: cannot DUP 0 at index ' + index);
                }
                op = new DupOp(constID);
            }
            else {
                var name_1 = jsonCodeObject.names[constID];
                switch (c) {
                    case NativeOp.STORE.opcode:
                        op = new AssignOp(name_1, false);
                        break;
                    case NativeOp.STORE_QUOTE.opcode:
                        op = new AssignOp(name_1, true);
                        break;
                    case NativeOp.LOAD_FAST.opcode:
                        op = new LocalReadOp(name_1);
                        break;
                    case NativeOp.LOAD_SLOW.opcode:
                        op = new ReadOp(name_1);
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
