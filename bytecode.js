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
    CodeObject.prototype.toJSON = function () {
        return {
            bytecode: this.byteCodes.map(function (bc) { return bc.compiled; }),
            constants: this.primitives.map(function (p) { return p.v; })
        };
    };
    return CodeObject;
}());
function decompile(jsonCodeObject, index) {
    if (index === void 0) { index = 0; }
    var bc = jsonCodeObject.bytecode[index];
    var q = new Quote();
    var pos = 0;
    var _loop_1 = function () {
        var c = bc[pos++];
        var op = NativeOp.getByOpCode(c);
        if (op === null) {
            throw new Error('Bytecode error: illegal opcode `' + c + '` at index ' + index + ', position ' + (pos - 1));
        }
        else if (op.name === null) {
            var startPos = pos;
            while (/[0-9]/.test(bc[pos]) && ++pos < bc.length)
                ;
            if (startPos === pos) {
                throw new Error('Bytecode error: expected numerical index at index ' + index + ', position ' + pos);
            }
            var constID_1 = parseInt(bc.substring(startPos, pos));
            if (c === NativeOp.CONST_QUOTE.opcode) {
                if (constID_1 < index) {
                    throw new Error('Bytecode error: backward reference in bytecode array');
                }
                op = new PushOp(decompile(jsonCodeObject, constID_1));
            }
            else {
                var v_1 = jsonCodeObject.constants[constID_1];
                reqString = function () {
                    if (typeof v_1 !== 'string') {
                        throw new Error('Bytecode error: expected string constant at index ' + index + ', id ' + constID_1);
                    }
                    return v_1;
                };
                switch (c) {
                    case NativeOp.CONST_INT.opcode:
                        if (typeof v_1 !== 'number' || v_1 !== (v_1 | 0)) {
                            throw new Error('Bytecode error: expected int constant at index ' + index + ', id ' + constID_1);
                        }
                        op = new PushOp(new IntValue(v_1));
                        break;
                    case NativeOp.CONST_DOUBLE.opcode:
                        if (typeof v_1 !== 'number') {
                            throw new Error('Bytecode error: expected double constant at index ' + index + ', id ' + constID_1);
                        }
                        op = new PushOp(new DoubleValue(v_1));
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
    };
    var reqString;
    while (pos < bc.length) {
        _loop_1();
    }
    return q;
}
