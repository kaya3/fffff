"use strict";
function typecheck(v, typeTag) {
    if (v.type === typeTag) {
        return v;
    }
    else {
        throw new Error('Type error: expected ' + typeTag + ', was ' + v.type);
    }
}
var StringValue = /** @class */ (function () {
    function StringValue(v) {
        this.v = v;
        this.type = 'string';
    }
    StringValue.prototype.repr = function () {
        var singleQuotes = 0;
        var doubleQuotes = 0;
        for (var i = 0; i < this.v.length; ++i) {
            switch (this.v[i]) {
                case '"':
                    ++doubleQuotes;
                    break;
                case "'":
                    ++singleQuotes;
                    break;
            }
        }
        var d = (singleQuotes <= doubleQuotes ? "'" : '"');
        var sb = [d];
        for (var i = 0; i < this.v.length; ++i) {
            var c = this.v[i];
            switch (c) {
                case '\n':
                    sb.push('\\n');
                    break;
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
                    if (c === d) {
                        sb.push("\\");
                    }
                    sb.push(c);
                    break;
                default:
                    var cc = c.charCodeAt(i);
                    if (cc < 0x20 || cc > 0x7F) {
                        sb.push('\\u');
                        sb.push(('0000' + cc.toString(16)).substr(-4));
                    }
                    else {
                        sb.push(c);
                    }
            }
        }
        sb.push(d);
        return sb.join('');
    };
    StringValue.prototype.toString = function () {
        return this.v;
    };
    return StringValue;
}());
var IntValue = /** @class */ (function () {
    function IntValue(v) {
        this.type = 'int';
        this.v = v | 0;
    }
    IntValue.prototype.repr = function () {
        return this.v.toString();
    };
    IntValue.prototype.toString = function () {
        return this.v.toString();
    };
    return IntValue;
}());
var BoolValue = /** @class */ (function () {
    function BoolValue(v) {
        this.v = v;
        this.type = 'boolean';
    }
    BoolValue.of = function (b) {
        return b ? BoolValue.TRUE : BoolValue.FALSE;
    };
    BoolValue.prototype.repr = function () {
        return this.v ? 'true' : 'false';
    };
    BoolValue.prototype.toString = function () {
        return this.v ? 'true' : 'false';
    };
    BoolValue.TRUE = new BoolValue(true);
    BoolValue.FALSE = new BoolValue(false);
    BoolValue.PUSH_TRUE = new PushOp(BoolValue.TRUE);
    BoolValue.PUSH_FALSE = new PushOp(BoolValue.FALSE);
    return BoolValue;
}());
var DoubleValue = /** @class */ (function () {
    function DoubleValue(v) {
        this.v = v;
        this.type = 'double';
    }
    DoubleValue.prototype.repr = function () {
        return this.v.toString();
    };
    DoubleValue.prototype.toString = function () {
        return this.v.toString();
    };
    return DoubleValue;
}());
var Quote = /** @class */ (function () {
    function Quote() {
        this.type = 'quote';
        this.ops = [];
        this.isStringing = false;
    }
    Quote.prototype.repr = function () {
        if (this.isStringing) {
            return "(...)";
        }
        else {
            this.isStringing = true;
            var sb = ['('];
            var glue = '';
            for (var i = 0; i < this.ops.length; ++i) {
                sb.push(glue);
                glue = ' ';
                sb.push(this.ops[i].repr());
            }
            sb.push(')');
            this.isStringing = false;
            return sb.join('');
        }
    };
    Quote.prototype.toString = function () {
        return this.repr();
    };
    return Quote;
}());
var VStack = /** @class */ (function () {
    function VStack() {
        this.type = 'stack';
        this.v = [];
        this.isStringing = false;
    }
    VStack.prototype.peek = function (typeTag) {
        return typecheck(this.v.peek(), typeTag);
    };
    VStack.prototype.pop = function (typeTag) {
        return typecheck(this.popAny(), typeTag);
    };
    VStack.prototype.peekAny = function () {
        return this.v.peek();
    };
    VStack.prototype.popAny = function () {
        if (this.v.length === 0) {
            throw new Error('Illegal state: pop from empty stack');
        }
        return this.v.pop();
    };
    VStack.prototype.get = function (index) {
        if (index < 0 || index >= this.v.length) {
            throw new Error('Illegal index: ' + index + ' from length ' + this.v.length);
        }
        return this.v[index];
    };
    VStack.prototype.repr = function () {
        if (this.isStringing) {
            return '[...].';
        }
        else {
            this.isStringing = true;
            var sb = ['['];
            var glue = '';
            for (var i = 0; i < this.v.length; ++i) {
                sb.push(glue);
                glue = ' ';
                sb.push(this.v[i].repr());
            }
            sb.push('].');
            this.isStringing = false;
            return sb.join('');
        }
    };
    VStack.prototype.toString = function () {
        return this.repr();
    };
    return VStack;
}());
var Scope = /** @class */ (function () {
    function Scope() {
        this.type = 'scope';
        this.v = Object.create(null);
        this.isStringing = false;
    }
    Scope.prototype.read = function (name) {
        if (Object.prototype.hasOwnProperty.call(this.v, name)) {
            return this.v[name];
        }
        else {
            return null;
        }
    };
    Scope.prototype.doAssignment = function (op, val) {
        if (op.doNow) {
            this.assignOp(op.name, val);
        }
        else {
            this.assignValue(op.name, val);
        }
    };
    Scope.prototype.assignOp = function (name, q) {
        this.v[name] = new QuotedOp(q);
    };
    Scope.prototype.assignValue = function (name, val) {
        this.v[name] = new PushOp(val);
    };
    Scope.prototype.repr = function () {
        if (this.isStringing) {
            return '{...}.';
        }
        else {
            this.isStringing = true;
            var sb = ['{'];
            var glue = '';
            for (var k in this.v) {
                if (Object.prototype.hasOwnProperty.call(this.v, k)) {
                    sb.push(glue);
                    glue = ' ';
                    var op = this.v[k];
                    if (op instanceof PushOp) {
                        sb.push(op.v.repr());
                        sb.push('>');
                    }
                    else {
                        sb.push(op.q.repr());
                        sb.push('>!');
                    }
                    sb.push(k);
                }
            }
            sb.push('}.');
            this.isStringing = false;
            return sb.join('');
        }
    };
    Scope.prototype.toString = function () {
        return this.repr();
    };
    return Scope;
}());
