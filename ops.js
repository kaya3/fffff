"use strict";
var PushOp = /** @class */ (function () {
    function PushOp(v) {
        this.v = v;
    }
    PushOp.prototype.repr = function () {
        return this.v.repr();
    };
    return PushOp;
}());
var QuotedOp = /** @class */ (function () {
    function QuotedOp(q) {
        this.q = q;
        this.isStringing = false;
    }
    QuotedOp.prototype.repr = function () {
        if (this.isStringing) {
            return '(...)';
        }
        else {
            this.isStringing = true;
            var sb = [];
            var glue = '';
            for (var i = 0; i < this.q.ops.length; ++i) {
                sb.push(glue);
                glue = ' ';
                sb.push(this.q.ops[i].repr());
            }
            this.isStringing = false;
            sb.push(')');
            return sb.join('');
        }
    };
    return QuotedOp;
}());
var AssignOp = /** @class */ (function () {
    function AssignOp(name, doNow) {
        this.name = name;
        this.doNow = doNow;
    }
    AssignOp.prototype.repr = function () {
        return '>' + this.name;
    };
    return AssignOp;
}());
var CommentOp = /** @class */ (function () {
    function CommentOp(s) {
        this.s = s;
    }
    CommentOp.prototype.repr = function () {
        return '#' + this.s + '\n';
    };
    return CommentOp;
}());
var ReadOp = /** @class */ (function () {
    function ReadOp(name) {
        this.name = name;
    }
    ReadOp.prototype.repr = function () {
        return this.name;
    };
    return ReadOp;
}());
var LocalReadOp = /** @class */ (function () {
    function LocalReadOp(name) {
        this.name = name;
    }
    LocalReadOp.prototype.repr = function () {
        return this.name;
    };
    return LocalReadOp;
}());
var NativeOp = /** @class */ (function () {
    function NativeOp(name) {
        this.name = name;
        NativeOp.opsMap[name] = this;
        NativeOp.opsArray.push(this);
        this.opCode = NativeOp.opsArray.length;
    }
    NativeOp.getByOpCode = function (opCode) {
        if (opCode < 1 || opCode >= NativeOp.opsArray.length) {
            throw new Error('Illegal state: unknown opcode ' + opCode);
        }
        return NativeOp.opsArray[opCode - 1];
    };
    NativeOp.getByName = function (name) {
        return NativeOp.opsMap[name] || null;
    };
    NativeOp.prototype.repr = function () {
        return this.name;
    };
    NativeOp.opsArray = [];
    NativeOp.opsMap = Object.create(null);
    NativeOp.STACK_DESCEND = new NativeOp('[');
    NativeOp.STACK_ASCEND = new NativeOp(']');
    NativeOp.STACK_ENTER = new NativeOp('.[');
    NativeOp.STACK_EXIT = new NativeOp('].');
    NativeOp.SCOPE_DESCEND = new NativeOp('{');
    NativeOp.SCOPE_ASCEND = new NativeOp('}');
    NativeOp.SCOPE_ENTER = new NativeOp('.{');
    NativeOp.SCOPE_EXIT = new NativeOp('}.');
    NativeOp.NOW = new NativeOp('!');
    NativeOp.TRUE = new NativeOp('true');
    NativeOp.FALSE = new NativeOp('false');
    // TODO: imports?
    NativeOp.IMPORT = new NativeOp('import');
    NativeOp.IMPORT_AS = new NativeOp('import_as');
    NativeOp.EXPORT = new NativeOp('export');
    NativeOp.PRINT = new NativeOp('print');
    NativeOp.PRINTLN = new NativeOp('println');
    NativeOp.DEL = new NativeOp('del');
    NativeOp.PUSH = new NativeOp('push');
    NativeOp.POP = new NativeOp('pop');
    NativeOp.LEN = new NativeOp('len');
    NativeOp.GET = new NativeOp('get');
    NativeOp.AND = new NativeOp('and');
    NativeOp.OR = new NativeOp('or');
    NativeOp.NOT = new NativeOp('not');
    NativeOp.IF = new NativeOp('if');
    NativeOp.REPEAT = new NativeOp('repeat');
    NativeOp.THIS = new NativeOp('this');
    NativeOp.STACK = new NativeOp('stack');
    NativeOp.ADD = new NativeOp('+');
    NativeOp.SUBTRACT = new NativeOp('-');
    NativeOp.MULTIPLY = new NativeOp('*');
    NativeOp.POW = new NativeOp('**');
    NativeOp.DIVIDE = new NativeOp('/');
    NativeOp.MODULO = new NativeOp('%');
    NativeOp.BIT_AND = new NativeOp('&');
    NativeOp.BIT_OR = new NativeOp('|');
    NativeOp.BIT_NEG = new NativeOp('~');
    NativeOp.BIT_XOR = new NativeOp('^');
    NativeOp.EQUALS = new NativeOp('=');
    NativeOp.LESS_THAN = new NativeOp('<');
    NativeOp.GREATER_THAN = new NativeOp('>');
    NativeOp.LESS_THAN_OR_EQUAL = new NativeOp('<=');
    NativeOp.GREATER_THAN_OR_EQUAL = new NativeOp('>=');
    return NativeOp;
}());
