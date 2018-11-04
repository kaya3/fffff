"use strict";
var PushOp = /** @class */ (function () {
    function PushOp(v) {
        this.v = v;
    }
    PushOp.prototype.compile = function (constants) {
        if (this.v.type === 'quote') {
            return constants.addQuote(this.v);
        }
        else if (this.v.type === 'int' || this.v.type === 'double' || this.v.type === 'string') {
            return constants.addPrimitive(this.v.v, this.v.type);
        }
        else {
            throw new Error('Illegal state: compile-time constant must be int, double, string or quote');
        }
    };
    PushOp.prototype.repr = function () {
        return this.v.repr();
    };
    return PushOp;
}());
var QuotedOp = /** @class */ (function () {
    function QuotedOp(q) {
        this.q = q;
    }
    QuotedOp.prototype.compile = function (constants) {
        throw new Error('Illegal state: QuotedOp should not occur at compile-time');
    };
    QuotedOp.prototype.repr = function () {
        return this.q.repr() + '!';
    };
    return QuotedOp;
}());
var AssignOp = /** @class */ (function () {
    function AssignOp(name, doNow) {
        this.name = name;
        this.doNow = doNow;
    }
    AssignOp.prototype.compile = function (constants) {
        return (this.doNow ? NativeOp.STORE_QUOTE : NativeOp.STORE).opcode + constants.addName(this.name);
    };
    AssignOp.prototype.repr = function () {
        return (this.doNow ? '>!' : '>') + this.name;
    };
    return AssignOp;
}());
var CommentOp = /** @class */ (function () {
    function CommentOp(s) {
        this.s = s;
    }
    CommentOp.prototype.compile = function (constants) {
        return '';
    };
    CommentOp.prototype.repr = function () {
        return '#' + this.s + '\n';
    };
    return CommentOp;
}());
var ReadOp = /** @class */ (function () {
    function ReadOp(name) {
        this.name = name;
    }
    ReadOp.prototype.compile = function (constants) {
        return NativeOp.LOAD_SLOW.compile(constants) + constants.addName(this.name);
    };
    ReadOp.prototype.repr = function () {
        return this.name;
    };
    return ReadOp;
}());
var LocalReadOp = /** @class */ (function () {
    function LocalReadOp(name) {
        this.name = name;
    }
    LocalReadOp.prototype.compile = function (constants) {
        return NativeOp.LOAD_FAST.compile(constants) + constants.addName(this.name);
    };
    LocalReadOp.prototype.repr = function () {
        return this.name;
    };
    return LocalReadOp;
}());
var NativeOp = /** @class */ (function () {
    function NativeOp(name, opcode) {
        this.name = name;
        this.opcode = opcode;
        if (name !== null) {
            NativeOp.byName[name] = this;
        }
        if (Object.prototype.hasOwnProperty.call(NativeOp.byOpCode, opcode)) {
            throw new Error('Illegal state: opcode `' + opcode + '` is already defined');
        }
        NativeOp.byOpCode[opcode] = this;
    }
    NativeOp.getByName = function (name) {
        return NativeOp.byName[name] || null;
    };
    NativeOp.getByOpCode = function (opcode) {
        return NativeOp.byOpCode[opcode] || null;
    };
    NativeOp.prototype.compile = function (constants) {
        return this.opcode;
    };
    NativeOp.prototype.repr = function () {
        if (this.name === null) {
            throw new Error('Illegal state: opcode `' + this.opcode + '` should not exist in interactive mode');
        }
        return this.name;
    };
    NativeOp.byName = Object.create(null);
    NativeOp.byOpCode = Object.create(null);
    NativeOp.STACK_DESCEND = new NativeOp('[', 'd');
    NativeOp.STACK_ASCEND = new NativeOp(']', 'a');
    NativeOp.STACK_ENTER = new NativeOp('.[', 'e');
    NativeOp.STACK_EXIT = new NativeOp('].', 'x');
    NativeOp.SCOPE_DESCEND = new NativeOp('{', 'D');
    NativeOp.SCOPE_ASCEND = new NativeOp('}', 'A');
    NativeOp.SCOPE_ENTER = new NativeOp('.{', 'E');
    NativeOp.SCOPE_EXIT = new NativeOp('}.', 'X');
    NativeOp.NOW = new NativeOp('!', '!');
    NativeOp.STORE = new NativeOp(null, 's');
    NativeOp.STORE_QUOTE = new NativeOp(null, 'q');
    NativeOp.LOAD_FAST = new NativeOp(null, 'l');
    NativeOp.LOAD_SLOW = new NativeOp(null, 'L');
    NativeOp.CONST_QUOTE = new NativeOp(null, 'Q');
    NativeOp.CONST_INT = new NativeOp(null, 'j');
    NativeOp.CONST_DOUBLE = new NativeOp(null, 'J');
    NativeOp.CONST_STRING = new NativeOp(null, 'k');
    NativeOp.TRUE = new NativeOp('true', 't');
    NativeOp.FALSE = new NativeOp('false', 'f');
    // TODO: imports?
    NativeOp.IMPORT = new NativeOp('import', 'i');
    NativeOp.IMPORT_AS = new NativeOp('import_as', 'I');
    NativeOp.EXPORT = new NativeOp('export', '$');
    NativeOp.PRINT = new NativeOp('print', 'p');
    NativeOp.PRINTLN = new NativeOp('println', 'P');
    NativeOp.DEL = new NativeOp('del', '_');
    NativeOp.PUSH = new NativeOp('push', 'b');
    NativeOp.POP = new NativeOp('pop', 'B');
    NativeOp.LEN = new NativeOp('len', 'n');
    NativeOp.GET = new NativeOp('get', 'g');
    NativeOp.AND = new NativeOp('and', 'y');
    NativeOp.OR = new NativeOp('or', 'Y');
    NativeOp.NOT = new NativeOp('not', 'z');
    NativeOp.IF = new NativeOp('if', '?');
    NativeOp.REPEAT = new NativeOp('repeat', 'r');
    NativeOp.THIS = new NativeOp('this', 'T');
    NativeOp.STACK = new NativeOp('stack', 'S');
    NativeOp.ADD = new NativeOp('+', '+');
    NativeOp.SUBTRACT = new NativeOp('-', '-');
    NativeOp.MULTIPLY = new NativeOp('*', '*');
    NativeOp.POW = new NativeOp('**', 'w');
    NativeOp.DIVIDE = new NativeOp('/', '/');
    NativeOp.MODULO = new NativeOp('%', '%');
    NativeOp.BIT_AND = new NativeOp('&', '&');
    NativeOp.BIT_OR = new NativeOp('|', '|');
    NativeOp.BIT_NEG = new NativeOp('~', '~');
    NativeOp.BIT_XOR = new NativeOp('^', '^');
    NativeOp.EQUALS = new NativeOp('=', '=');
    NativeOp.LESS_THAN = new NativeOp('<', 'c');
    NativeOp.GREATER_THAN = new NativeOp('>', 'h');
    NativeOp.LESS_THAN_OR_EQUAL = new NativeOp('<=', 'C');
    NativeOp.GREATER_THAN_OR_EQUAL = new NativeOp('>=', 'H');
    return NativeOp;
}());
var _NATIVE = {
    imul: (Math.imul || function (a, b) {
        // Math.imul polyfill
        var aHi = (a >>> 16) & 0xffff;
        var aLo = a & 0xffff;
        var bHi = (b >>> 16) & 0xffff;
        var bLo = b & 0xffff;
        return ((aLo * bLo) + (((aHi * bLo + aLo * bHi) << 16) >>> 0) | 0);
    }),
    idiv: function (a, b) {
        if (b === 0) {
            _ERROR.arithmeticError(a + ' / ' + b);
        }
        return _NATIVE.trunc(a / b);
    },
    imod: function (a, b) {
        if (b === 0) {
            _ERROR.arithmeticError(a + ' % ' + b);
        }
        return a % b;
    },
    ipow: function (a, b) {
        if (b < 0 || (a === 0 && b === 0)) {
            _ERROR.arithmeticError(a + ' ** ' + b);
        }
        return Math.pow(a, b);
    },
    trunc: (Math.trunc || function (v) {
        // Math.trunc polyfill
        v = +v;
        if (!isFinite(v)) {
            return v;
        }
        return (v - v % 1) || (v < 0 ? -0 : v === 0 ? v : 0);
    }),
    loadSlow: function (scopes, name) {
        for (var i = scopes.length - 1; i >= 0; --i) {
            if (Object.prototype.hasOwnProperty.call(scopes[i], name)) {
                return scopes[i][name];
            }
        }
        _ERROR.nameError(name);
    }
};
var _ERROR = {
    wrongType: function (actualType, expectedType) {
        throw new Error('Type error: expected ' + expectedType + ', was ' + actualType);
    },
    printNotSupported: function (actualType) {
        throw new Error('Type error: expected string, int or double; was ' + actualType);
    },
    emptyStack: function () {
        throw new Error('Illegal state: pop from empty stack');
    },
    peekEmptyStack: function () {
        throw new Error('Illegal state: peek at empty stack');
    },
    ascendFromGlobalStack: function () {
        throw new Error("Illegal state: can't ascend from global stack");
    },
    ascendFromGlobalScope: function () {
        throw new Error("Illegal state: can't ascend from global scope");
    },
    nameError: function (name) {
        throw new Error('Name error: ' + name);
    },
    arithmeticError: function (msg) {
        throw new Error('Arithmetic error: ' + msg);
    },
    indexOutOfBounds: function (i, n) {
        throw new Error('Index out of bounds: ' + i + ' from length ' + n);
    }
};
