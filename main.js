"use strict";
var OpCall = /** @class */ (function () {
    function OpCall(op) {
        this.op = op;
        this._hasNext = true;
    }
    OpCall.prototype.hasNext = function () {
        return this._hasNext;
    };
    OpCall.prototype.next = function () {
        this._hasNext = false;
        return this.op;
    };
    OpCall.prototype.stackTrace = function (sb) {
        // do nothing
    };
    return OpCall;
}());
var QuoteCall = /** @class */ (function () {
    function QuoteCall(q) {
        this.q = q;
        this.pos = 0;
        // used to generate correct stack traces after tail-call optimisation
        this.calls = 1;
    }
    QuoteCall.prototype.hasNext = function () {
        return this.pos < this.q.ops.length;
    };
    QuoteCall.prototype.next = function () {
        return this.q.ops[this.pos++];
    };
    QuoteCall.prototype.stackTrace = function (sb) {
        while (--this.calls >= 0) {
            sb.push('\tat (');
            var glue = '';
            for (var i = 0; i < this.q.ops.length; ++i) {
                sb.push(glue);
                glue = ' ';
                if (i === this.pos - 1) {
                    sb.push('@');
                }
                sb.push(this.q.ops[i].repr());
            }
            sb.push(')\n');
            this.pos = this.q.ops.length;
        }
    };
    return QuoteCall;
}());
var RepeatCall = /** @class */ (function () {
    function RepeatCall(q, n) {
        this.n = n;
        this.i = 0;
        this.op = new QuotedOp(q);
    }
    RepeatCall.prototype.hasNext = function () {
        return this.i < this.n;
    };
    RepeatCall.prototype.next = function () {
        ++this.i;
        return this.op;
    };
    RepeatCall.prototype.stackTrace = function (sb) {
        sb.push('\tat iteration ' + this.i + ' of ' + this.n + '\n');
    };
    return RepeatCall;
}());
var Interpreter = /** @class */ (function () {
    function Interpreter(out) {
        if (out === void 0) { out = console.log; }
        this.out = out;
        this.callStack = [];
        this.scopeStack = [new Scope()];
        this.valueStacks = [new VStack()];
        this.exported = null;
    }
    Interpreter.prototype.stackTrace = function () {
        var sb = [];
        while (this.callStack.length) {
            this.callStack.pop().stackTrace(sb);
        }
        return sb.join('');
    };
    Interpreter.prototype.execQuote = function (q) {
        this.callStack.clear();
        this.callStack.push(new QuoteCall(q));
        while (this.callStack.length) {
            this.step();
        }
    };
    Interpreter.prototype.step = function () {
        var c = this.callStack.peek();
        if (!c.hasNext()) {
            this.callStack.pop();
            return;
        }
        var op = c.next();
        var vs = this.valueStacks.peek();
        if (op instanceof NativeOp) {
            switch (op.name) {
                case 'true':
                    vs.push(BoolValue.TRUE);
                    break;
                case 'false':
                    vs.push(BoolValue.FALSE);
                    break;
                case 'import':
                case 'import_as':
                case 'export':
                    // TODO
                    throw new Error('Not implemented');
                case '[':
                    this.valueStacks.push(new VStack());
                    break;
                case '.[':
                    this.valueStacks.push(vs.pop('stack'));
                    break;
                case ']':
                    if (this.valueStacks.length === 1) {
                        _ERROR.ascendFromGlobalStack();
                    }
                    this.valueStacks.pop();
                    break;
                case '].':
                    if (this.valueStacks.length === 1) {
                        _ERROR.ascendFromGlobalStack();
                    }
                    this.valueStacks.pop();
                    this.valueStacks.peek().push(vs);
                    break;
                case '{':
                    this.scopeStack.push(new Scope());
                    break;
                case '.{':
                    this.scopeStack.push(vs.pop('scope'));
                    break;
                case '}':
                    if (this.scopeStack.length === 1) {
                        _ERROR.ascendFromGlobalScope();
                    }
                    this.scopeStack.pop();
                    break;
                case '}.':
                    if (this.scopeStack.length === 1) {
                        _ERROR.ascendFromGlobalScope();
                    }
                    vs.push(this.scopeStack.pop());
                    break;
                case '!':
                    var q = vs.pop('quote');
                    if (c instanceof QuoteCall && !c.hasNext() && c.q === q) {
                        // tail recursion
                        c.pos = 0;
                        ++c.calls;
                    }
                    else {
                        this.callStack.push(new QuoteCall(q));
                    }
                    break;
                case 'print':
                    this.out(vs.popAny().toString());
                    break;
                case 'println':
                    this.out(vs.popAny().toString() + '\n');
                    break;
                case 'del':
                    vs.popAny();
                    break;
                case 'push':
                    var v = vs.popAny();
                    vs.peek('stack').push(v);
                    break;
                case 'pop':
                    var s = vs.peek('stack');
                    vs.push(s.popAny());
                    break;
                case 'len':
                    s = vs.pop('stack');
                    vs.push(new IntValue(s.length()));
                    break;
                case 'get':
                    var i = this.popInt();
                    vs.push(vs.getValue(i));
                    break;
                case 'and':
                    var b1 = this.popBool();
                    var b2 = this.popBool();
                    vs.push(b1 && b2 ? BoolValue.TRUE : BoolValue.FALSE);
                    break;
                case 'or':
                    b1 = this.popBool();
                    b2 = this.popBool();
                    vs.push(b1 || b2 ? BoolValue.TRUE : BoolValue.FALSE);
                    break;
                case 'not':
                    vs.push(this.popBool() ? BoolValue.FALSE : BoolValue.TRUE);
                    break;
                case 'if':
                    b1 = this.popBool();
                    q = vs.pop('quote');
                    if (b1) {
                        this.callStack.push(new QuoteCall(q));
                    }
                    break;
                case 'repeat':
                    var n = vs.pop('int').v;
                    q = vs.pop('quote');
                    this.callStack.push(new RepeatCall(q, n));
                    break;
                case 'this':
                    vs.push(this.scopeStack.peek());
                    break;
                case 'stack':
                    vs.push(vs);
                    break;
                case '+':
                    vs.push(new IntValue(this.popInt() + this.popInt()));
                    break;
                case '-':
                    vs.push(new IntValue((-this.popInt()) + this.popInt()));
                    break;
                case '*':
                    vs.push(new IntValue(_NATIVE.imul(this.popInt(), this.popInt())));
                    break;
                case '**':
                    var i2 = this.popInt();
                    var i1 = this.popInt();
                    vs.push(new IntValue(_NATIVE.ipow(i1, i2)));
                    break;
                case '/':
                    i2 = this.popInt();
                    i1 = this.popInt();
                    vs.push(new IntValue(_NATIVE.idiv(i1, i2)));
                    break;
                case '%':
                    i2 = this.popInt();
                    i1 = this.popInt();
                    vs.push(new IntValue(_NATIVE.imod(i1, i2)));
                    break;
                case '&':
                    vs.push(new IntValue(this.popInt() & this.popInt()));
                    break;
                case '|':
                    vs.push(new IntValue(this.popInt() | this.popInt()));
                    break;
                case '~':
                    vs.push(new IntValue(~this.popInt()));
                    break;
                // TODO: allow comparisons of other types
                case '=':
                    vs.push(_NATIVE.boolean(this.popInt() === this.popInt()));
                    break;
                // operands popped in reverse order
                case '<':
                    vs.push(_NATIVE.boolean(this.popInt() > this.popInt()));
                    break;
                case '>':
                    vs.push(_NATIVE.boolean(this.popInt() < this.popInt()));
                    break;
                case '<=':
                    vs.push(_NATIVE.boolean(this.popInt() >= this.popInt()));
                    break;
                case '>=':
                    vs.push(_NATIVE.boolean(this.popInt() <= this.popInt()));
                    break;
                default:
                    throw new Error('Illegal state: unknown operator ' + op);
            }
        }
        else if (op instanceof QuotedOp) {
            this.callStack.push(new QuoteCall(op.q));
        }
        else if (op instanceof PushOp) {
            // TODO: check read-only status of imported stacks
            vs.push(op.v);
        }
        else if (op instanceof AssignOp) {
            // TODO: check read-only status of imported scopes
            this.scopeStack.peek().doAssignment(op, vs.popAny());
        }
        else if (op instanceof ReadOp) {
            var name_1 = op.name;
            var doOp = null;
            var i = this.scopeStack.length;
            while (doOp === null) {
                if (--i >= 0) {
                    doOp = this.scopeStack[i].load(name_1);
                }
                else {
                    // TODO: builtins only need to be dynamically resolved if they can be overloaded
                    _ERROR.nameError(name_1);
                }
            }
            this.callStack.push(new OpCall(doOp));
        }
        else if (op instanceof LocalReadOp) {
            var name_2 = op.name;
            var doOp = this.scopeStack.peek().load(name_2) || _ERROR.nameError(name_2);
            ;
            this.callStack.push(new OpCall(doOp));
        }
        else if (op instanceof CommentOp) {
            // do nothing
        }
        else {
            // typecheck to make sure all ops covered
            var ignore = op;
            throw new Error('Illegal state: unknown operator ' + op);
        }
    };
    Interpreter.prototype.popBool = function () {
        return this.valueStacks.peek().pop('boolean').v;
    };
    Interpreter.prototype.popInt = function () {
        return this.valueStacks.peek().pop('int').v;
    };
    Interpreter.prototype.popDouble = function () {
        return this.valueStacks.peek().pop('double').v;
    };
    Interpreter.prototype.toString = function () {
        var sb = [];
        for (var i = 0; i < this.valueStacks.length; ++i) {
            sb.push(i, ': ', this.valueStacks[i].repr(), '\n');
        }
        for (var i = 0; i < this.scopeStack.length; ++i) {
            sb.push(i, ': ', this.scopeStack[i].repr(), '\n');
        }
        return sb.join('');
    };
    return Interpreter;
}());
