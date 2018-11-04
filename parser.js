"use strict";
Array.prototype.peek = function () {
    if (this.length === 0) {
        throw new Error('Illegal state: peek at empty stack');
    }
    return this[this.length - 1];
};
Array.prototype.clear = function () {
    // this is allowed because JavaScript is silly
    this.length = 0;
};
var Parser = /** @class */ (function () {
    function Parser(keepComments) {
        if (keepComments === void 0) { keepComments = false; }
        this.keepComments = keepComments;
        this.src = '';
        this.quotes = [];
        this.pos = 0;
        this.stringLiteralBuilder = null;
        this.stringLiteralDelimiter = null;
    }
    Parser.parse = function (src, keepComments) {
        if (keepComments === void 0) { keepComments = false; }
        var p = new Parser(keepComments);
        p.read(src);
        var q = p.parse();
        if (q === null) {
            throw new Error('Syntax error: unexpected end of source');
        }
        return q;
    };
    Parser.prototype.read = function (src) {
        this.src = src;
        this.pos = 0;
    };
    Parser.prototype.parse = function () {
        if (this.quotes.length === 0) {
            this.quotes.push(new Quote());
            this.stringLiteralBuilder = null;
            this.stringLiteralDelimiter = null;
        }
        else if (this.stringLiteralBuilder != null) {
            this.stepStringLiteral(this.quotes.peek());
        }
        while (this.pos < this.src.length) {
            this.step();
        }
        if (this.quotes.length === 1 && this.stringLiteralBuilder === null) {
            return this.quotes.pop();
        }
        else {
            return null;
        }
    };
    Parser.prototype.step = function () {
        var c = this.src[this.pos];
        var q = this.quotes.peek();
        if (/\s|\n/.test(c)) {
            this.stepWhitespace();
        }
        else if (c === '#') {
            var startPos = ++this.pos;
            this.stepComment();
            if (this.keepComments) {
                q.ops.push(new CommentOp(this.src.substring(startPos, this.pos)));
            }
        }
        else if (c === '(') {
            ++this.pos;
            this.quotes.push(new Quote());
        }
        else if (c === ')') {
            ++this.pos;
            if (this.quotes.length > 1) {
                this.quotes.pop();
                this.quotes.peek().ops.push(new PushOp(q));
            }
            else {
                throw new Error("Syntax error: unexpected character ) at position " + this.pos);
            }
        }
        else if (c === '!') {
            ++this.pos;
            q.ops.push(NativeOp.NOW);
        }
        else if (c === '.') {
            if (++this.pos === this.src.length) {
                throw new Error("Syntax error: unexpected end of source: expected .[ or .{ or .identifier");
            }
            c = this.src[this.pos];
            if (c === '[') {
                ++this.pos;
                q.ops.push(NativeOp.STACK_ENTER);
            }
            else if (c === '{') {
                ++this.pos;
                q.ops.push(NativeOp.SCOPE_ENTER);
            }
            else if (this.identifierChar(c)) {
                var name_1 = this.nextIdentifier(true);
                q.ops.push(NativeOp.SCOPE_ENTER);
                q.ops.push(new LocalReadOp(name_1));
                q.ops.push(NativeOp.SCOPE_ASCEND);
            }
            else {
                throw new Error("Syntax error: unexpected character " + c + " at position " + this.pos);
            }
        }
        else if (c === '[') {
            ++this.pos;
            q.ops.push(NativeOp.STACK_DESCEND);
        }
        else if (c === '{') {
            ++this.pos;
            q.ops.push(NativeOp.SCOPE_DESCEND);
        }
        else if (c === ']') {
            if (++this.pos === this.src.length || this.src[this.pos] !== '.') {
                q.ops.push(NativeOp.STACK_ASCEND);
            }
            else {
                ++this.pos;
                q.ops.push(NativeOp.STACK_EXIT);
            }
        }
        else if (c === '}') {
            if (++this.pos === this.src.length || this.src[this.pos] !== '.') {
                q.ops.push(NativeOp.SCOPE_ASCEND);
            }
            else {
                ++this.pos;
                q.ops.push(NativeOp.SCOPE_EXIT);
            }
        }
        else if (c === '"' || c === "'") {
            this.stepStringLiteral(q);
        }
        else if (this.digitChar(c) || (c === '-' && this.pos + 1 < this.src.length && this.digitChar(this.src[this.pos + 1]))) {
            q.ops.push(new PushOp(this.nextNumber()));
        }
        else if (c === '>' && this.pos + 1 < this.src.length && (this.identifierChar(this.src[this.pos + 1]) || this.src[this.pos + 1] === '!')) {
            var Assignment = /** @class */ (function () {
                function Assignment() {
                    this.names = [];
                    this.doNow = false;
                }
                return Assignment;
            }());
            // assign var
            var assignments = [];
            do {
                var a = new Assignment();
                if (this.src[++this.pos] === '!') {
                    a.doNow = true;
                    ++this.pos;
                }
                do {
                    a.names.push(this.nextIdentifier(true));
                } while (this.pos < this.src.length && this.src[this.pos] === '.' && ++this.pos < this.src.length /* skip the . */);
                assignments.push(a);
            } while (this.pos < this.src.length && this.src[this.pos] === ',');
            while (assignments.length) {
                var a = assignments.pop();
                var scopes = 0;
                while (true) {
                    var name_2 = a.names.shift();
                    if (!a.names.length) {
                        q.ops.push(new AssignOp(name_2, a.doNow));
                        break;
                    }
                    else {
                        q.ops.push(scopes === 0 ? new ReadOp(name_2) : new LocalReadOp(name_2));
                        q.ops.push(NativeOp.SCOPE_ENTER);
                        ++scopes;
                    }
                }
                while (--scopes >= 0) {
                    q.ops.push(NativeOp.SCOPE_ASCEND);
                }
            }
        }
        else {
            var name_3;
            if (this.identifierChar(c)) {
                name_3 = this.nextIdentifier(false);
            }
            else if (this.opChar(c)) {
                name_3 = this.nextOpName();
            }
            else {
                throw new Error("Syntax error: unexpected character " + c + " at position " + this.pos);
            }
            var op = NativeOp.getByName(name_3);
            if (op !== null) {
                q.ops.push(op);
            }
            else {
                // TODO: are built-in operators allowed to be overloaded?
                q.ops.push(new ReadOp(name_3));
            }
        }
    };
    Parser.prototype.stepWhitespace = function () {
        // TODO: search for next non-whitespace character after pos
        while (/\s|\n/.test(this.src[this.pos]) && ++this.pos < this.src.length)
            ;
    };
    Parser.prototype.stepComment = function () {
        while (this.pos < this.src.length && this.src[this.pos++] !== '\n')
            ;
    };
    Parser.prototype.digitChar = function (c) {
        return /[0-9]/.test(c);
    };
    Parser.prototype.identifierChar = function (c) {
        return /[a-zA-Z0-9_]/.test(c);
    };
    Parser.prototype.opChar = function (c) {
        // TODO: replace with regex?
        return c === '>' || c === '<' || c === '='
            || c === '?' || c === '\\'
            || c === '+' || c === '-' || c === '*' || c === '/' || c === '%'
            || c === '&' || c === '|' || c === '!' || c === '~' || c === '^';
    };
    Parser.prototype.nextNumber = function () {
        var startPos = this.pos;
        if (this.src[this.pos] === '-') {
            ++this.pos;
        }
        var hasExponent = false;
        var isDouble = false;
        while (this.pos < this.src.length) {
            var c = this.src[this.pos];
            if (c === '.') {
                if (isDouble) {
                    throw new Error("Syntax error: unexpected character . at position " + this.pos);
                }
                isDouble = true;
            }
            else if (c === 'e' || c === 'E') {
                if (hasExponent) {
                    throw new Error("Syntax error: unexpected character " + c + " at position " + this.pos);
                }
                hasExponent = true;
            }
            else if (!this.digitChar(c)) {
                break;
            }
            ++this.pos;
        }
        var n = this.src.substring(startPos, this.pos);
        try {
            if (isDouble) {
                // TODO
                throw new Error('Error: doubles are not implemented');
                return new DoubleValue(parseFloat(n));
            }
            else {
                // TODO: use bigint?
                return new IntValue(parseInt(n));
            }
        }
        catch (e) {
            throw new Error("Syntax error: invalid numeric literal " + n + " at position " + startPos);
        }
    };
    Parser.prototype.stepStringLiteral = function (q) {
        if (this.stringLiteralBuilder === null || this.stringLiteralDelimiter === null) {
            var d = this.src.charAt(this.pos++);
            this.stringLiteralDelimiter = d;
            if (this.src[this.pos] === d && this.src[this.pos + 1] === d) {
                this.pos += 2;
                this.stringLiteralDelimiter = d + d + d;
            }
            this.stringLiteralBuilder = [];
        }
        while (this.pos < this.src.length) {
            var c = this.src[this.pos++];
            switch (c) {
                case '\\':
                    if (this.pos === this.src.length) {
                        throw new Error("Syntax error: unexpected end of source: expected escape sequence in String literal");
                    }
                    switch (c = this.src[++this.pos]) {
                        case '"':
                        case "'":
                        case '\\':
                            this.stringLiteralBuilder.push(c);
                            break;
                        case 'n':
                            this.stringLiteralBuilder.push('\n');
                            break;
                        case 'r':
                            this.stringLiteralBuilder.push('\r');
                            break;
                        case 't':
                            this.stringLiteralBuilder.push('\t');
                            break;
                        case 'b':
                            this.stringLiteralBuilder.push('\b');
                            break;
                        case 'f':
                            this.stringLiteralBuilder.push('\f');
                            break;
                        case 'u':
                            if (this.pos + 4 > this.src.length) {
                                throw new Error("Syntax error: unexpected end of source: expected unicode escape sequence in String literal");
                            }
                            var hex = parseInt(this.src.substring(this.pos, this.pos + 4), 16);
                            this.stringLiteralBuilder.push(String.fromCharCode(hex));
                            this.pos += 4;
                            break;
                        default:
                            throw new Error("Syntax error: unknown escape sequence \\" + c + " in String literal at position " + this.pos);
                    }
                    break;
                case '\n':
                    if (this.stringLiteralDelimiter.length === 1) {
                        throw new Error("Syntax error: unexpected newline in String literal at position " + this.pos + " (use triple delimiter?)");
                    }
                    else {
                        this.stringLiteralBuilder.push('\n');
                    }
                    break;
                case '"':
                case "'":
                    if (this.src.substring(this.pos - 1, this.pos + this.stringLiteralDelimiter.length - 1) === this.stringLiteralDelimiter) {
                        this.pos += this.stringLiteralDelimiter.length - 1;
                        var s = this.stringLiteralBuilder.join('');
                        this.stringLiteralBuilder = null;
                        this.stringLiteralDelimiter = null;
                        q.ops.push(new PushOp(new StringValue(s)));
                        return;
                    }
                default:
                    this.stringLiteralBuilder.push(c);
            }
        }
        if (this.stringLiteralDelimiter.length === 3) {
            this.stringLiteralBuilder.push("\n");
        }
        else {
            throw new Error("Syntax error: unexpected end of source: expected delimiter " + this.stringLiteralDelimiter + " in String literal");
        }
    };
    Parser.prototype.nextIdentifier = function (throwIfKeyword) {
        var startPos = this.pos;
        while (++this.pos < this.src.length && this.identifierChar(this.src[this.pos]))
            ;
        if (startPos === this.pos || this.digitChar(this.src.charAt(startPos))) {
            throw new Error("Syntax error: expected identifier at position " + startPos);
        }
        var name = this.src.substring(startPos, this.pos);
        if (name === '') {
            throw new Error('Parser error: empty name at position ' + startPos + " (shouldn't happen)");
        }
        else if (throwIfKeyword && NativeOp.getByName(name) !== null) {
            throw new Error("Syntax error: unexpected keyword " + name + " at position " + startPos);
        }
        return name;
    };
    Parser.prototype.nextOpName = function () {
        var startPos = this.pos;
        while (++this.pos < this.src.length && this.opChar(this.src[this.pos]))
            ;
        return this.src.substring(startPos, this.pos);
    };
    return Parser;
}());
