interface Array<T> {
	peek(): T;
	clear(): void;
}
Array.prototype.peek = function() {
	if(this.length === 0) { throw new Error('Peek at empty stack'); }
	return this[this.length-1];
};
Array.prototype.clear = function() {
	// this is allowed because JavaScript is silly
	this.length = 0;
};

class Parser {
	public static parse(src: string, keepComments: boolean = false): Quote {
		let p: Parser = new Parser(keepComments);
		p.read(src);
		let q: Quote|null = p.parse();
		if(q === null) {
			throw new Error("Unexpected end of source: expected )");
		}
		return q;
	}
	
	private src: string = '';
	private readonly quotes: Array<Quote> = [];
	private pos: number = 0;
	
	public constructor(private readonly keepComments: boolean = false) {}
	
	public read(src: string): void {
		this.src = src;
		this.pos = 0;
	}
	
	public parse(): Quote|null {
		if(this.quotes.length === 0) {
			this.quotes.push(new Quote());
			this.stringLiteralBuilder = null;
			this.stringLiteralDelimiter = null;
		} else if(this.stringLiteralBuilder != null) {
			this.stepStringLiteral(this.quotes.peek());
		}
		
		while(this.pos < this.src.length) {
			this.step();
		}
		
		if(this.quotes.length === 1 && this.stringLiteralBuilder === null) {
			return this.quotes.pop() || null;
		} else {
			return null;
		}
	}
	
	private step(): void {
		let c: string = this.src[this.pos];
		let q: Quote = this.quotes.peek();
		if(/\s/.test(c)) {
			this.stepWhitespace();
		} else if(c === '#') {
			let startPos: number = ++this.pos;
			this.stepComment();
			if(this.keepComments) {
				q.ops.push(new CommentOp(this.src.substring(startPos, this.pos)));
			}
		} else if(c === '(') {
			++this.pos;
			this.quotes.push(new Quote());
		} else if(c === ')') {
			++this.pos;
			if(this.quotes.length > 1) {
				this.quotes.pop();
				this.quotes.peek().ops.push(new PushOp(q));
			} else {
				throw new Error("Unexpected ) at position " + this.pos);
			}
		} else if(c === '!') {
			++this.pos;
			q.ops.push(NativeOp.NOW);
		} else if(c === '.') {
			if(++this.pos === this.src.length) {
				throw new Error("Unexpected end of source: expected .[ or .{ or .identifier");
			}
			
			c = this.src[this.pos];
			if(c === '['){
				++this.pos;
				q.ops.push(NativeOp.STACK_ENTER);
			} else if(c === '{') {
				++this.pos;
				q.ops.push(NativeOp.SCOPE_ENTER);
			} else if(this.identifierChar(c)) {
				let name: string = this.nextIdentifier(true);
				q.ops.push(NativeOp.SCOPE_ENTER);
				q.ops.push(new LocalReadOp(name));
				q.ops.push(NativeOp.SCOPE_ASCEND);
			} else {
				throw new Error("Unexpected character " + c + " at position " + this.pos);
			}
		} else if(c === '[') {
			++this.pos;
			q.ops.push(NativeOp.STACK_DESCEND);
		} else if(c === '{') {
			++this.pos;
			q.ops.push(NativeOp.SCOPE_DESCEND);
		} else if(c === ']') {
			if(++this.pos === this.src.length || this.src[this.pos] !== '.') {
				q.ops.push(NativeOp.STACK_ASCEND);
			} else {
				++this.pos;
				q.ops.push(NativeOp.STACK_EXIT);
			}
		} else if(c === '}') {
			if(++this.pos === this.src.length || this.src[this.pos] !== '.') {
				q.ops.push(NativeOp.SCOPE_ASCEND);
			} else {
				++this.pos;
				q.ops.push(NativeOp.SCOPE_EXIT);
			}
		} else if(c === '"' || c === '\'') {
			this.stepStringLiteral(q);
		} else if(this.digitChar(c) || (c === '-' && this.pos+1 < this.src.length && this.digitChar(this.src[this.pos+1]))) {
			q.ops.push(new PushOp(this.nextNumber()));
		} else if(c === '>' && (this.identifierChar(c = this.src[this.pos+1]) || c === '!')) {
			class Assignment {
				readonly names: Array<string> = [];
				doNow: boolean = false;
			}
			
			// assign var
			let assignments: Array<Assignment> = [];
			do {
				let a: Assignment = new Assignment();
				if(this.src[++this.pos] === '!') {
					a.doNow = true;
					++this.pos;
				}
				
				do {
					a.names.push(this.nextIdentifier(true));
				} while(this.pos < this.src.length && this.src[this.pos] === '.' && ++this.pos < this.src.length /* skip the . */);
				
				assignments.push(a);
			} while(this.pos < this.src.length && this.src[this.pos] === ',');
			
			while(assignments.length) {
				let a: Assignment = assignments.pop() as Assignment;
				let scopes: number = 0;
				while(true) {
					let name: string = a.names.shift() as string;
					
					if(!a.names.length) {
						q.ops.push(new AssignOp(name, a.doNow));
						break;
					} else {
						q.ops.push(scopes === 0 ? new ReadOp(name) : new LocalReadOp(name));
						q.ops.push(NativeOp.SCOPE_ENTER);
						++scopes;
					}
				}
				
				while(--scopes >= 0) {
					q.ops.push(NativeOp.SCOPE_ASCEND);
				}
			}
		} else if(this.identifierChar(c)) {
			let name: string = this.nextIdentifier(false);
			let op: Op|null = NativeOp.getByName(name);
			if(op !== null) {
				q.ops.push(op);
			} else {
				// read
				q.ops.push(new ReadOp(name));
			}
		} else if(this.opChar(c)) {
			// apply op
			q.ops.push(new ReadOp(this.nextOpName()));
		} else {
			throw new Error("Unexpected character " + c + " at position " + this.pos);
		}
	}
	
	private stepWhitespace(): void {
		// TODO: search for next non-whitespace character after pos
		while(++this.pos < this.src.length && /\s/.test(this.src[this.pos]));
	}
	
	private stepComment(): void {
		while(this.pos < this.src.length && this.src[this.pos++] !== '\n');
	}
	
	private digitChar(c: string): boolean {
		return /[0-9]/.test(c);
	}
	
	private identifierChar(c: string): boolean {
		return /[a-zA-Z0-9_]/.test(c);
	}
	
	private opChar(c: string): boolean {
		// TODO: replace with regex?
		return c === '>' || c === '<' || c === '='
			|| c === '?' || c === '\\'
			|| c === '+' || c === '-' || c === '*' || c === '/' || c === '%'
			|| c === '&' || c === '|' || c === '!' || c === '~' || c === '^'
			;
	}
	
	private nextNumber(): Value {
		let startPos: number = this.pos;
		if(this.src[this.pos] === '-') {
			++this.pos;
		}
		
		let hasExponent: boolean = false;
		let isDouble: boolean = false;
		while(this.pos < this.src.length) {
			let c: string = this.src[this.pos];
			if(c === '.') {
				if(isDouble) {
					throw new Error("Unexpected character . at position " + this.pos);
				}
				isDouble = true;
			} else if(c === 'e' || c === 'E') {
				if(hasExponent) {
					throw new Error("Unexpected character " + c + " at position " + this.pos);
				}
				hasExponent = true;
			} else if(!this.digitChar(c)) {
				break;
			}
			++this.pos;
		}
		
		let n: string = this.src.substring(startPos, this.pos);
		try {
			if(isDouble) {
				// TODO
				throw new Error('Error: doubles are not implemented');
				return new DoubleValue(parseFloat(n));
			} else {
				// TODO: use bigint?
				return new IntValue(parseInt(n));
			}
		} catch(e) {
			throw new Error("Syntax error: invalid numeric literal " + n + " at position " + startPos);
		}
	}
	
	private stringLiteralBuilder: Array<string>|null = null;
	private stringLiteralDelimiter: string|null = null;
	private stepStringLiteral(q: Quote): void {
		if(this.stringLiteralBuilder === null || this.stringLiteralDelimiter === null) {
			let d: string = this.src.charAt(this.pos++);
			this.stringLiteralDelimiter = d;
			if(this.src[this.pos] === d && this.src[this.pos+1] === d) {
				this.pos += 2;
				this.stringLiteralDelimiter = d + d + d;
			}
			
			this.stringLiteralBuilder = [];
		}
		
		while(this.pos < this.src.length) {
			let c: string = this.src[this.pos++];
			switch(c) {
				case '\\':
					if(this.pos === this.src.length) {
						throw new Error("Unexpected end of source: expected escape sequence in String literal");
					}
					
					switch(c = this.src[++this.pos]) {
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
							if(this.pos + 4 > this.src.length) {
								throw new Error("Unexpected end of source: expected unicode escape sequence in String literal");
							}
							let hex: number = parseInt(this.src.substring(this.pos, this.pos+4), 16);
							this.stringLiteralBuilder.push(String.fromCharCode(hex));
							this.pos += 4;
							break;
						
						default:
							throw new Error("Unknown escape sequence \\" + c + " in String literal at position " + this.pos);
					}
					break;
				
				case '\n':
					if(this.stringLiteralDelimiter.length === 1) {
						throw new Error("Unexpected newline in String literal at position " + this.pos + " (use triple delimiter?)");
					} else {
						this.stringLiteralBuilder.push('\n');
					}
					break;
				
				case '"':
				case "'":
					if(this.src.substring(this.pos-1, this.pos+this.stringLiteralDelimiter.length-1) === this.stringLiteralDelimiter) {
						this.pos += this.stringLiteralDelimiter.length - 1;
						let s: string = this.stringLiteralBuilder.join('');
						this.stringLiteralBuilder = null;
						this.stringLiteralDelimiter = null;
						q.ops.push(new PushOp(new StringValue(s)));
						return;
					}
				default:
					this.stringLiteralBuilder.push(c);
			}
		}
		
		if(this.stringLiteralDelimiter.length === 3) {
			this.stringLiteralBuilder.push("\n");
		} else {
			throw new Error("Unexpected end of source: expected delimiter " + this.stringLiteralDelimiter + " in String literal");
		}
	}
	
	private nextIdentifier(throwIfKeyword: boolean): string {
		let startPos: number = this.pos;
		while(++this.pos < this.src.length && this.identifierChar(this.src[this.pos]));
		if(startPos === this.pos || this.digitChar(this.src.charAt(startPos))) {
			throw new Error("Expected identifier at position " + startPos);
		}
		let name: string = this.src.substring(startPos, this.pos);
		if(throwIfKeyword && NativeOp.getByName(name) !== null) {
			throw new Error("Unexpected keyword " + name + " at position " + startPos);
		}
		return name;
	}
	
	private nextOpName(): string {
		let startPos: number = this.pos;
		while(++this.pos < this.src.length && this.opChar(this.src[this.pos]));
		return this.src.substring(startPos, this.pos);
	}
}
