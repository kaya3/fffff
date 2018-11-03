class VM {
	// push from the constants array to the current stack
	pushConstant(i: number): void {
		// TODO
	}
	
	// perform built-in operation
	doBuiltIn(name: string): void {
		// TODO
	}
	
	// load from current scope and push to the current stack
	loadFast(name: string): void {
		// TODO
	}
	
	// load from first scope found and push to the current stack
	loadSlow(name: string): void {
		// TODO
	}
	
	// pop from current stack and store in current scope
	store(name: string): void {
		// TODO
	}
	
	// delete top item from current stack
	pop(): void {
		// TODO
	}
	
	// open a new stack
	openStack(): void {
		// TODO
	}
	
	// close the current stack and push it to the stack below
	closeStack(): void {
		// TODO
	}
	
	// open a new scope
	openScope(): void {
		// TODO
	}
	
	// close the current scope and push it to the current stack
	closeScope(): void {
		// TODO
	}
}
