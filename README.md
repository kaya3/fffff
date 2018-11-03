# fffff - The Programming Language

fffff is a stack based programming language.

This project aims to convert fffff source code into valid javascript that can be executed in the browser.

## How do I program in fffff?

When programming in fffff, always remember that everything works on a stack.

#### Pushing and deleting from the stack.


We start off with an empty stack:

```
[].
```

When fffff outputs the stack, it is in fact valid ffff. It will give you the code such that given a empty stack will return that stack into the same state.

Push something to the stack is easy. To put a number on the stack is simply: `1`

The dot after closing square bracket (`].`) means that when you exit a scope or stack will leave the result on the stack. Example will be outlined below.

E.g. When our program `1` is executed we will result in the following stack:

```
[1].
```

The program `1 2` will give us a stack of:

```
[1 2].
```

To delete an item on the stack, use the built in command `del`.

```
>>> 1 2 del

[1].
```

This is very much a pop in data structures but we instead we forget about it.

#### Comments

Anything after a hash (`#`) will mean the rest of the line is a comment.

```
>>> # Hello World!
```

#### Mathematical Operators

fffff comes with built in  mathematical operators. They can be used as follows:

```
>>> 1 2 +

[3].
```

The following operators are in fffff:

| Symbol        | Function           |
| ------------- |:------------------:|
| `+`           | Addition           |
| `-`           | Subtraction        |
| `*`           | Multiplication     |
| `/`           | Divide             |
| `**`          | Power              |
| `%`           | Modulus            |
| `&`           | Bitwise And        |
| `\|`          | Bitwise Or         |
| `~`           | Bitwise Not        |
| `^`           | Bitwise And        |
| `=`           | Equality           |
| `<`           | Less than          |
| `>`           | Greater than       |
| `<=`          | Less than equal    |
| `>=`          | Greater than equal |

Some further examples:

```
>>> 24 12 + -1 *

[-36].
```

#### Boolean Keywords

**true** - Represents the boolean true. Will push true on the stack.

```
>>> true

[true].
```

**false** - Represents the boolean false. Will push false on the stack.

```
>>> false

[false].
```

**and** - Pops top two elements from the stack and applies logical and.

```
>>> true false and

[false].
```

**or** - Pops top two elements from the stack and applies logical or.

```
>>> true false or

[true].
```

**not** - Pops top element from the stack and applies logical not.

```
>>> true not

[false]. 
```

#### Other keywords


**print** - pops the top item on the stack and prints it.

```
>>> 1 print

1
[].
```

**println** - pops the top item on the stack and prints it. Will print a new line after.

```
>>> 1 println

1

[ ].
```

**del** - Removes the top element from the stack

**push** - Given a stack and value will push that value onto *that* stack.

```
>>> []. 1 push

[[1].].
```


**pop** - Given a stack with an element will pop the top element from it and push it on the current stack.

```
>>> [ 1 ]. pop

[1].
```

**len** - Takes an argument of a stack and will return the length of the stack.
```
>>> [1 2 3 4]. len

[4].
```

**get** - Get the n'th element from the list (indexes start at 0). The 0th element will be the bottom of the stack.
```
>>> [1 2 3 4]. get 1

[2].
```

**if** - Executes the quote if the the expression is true.

```
>>> ( 1 2 3 ) true if

[1 2 3].

>>> (4 5 6) false if

[1 2 3].
```

**repeat** - Repeat the quote n times.

```
>>> (3 4) 4 repeat

[3 4 3 4 3 4 3 4].
```

**this** - Pushes the current scope to the stack.

```
>>> this

[{}.].
```

**stack** - Pushes the current stack to the stack. In this example we are pushing a reference to the top level stack inside the top level stack (This is why the ... appears).

```
>>> stack

[[...].].
```
