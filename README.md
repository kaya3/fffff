# fffff - the silliest stack-based concatenative language

**fffff** is a stack-based concatenative programming language.

This project provides an implementation of fffff in Javascript (actually Typescript).
The implementation includes an interpreter with a REPL, and a compiler to Javascript.

## How do I program in fffff?

See the [documentation](https://kaya3.github.io/fffff/docs/).

## Why shouldn't I program in fffff?

fffff is designed to be easy to implement, not easy to write in.

#### Examples

Hello world:

```
"Hello, world!" println
```

The classic Fizzbuzz problem:

```
(
    n 3 % 0 = >three
    n 5 % 0 = >five
    
    ("fizz" print) three if
    ("buzz" print) five if
    (n print) three five or not if
    
    "" println
) >!print_num

1 >n
(print_num n 1 + >n) 100 repeat
```
