# fffff - the silliest stack-based concatenative language

**fffff** is a stack based concatenative programming language.

This project provides an implementation of fffff in Javascript (actually Typescript).
The implementation includes an interpreter with a [REPL](https://kaya3.github.io/fffff/repl.html), and a [compiler to Javascript](https://kaya3.github.io/fffff/compiler.html).

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
# declare an instruction named print_num
(
    # three := n % 3 = 0
    n 3 % 0 = >three
    # five := n % 5 = 0
    n 5 % 0 = >five
    
    # if three: push "fizz" to the stack
    ("fizz") three if
    # if five: push "buzz" to the stack
    ("buzz") five if
    # if not (three or five): push n to the stack
    (n) three five or not if
    
    # print what's on the stack
    println
) >!print_num

# n := 1
1 >n

# do (print_num; n := n + 1) 100 times
(print_num n 1 + >n) 100 repeat
```

Object-oriented fffff:

```
# creates an instance and calls its init method
({>class}. @.class.init) >!new

# shorthand for dispatching methods by the instance's class
(@.class) >!_

# Point class
{
    # initialiser
    (.{ >x,y }.) >!init
    
    # getters
    (.x) >!x
    (.y) >!y
    
    # scale method
    ({ >self >s
        <s <self.x * >self.x
        <s <self.y * >self.y
    }) >!scale

    # dump method
    (.{
        [<x <y]. println
    }) >!dump
}. >Point

# Point3 subclass
{
    Point >super
    
    # initialiser
    (.{ >z }. super.init) >!init
    
    # must inherit explicitly, unfortunately
    (super.x) >!x
    (super.y) >!y
    
    # z method
    (.z) >!z
    
    # override scale method
    ({ >self >s
        <s <self super.scale
        <s <self.z * >self.z
    }) >!scale
    
    # override dump method
    (.{
        [<x <y <z]. println
    }) >!dump
}. >Point3


# create instance
2 3 Point new >p

# access field directly
p.x println

# access field via getter
p _.x println

# call dump method
p _.dump

# call scale method
5 p _.scale

# call dump method via Point class
p Point.dump


# create subclass instance
2 3 4 Point3 new >q

# access field directly
q.x println

# access field via getter
q _.x println

# call dump method
q _.dump

# call scale method
5 q _.scale

# call dump method via Point3 class
q Point3.dump

# call dump method via Point class; this won't print q.z
q Point.dump
```
