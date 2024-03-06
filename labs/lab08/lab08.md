# Context Free Grammars

## Why Grammars?
Context free grammars give us a formal way of specifying and studying the *syntax* of a programming language. It defines the shape of language and does not assign any meaning to recognized sentences.

## Language 1

In the following grammar, `<expr>` is the *starting symbol*.
```
<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<expr>  ::= <digit>
          | <expr> + <expr>
```

terminal symbols: `0, .., 9, +`  
non-terminal symbols: `<digit>, <expr>`

```
1 + 1

left-most derivation
<expr> = <expr> + <expr>
       = <digit> + <expr>
       = 1 + <expr>
       = 1 + <digit>
       = 1 + 1

right-most derivation
<expr> = <expr> + <expr>
       = <expr> + <digit>
       = <expr> + 1
       = <digit> + 1
       = 1 + 1
```

```
1 + 2 + 3

left-most
<expr> = <expr> + <expr>
       = <digit> + <expr>
       = 1 + <expr>
       = 1 + <expr> + <expr>
       = 1 + <digit> + <expr>
       = 1 + 2 + <expr>
       = 1 + 2 + <digit>
       = 1 + 2 + 3

left-most
<expr> = <expr> + <expr>
       = <expr> + <expr> + <expr>
       = <digit> + <expr> + <expr>
       = 1 + <expr> + <expr>
       = 1 + <digit> + <expr>
       = 1 + 2 + <expr>
       = 1 + 2 + <digit>
       = 1 + 2 + 3
```

Question: Are the following sentential forms derivable?
- `12 + 0`
- `1 + (2 + 3)`
- `3 * 5`

## Language 2

In the following grammar, `<expr>` is the *starting symbol*.
```
<digit>  ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<number> ::= <digit> | <digit><number>
<expr>   ::= <number>
           | <expr> + <expr>
```

terminal symbols: `0, .., 9, +`  
non-terminal symbols: `<digit>, <number>, <expr>`

```
1 + 1

<expr> = <expr> + <expr>
       = <number> + <expr>
       = <digit> + <expr>
       = 1 + <expr>
       = 1 + <number>
       = 1 + <digit>
       = 1 + 1
```

```
12 + 2 + 3

left-most
<expr> = <expr> + <expr>
       = <expr> + <expr> + <expr>
       = <number> + <expr> + <expr>
       = <digit><number> + <expr> + <expr>
       = 1<number> + <expr> + <expr>
       = 1<digit> + <expr> + <expr>
       = 12 + <expr> + <expr>
       = 12 + <number> + <expr>
       = 12 + <digit> + <expr>
       = 12 + 2 + <expr>
       = 12 + 2 + <number>
       = 12 + 2 + <digit>
       = 12 + 2 + 3
```

Question: Are the following sentential form derivable?
- `12 + 0`
- `1 + (2 + 3)`
- `3 * 5`

## Language 3

In the following grammar, `<expr>` is the *starting symbol*.
```
<digit>  ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<number> ::= <digit> | <digit><number>
<expr>   ::= <number> 
           | ( <expr> )
           | <expr> + <expr>
           | <expr> * <expr>
```

terminal symbols: `0, .., 9, +, *, (, )`  
non-terminal symbols: `<digit>, <number>, <expr>`

```
1 + (2 * 33)

left-most
<expr> = <expr> + <expr>
       = <number> + <expr>
       = <digit> + <expr>
       = 1 + <expr>
       = 1 + ( <expr> )
       = 1 + ( <expr> * <expr> )
       = 1 + ( <number> * <expr> )
       = 1 + ( <digit> * <expr> )
       = 1 + ( 2 * <expr> )
       = 1 + ( 2 * <number> )
       = 1 + ( 2 * <digit><number> )
       = 1 + ( 2 * 3<number> )
       = 1 + ( 2 * 3<digit> )
       = 1 + ( 2 * 33 )
```

## Language 4

```
1 + 2 + 3

        <expr>
       /      \
    <expr> + <expr>
    /   \         \
 <expr> + <expr>   ..
   |        |       3
   ..       ..
   1         2

        <expr>
       /      \
    <expr> + <expr>
      |       /   \
     ..    <expr> + <expr>
     1       |        |
             ..       ..
             2        3

1 + 2 * 3

(1 + 2) * 3    wrong
1 + (2 * 3)    correct


1 + 2 + 3
(1 + 2) + 3  left associative symbol

A -> B -> C
A -> (B -> C) right associative symbol
```

Goals:
- Unambiguous
- `*` to have higher precedence than `+`
- `*, +` to be left associative

```
<digit>  ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<number> ::= <digit> | <digit><number>

<expr0> ::= <number>
          | ( <expr> )

<expr1> ::= <expr0> | <expr1> * <expr0>

<expr2> ::= <expr1> | <expr2> + <expr1>

<expr> ::= <expr2>
```

```
1 + 2 * 3


               <expr>
                 |
               <expr2>
              /       \
           <expr2> +   <expr1>
            |          /     \
           <expr1> <expr1> * <expr0>
            |         |        |
           <expr0> <expr0>   <number>
            |         |        |
           <number> <number> <digit>
            |         |        |
           <digit>  <digit>    3
            |         |
            1         2


(1 + 2) * 3

       <expr>
         |
       <expr2>
         |
       <expr1>
       /     \
    <expr1> * <expr0>
     |          |
    <expr0>   <number>
     |          |
 ( <expr> )   <digit>
     |          |
   <expr2>      3
   /   \
<expr2> + <expr1>
  |         |
<expr1>   <expr0>
  |         |
<expr0>   <number>
  |         |
<number>  <digit>
  |         |
<digit>     2
  |
  1
```

## Representing Languages as Trees

Internal to a compiler/interpreter, source programs are represented as data structures known as *abstract syntax trees* (AST). 

The grammar of a language can be directly encoded as an AST in OCaml. Non-terminals are defined as new datatype while terminals are defined as constructors of those datatypes.

Naive AST of Language3.
```ocaml
type digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
type number = Digit of digit | Cons of digit * number 
type expr = Num of number
          | Parens of expr
          | Add of expr * expr
          | Mul of expr * expr
```

ASTs defined this way tend to be overly verbose and inefficient to compute with. When designing an AST, the encoding does *not* need to correspond to the language grammar 1-to-1. It only needs to capture key features of the language.

Better AST of Language3
```ocaml
type expr = Num of int
          | Add of expr * expr (* (add e1 e2) *)
          | Mul of expr * expr (* (mul e1 e2) *)
```

Notice in this encoding of Language3, we used `int` to efficiently represent `<number>`. Any sentential form recognized by `<number>` can be represented as an `int` value. However, not every value `int` corresponds to a sentential form recognized by `<number>` (i.e. `-1`). This means that when implementing a parser (of type `string -> expr`) that transforms `string` to `expr`, we have to make sure that invalid integers are rejected by the parser.

Additionally, each `expr` can be seen as implicitly parenthesized. This makes having an explicit parentheses constructor redundant.
