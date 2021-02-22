# exp-lang
Simple expression based functional language compiler.

## Values
Functions are first class values in this language. Values are all typed by the following type system:
```
τ ::= int | bool | τ1 -> τ2 | α
```
where α is a polymorphic type variable.
A value may have any of these types.

## Let expressions
Let expressions assign an expression value to a variable. This is also how named functions must be defined - by letting an identifier equal a lambda expression.
The syntax for a let expression is as follows:
```
let x = e1 in e2
```
where "x" is an identifier name, "e1" is the bound expression, and "e2" is the used expression

## Lambda expressions
Lambda expressions create a lambda abstraction with a named identifier.
The syntax for a lambda expression is as follows:
```
$x -> e
```
where "x" is an identifier name and "e" is the function body (i.e. equivalent to $\lambda x. e$)

## Binary operators
There is a fixed set of binary operators available, namely +, -, *, /, <, >, <=, >= and ==.
These behave as expected, and are infix operators.

## Variables
Variables are referenced by their identifier name, which must be in scope at the time of use

## Literals
There are integer literals, which are just written as numbers (e.g. `5`), and boolean literals which are either `True` or `False`.
