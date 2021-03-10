# exp-lang
Simple expression based functional language compiler.

## Values
Functions are first class values in this language. Values are all typed by the following type system:
```
τ ::= unit | int | bool | (τ1, τ2) | τ1 -> τ2 | α
```
where α is a polymorphic type variable.
A value may have any of these types.

## Let expressions
Let expressions assign an expression value to a variable. This is also how named functions must be defined - by letting an identifier equal a lambda expression.
The syntax for a let expression is as follows:
```
let x = e1 in e2
```
where "x" is a pattern, "e1" is the bound expression, and "e2" is the rest of the expression.

Note that "x" may NOT appear in e1.

## Let Rec expressions
Let expressions which allow for a recursive binding. Here, the name may be used in its binding.
```
let rec x = e1 in e2
```
where "x" is an identifier - general patterns are not allowed here. "e1" is again the bound expression, but "x" may appear in it. "e2" as before.

## Lambda expressions
Lambda expressions create a lambda abstraction with a named identifier.
The syntax for a lambda expression is as follows:
```
$x -> e
```
where "x" is a pattern and "e" is the function body (i.e. equivalent to λx. e with the extension of pattern matching)

## Patterns
There are three forms of patterns.
### Literal Patterns
```
1, 2, True, False, () etc
```
Literal patterns can be in the form of any literal in the language.
### Variable patterns
```
x, y
```
Variable patterns are in the form of an identifier
### Pair patterns
```
(p1, p2)
```
where p1 and p2 are patterns.

## Binary operators
There is a fixed set of binary operators available, namely +, -, *, /, <, >, <=, >= and ==.
These behave as expected, and are infix operators.

## Variables
Variables are referenced by their identifier name, which must be in scope at the time of use

## Literals
Integer literals, which are just written as numbers (e.g. `5`)

Boolean literals which are either `True` or `False`.

Unit literal, which is just `()`; represents a singleton set
