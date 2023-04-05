# Etl

Etl (pronounced like "nettle" without the n) is an esoteric programming language for transforming expressions.
There are three data types in Etl: pairs, varaibles and operators. Pairs are pairs of the said datatypes.
Pairs are written with parenthesies and arguments seperated by spaces. Operators start with a `!` which is 
followed by any sequence of non-whitespace characters. Variables are all other sequences of characters.
Here is an example:
```
(hello (((!0) foo) bar))
```

Programs are written as a set of replacement rules and a list of expressions to apply the rules to. Here is the syntax and how they work:

```
rule swap (!S (a b)) -> (b a)
(!S ((!S (a b)) c))
```
which outputs `(c (b a))`.


The general syntax for rules is:
```
rule <name> <pattern> -> <replacement>
```
where \<name\> is replaced by a string of the name of the rule (currently there is no use for the name, one 
will be added in the future), and \<pattern\> and \<replacement\> are replaced by expressions.

## Rule application process:
First we do a process called pattern matching which may or may not yield a mapping between the dummy names and actual expressions called "bindings".
For example when we match the expression `(!S (a (b c)))` against the pattern `(!F (a b))` we get no match because the operator `!F`
doesn't have the same name as the operator in its place in the expression. However if we have the exprssion `(!S (x (y z)))` it matches 
producing the following bindings:
```
{
"a": x
"b": (y z)
}
```
Now that we have those bindings we can produce a different expression. That's what the replacement part of the rule is for.
Say that we have a rule
```
rule swap (!S (a b)) -> (b a)
```
and an expression
```
(!S (x (y z)))
```
Our rule is applied recursively outside-in.
So first the program generats the outermost bindings:
```
{
"a": x,
"b": (y z)
}
```
Then the language takes the replacement expression and replaces every instance of "a" and "b" with the corresponding binding.
In the end of evaluation our expression we end up with
```
((y z) x)
```
## Undefined behaviour
What happens when you have two conflicting rules? What happens when there are variables in the replacement expression not found in the pattern?
I don't know! Experiment and have fun. This shouldn't be defined in the specification of the language because I am too lazy to research it.

## Using the repl
Currently the programming language is only available as a repl. To exit it simply type `bye`.

## Example programs
### reversing a linked list
```
rule r1 (!R ((x y) (z))) -> (!R ((x) (z y)))
rule r2 (!R (!0 x)) -> (x)
(!R (((((!0 a) b) c) d) !0))
```
outputs:
```
((((!0 d) c) b) a)
```
### SKI
https://en.wikipedia.org/wiki/SKI_combinator_calculus
```
rule S (((!S x) y) z) -> ((x z) (y z))
rule K ((!K x) y) -> (y)
rule I (!I y) -> (y)
``` 


