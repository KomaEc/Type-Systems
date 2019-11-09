# miniTT

This repo contains my implementation of miniTT with parser.

## Semantics
I find that there's a rules forgotten in the paper : the elimination rule for unit type. If anyone ever wrote a type checker that follows the rule presented in the paper faithfully, he/she would find that the 4th example, namely `boolElim` cannot pass type-checking. It will complains that an inferred type is not equal to a checked type.

Let's expand it a little more:
```
elimBool : Π c : (bool → U) . c false → c true → Π b : bool . c b
         = λ c . λ h0 . λ h1 . fun (True → λ _ . h1 | False → λ _ . h0)
; 
```
The type checker will check the term `λ _ . h1` against the type `Π b : bool . x₀ b`, and then (by following the rules) check the term `h1` against the type `x₀ (True x₃)`, where `x₃` the the generic value produced when checking a lambda-term. It's clear to us that `x₃` has type unit, but type checker can't witness this.

## Syntax
There're minor changes to the original syntax (mainly for parsing purpose) :

* expression variable should start with lower case, while constructor should start with upper case.

* `->` and `→` are allowed syntax for arrow type.

* `*` and `×` are allowed syntax for product type.

* a complete program consists of a series of declaration (without an expression at the end).