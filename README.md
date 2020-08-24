# Fundamentalist Functional Programming

An interpreter for the untyped [λ-calculus](https://en.wikipedia.org/wiki/Lambda_calculus), written in Haskell.

## λ-Term Syntax

Valid λ-terms are one of the following three things:

* Variables are non-empty strings of lower-case Latin letters: `a`, `b`, `c`, `x`, `y`, `z`, `aa`, `ab`, etc.
* λ-abstractions begin with either `λ` or `\`, then consist of a valid variable name and a subterm separated by a `.`: `λx.x`, `\x.x`.
* Applications consist of two subterms separated by a mandatory space: `(λx.x x) (λx.x)`.

Non-empty strings of decimal digits will be parsed according to the [Church encoding](https://en.wikipedia.org/wiki/Church_encoding) for that natural number; for example, `3` is syntactic sugar for `λf.λx.f (f (f x))`.
Additionally, the interpreter maintains an environment of named λ-terms which may be referred to by an identifier:

* Identifiers are non-empty strings of Latin letters and decimal digits which begin with an upper-case letter.
* The environment of named λ-terms may be extended using a `~let` command: `~let Identity := λx.x`, `~let Y := λf.(λx.f (x x)) (λx.f (x x))`.
