# lambda

A lambda calculus REPL implemented in Haskell, using Parsec for parsing.

```
$ rlwrap stack exec lambda-exe
> (\x.xx)y
y y
> (\n. \f. \x. f (n f x)) (\f. \x. f (f x))
\f. \x. f (f (f x))
```
