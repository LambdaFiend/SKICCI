# SKICCI
**SKI**-**C**ombinators **C**alculus **I**nterpreter.

## Introducing

This is an interpreter made for anyone who might be interested in experimenting with the *SKI-combinators calculus*.

Supports environment variables for SKI-terms, showing the contents of an environtment variables, displaying the list of environment variables, checking for syntactic same-ness, evaluating n-steps at a time (in-place, for environment variables), left-most reduction and right-most reduction.

I'll be adding a couple more features, eventually.

## Running the program

Haskell and Cabal should both be installed.

```cabal build```

```cabal run```

Type ```:?```, ```:h``` or ```:help``` for help within the program.

## Syntax and Semantics

| Syntax | Semantics |
| :----: | :-------- |
| S | The S-Combinator. |
| K | The K-Combinator. |
| I | The I-Combinator. |
| t1 t2 | An application.<br>It packs pairs of terms.<br>It's useful for packing<br>terms into a single "thing"<br>or manipulating the order<br>of the terms. |
| x | A variable.<br>Must be lowercase, and<br>can end on any amount<br>of primes. |

* I x -> x
* K x y -> x
* S x y z -> (x z) (y z)

Where x, y and z can be any SKI-Combinators term.

These are the weak-reduction (->w) rules for combinator terms.

Applications are conventionally left associative.

There's no need to put spaces in-between combinators, as they do not share names.

## Reporting issues

Do not forget to report any bugs. Contact me, otherwise you can create a new issue on this repository. Thanks!
