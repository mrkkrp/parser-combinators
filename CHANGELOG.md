## Parser combinators 0.4.0

* Improved the documentation.

* Re-exported `Control.Applicative.empty` from
  `Control.Applicative.Combinators`.

* Added the `Control.Monad.Combinators` and
  `Control.Monad.Combinators.NonEmpty` modules which contain more efficient
  versions of the combinators from `Control.Applicative.Combinators` and
  `Control.Applicative.Combinators.NonEmpty` respectively.

## Parser combinators 0.3.0

* Added the `skipCount` combinator.

* Improved algorithmic efficiency of the `count'` combinator.

## Parser combinators 0.2.1

* Removed the byte order marking at the beginning of the
  `Control.Applicative.Permutations` module.

## Parser combinators 0.2.0

* Added `Control.Applicative.Combinators.NonEmpty` module that exports
  non-empty list versions of combinators that cannot return empty lists.

* Added `Control.Applicative.Permutations` module that provides generalized
  permutation parser combinators.

## Parser combinators 0.1.0

* Initial release.
