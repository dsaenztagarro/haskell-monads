# hs-monads

GHC 7.10 required

Implements [Functor-Applicative-Monad Proposal](https://wiki.haskell.org/Functor-Applicative-Monad_Proposal)

## Examples

```haskell
runState playGame $ loadGame
# => (StandardAction,Game {players = ["David","Javier","Mario"]})
```
