# hs-monads

GHC 7.10 required

Implements [Functor-Applicative-Monad Proposal](https://wiki.haskell.org/Functor-Applicative-Monad_Proposal)

## Examples

### Monad State

```haskell
runState playGame $ loadGame
# => (StandardAction,Game {players = ["David","Javier","Mario"]})

*Main2> runState checkStack [1,2,3,5]
((),[1,2,3,5])
*Main2> runState checkStack [1,4]
((),[2])
```
