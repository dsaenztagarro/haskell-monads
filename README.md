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

-- random number generation
*Main3> runState threeRandoms $ CountRandom { crGen = mkStdGen(10), crCount = 0 }
((-2774747785423059091,-5364865979222864935,5005192715100199576),CountRandom {crGen = 1143547415 1422611300, crCount = 3})
```
