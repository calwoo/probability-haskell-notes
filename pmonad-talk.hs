-- Notes on a talk by Tom Schrijvers on the probability monad.

module PMonad where
import Prelude hiding (sum, Maybe, Just, Nothing, lookup)

-- Main building block of FP: total functions A -> B
sum :: [Int] -> Int
sum []     = 0
sum (x:xs) = x + sum xs


-- partial functions: A -> Maybe B
data Maybe x = Just x | Nothing

lookup :: Eq k => [(k,v)] -> k -> Maybe v
lookup [] _ = Nothing
lookup ((k',v):l) k
    | k == k' = Just v
    | otherwise = lookup l k

-- non-deterministic functions: A -> [B]
-- (okay, seems the theme is that monads are great models of computation, the
-- common motif of functional programming...)
member :: Eq k => [(k,v)] -> k -> [v]
member [] _ = []
member ((k',v):l) k
    | k == k' = v : member l k
    | otherwise = member l k

-- in general, if we want an affect, wrap B in a monad M
-- A -> M B

-- what is probabilistic functional programming?
-- A -> Dist B

newtype Probability = P Float
newtype Dist a = D {unD :: [(a, Probability)]}