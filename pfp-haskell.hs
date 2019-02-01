-- An implementation of Erwig-Kollmansberger "Probabilistic Functional Programming in Haskell"

-- datatype to represent distributions (this is to preserve referential transparency)
newtype Probability = P Float 
    deriving (Eq, Show)
newtype Dist a = D {unD :: [(a, Probability)]} 
    deriving (Eq, Show)

-- probability distributions can be constructed from lists of values using spread functions
type Spread a = [a] -> Dist a 

-- examples of spreads
uniform :: [a] -> Dist a
uniform ls = D (zip ls probs)
    where len = length ls
          prob = 1 / fromIntegral len
          probs = replicate len (P prob)

-- using this we can define things like the distribution over dice rolls
die = uniform [1..6]

-- as in basic probability theory, an event is a subset of the sample space
type Event a = a -> Bool
-- we want to assign probabilities to these events
sumP :: [Probability] -> Probability
sumP [] = P 0
sumP ((P p):ps) = case sumP ps of
     P total -> P (p + total)

(??) :: Event a -> Dist a -> Probability
(??) p = sumP . map snd . filter (p . fst) . unD

-- as an example, take the event that a die roll is greater than 2
greaterThan2 :: Event Int
greaterThan2 x
    | x > 2     = True
    | otherwise = False

example1 = greaterThan2 ?? die
example2 = (not . greaterThan2) ?? die

-- Given independent distributions, one way to combine them is by taking pairs
-- of values (could be twisted by a function) while multiplying their probabilities.
-- Here list comprehensions really come in handy!
joinWith :: (a -> b -> c) -> Dist a -> Dist b -> Dist c
joinWith f (D d) (D d') = D [(f x y, P $ p*q) | (x,P p) <- d, (y,P q) <- d']

-- Recall that a list comprehension is really a compressed do-notation:
joinWith' :: (a -> b -> c) -> Dist a -> Dist b -> Dist c
joinWith' f (D d) (D d') = 
    D (do (x, P p) <- d
          (y, P q) <- d'
          return (f x y, P $ p*q))

prod :: Dist a -> Dist b -> Dist (a,b)
prod = joinWith (,)


       

