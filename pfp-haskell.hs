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

-- prod produces "product distributions". A good example of this is with experiments
-- involving batches of dice
certainly :: a -> Dist a -- point distribution
certainly a = D [(a,P 1)]

dice :: Int -> Dist [Int]
dice 0 = certainly []
dice n = joinWith (:) die (dice (n-1))
       
-- However, what happens when independence breaks down? suppose that the second randvar
-- depends on the first. Then while the first distribution can be a represented by 
-- Dist a, the second randvar has distribution that depends on a function a -> Dist b.
-- (=======================)
-- the above motivation doesn't make much sense to me. A better one is to say that we want
-- to understand probabilistic functions a -> Dist b, ie functions that return a probability
-- distribution. A notion of composition of these functions intuitively makes sense: if the
-- distribution over b depends on a value a (conditional), and the distribution over c depends
-- on a value b, then what distribution underlies c given a? the answer is given by Kleisli
-- composition! probability is a monad!

instance Functor Dist where
    fmap f = D . fmap (\(x,p) -> (f x,p)) . unD

instance Applicative Dist where
    pure = return
    -- (<*>) :: Dist (a -> b) -> Dist a -> Dist b
    f <*> x = D [(g z, P $ p*q) | (z, P p) <- unD x, (g, P q) <- unD f]

instance Monad Dist where
    return x = certainly x
    (D d) >>= f = D [(y, P $ p*q) | (x, P p) <- d, (y, P q) <- unD (f x)]

-- RECALL:
-- monad => applicative
-- proof: f <*> x = f >>= (\g -> x >>= (\z -> return $ g z)).
--      in do notation this is do
--                              g <- f
--                              z <- x
--                              return $ g z
--  
-- in this case, unwrapped in Dist this becomes
--      f <*> x = D [(g z, P $ p*q) | (z, P p) <- x, (g, P q) <- f]