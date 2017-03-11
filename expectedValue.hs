{-# LANGUAGE FlexibleInstances #-}
module RandomVariable where
-- Probability distribution is created in form [(Int, Double)]
-- e.g. Random Varible X = [(1, 0.5), (2, 0.5)]
-- If random variable X^2 can be written as X * X. 


diceX = zip [1..] $ replicate 6 (1/6) :: [(Int, Double)]
diceY = diceX + diceX
type Sample = (Int, Double)

class Exponentable a where
    expo :: a -> Int -> a

instance Num [(Int, Double)] where
    x * y = (*) <$> x <*> y
    x + y = (+) <$> x <*> y
instance Exponentable [(Int, Double)] where
    x `expo` b = map (`expo` b) x
    
instance Num (Int, Double) where
    (a,b) * (c,d) = (a*c, b*d)
    (a,b) + (c,d) = (a+c, b*d)
instance Exponentable (Int, Double) where
    (a,b) `expo` c = (a ^ c, b)


expectedVal :: [(Int, Double)] -> Double
expectedVal xs = foldr (\(a,b) s -> s + fromIntegral (a::Int) * (b::Double)) 0.0 xs


