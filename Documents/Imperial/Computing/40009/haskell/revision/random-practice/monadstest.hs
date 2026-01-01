data Box a = Box a
    deriving(Show, Eq)

instance Functor Box where
    fmap f (Box x) = Box (f x)

instance Applicative Box where
    pure x = Box x
    (<*>) (Box f) (Box x) = Box (f x)

instance Monad Box where
    (>>=) (Box x) func = func x

safeDiv :: Double -> Double -> Either String Double
safeDiv x y =
    if y == 0 then Left "division by zero"
    else (/) <$> Right x <*> Right y


data Expr = Add Expr Expr | Mul Expr Expr | Val Int | Var String


SumInt :: [Either String Int] -> Either String Int
SumInt (Left x:xs) = Left "Not an integer"
SumInt (x:y:xs) = x >>= (x+y)