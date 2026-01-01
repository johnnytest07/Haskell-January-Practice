safeDiv :: Double -> Double -> Either String Double
safeDiv a 0 = Left "division by zero"
safeDiv a b = Right (a / b)

safeDiv' :: Double -> Double -> Either String Double
safeDiv' x y = do
    x' <- Right x
    y' <- Right y
    if y' == 0
        then Left "division by zero"
        else Right (x'/y')

safeDiv'' :: Double -> Double -> Either String Double
safeDiv'' x y =
    Right x >>= \x' ->
    Right y >>= \y' ->
        if y' == 0
            then Left "division by zero"
            else Right (x'/y')

safeDiv''' :: Double -> Double -> Either String Double
safeDiv''' x y =
    if y == 0 then Left "division by zero"
    else ((/) <$> Right x) <*> Right y

data Box a = Box a
    deriving (Show, Eq)

instance Functor Box where
    fmap f (Box x) = Box (f x)

instance Applicative Box where
    pure x = Box x
    (<*>) (Box func) (Box a) = (Box (func a))

instance Monad Box where
    return = Box
    (>>=) (Box x) func = func x

newtype SumInt = SumInt Int
    deriving (Show, Eq)

instance Semigroup SumInt where
    mempty = SumInt 0
    mappend (SumInt x) (SumInt y) = SumInt (x + y)