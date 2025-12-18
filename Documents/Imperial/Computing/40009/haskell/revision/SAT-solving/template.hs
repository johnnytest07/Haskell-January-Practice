module SOL where

import Data.List
import Data.Maybe

import Types
import TestData

printF :: Formula -> IO()
printF
  = putStrLn . showF
  where
    showF (Var v)
      = v
    showF (Not f)
      = '!' : showF f
    showF (And f f')
      = "(" ++ showF f ++ " & " ++ showF f' ++ ")"
    showF (Or f f')
      = "(" ++ showF f ++ " | " ++ showF f' ++ ")"

--------------------------------------------------------------------------
-- Part I

-- 1 mark
lookUp :: Eq a => a -> [(a, b)] -> b
-- Pre: The item being looked up has a unique binding in the list
lookUp inp ((x1,x2):xs)
  |inp == x1 = x2
  |otherwise = lookUp inp xs

-- 3 marks
vars :: Formula -> [Id]
vars x
  = case x of
    (Var ret) -> [ret]
    (Not nested) -> (sort . nub) (vars nested)
    (And nested1 nested2) -> (sort . nub) (vars nested1 ++ vars nested2)
    (Or nested1 nested2) -> (sort . nub) (vars nested1 ++ vars nested2)


-- 1 mark
idMap :: Formula -> IdMap
idMap x
  = zip (vars x) [1..]

--------------------------------------------------------------------------
-- Part II

-- An encoding of the Or distribution rules.
-- Both arguments are assumed to be in CNF, so the
-- arguments of all And nodes will also be in CNF.
distribute :: CNF -> CNF -> CNF
distribute a (And b c)
  = And (distribute a b) (distribute a c)
distribute (And a b) c
  = And (distribute a c) (distribute b c)
distribute a b
  = Or a b

-- 4 marks
toNNF :: Formula -> NNF
toNNF (Not (Not x)) = toNNF x
toNNF (Not (Or x y)) = And (toNNF (Not x)) (toNNF (Not y))
toNNF (Not (And x y)) = Or (toNNF (Not x)) (toNNF (Not y))
toNNF (And x y) = And (toNNF x) (toNNF y)
toNNF (Or x y) = Or (toNNF x) (toNNF y)
toNNF x = x

-- 3 marks
toCNF :: Formula -> CNF
toCNF inp
  = case toNNF inp of
    (Not x) -> Not (toCNF x)
    (And x y) -> And (toCNF x) (toCNF y)
    (Or x y) -> distribute (toCNF x) (toCNF y)
    _ -> toNNF inp

-- 4 marks
flatten :: CNF -> CNFRep
flatten inp = flattenWithMapping (idMap inp) inp
  where
    flattenWithMapping :: IdMap -> CNF -> CNFRep
    flattenWithMapping mapping cnf = case cnf of
      (And x y) -> flattenWithMapping mapping x ++ flattenWithMapping mapping y
      (Or x y) -> [helper mapping x ++ helper mapping y]
      x -> [helper mapping x]

    helper :: IdMap -> CNF -> [Int]
    helper mapping (Var x) = [lookUp x mapping]
    helper mapping (Not (Var x)) = [-(lookUp x mapping)]
    helper mapping (Or x y) = helper mapping x ++ helper mapping y

--------------------------------------------------------------------------
-- Part III

-- 5 marks


propUnits :: CNFRep -> (CNFRep, [Int])
propUnits inp = propUnits' inp ([],[])
  where
    refList = map negate (concat inp)

    exists :: Int -> Bool
    exists x = x `elem` refList

    propUnits' :: CNFRep -> (CNFRep, [Int]) -> (CNFRep, [Int])
    propUnits' [] x = x
    propUnits' (x:xs) (pFormula, pNum)
      |length x == 1 = propUnits' xs (pFormula, pNum ++ x)
      |otherwise = propUnits' xs (pFormula ++ [[y|y <- x, not (exists y)]], pNum ++ [y|y <- x, exists y])


-- 4 marks
dp :: CNFRep -> [[Int]]
dp
  = undefined

--------------------------------------------------------------------------
-- Part IV

-- Bonus 2 marks
allSat :: Formula -> [[(Id, Bool)]]
allSat
  = undefined

