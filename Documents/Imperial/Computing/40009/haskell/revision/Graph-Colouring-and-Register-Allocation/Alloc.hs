module Alloc where

import Data.Maybe
import Data.List

import Types
import Examples

------------------------------------------------------
--
-- Part I
--
count :: Eq a => a -> [a] -> Int
count ref lst
  = sum (map (\x -> if x == ref then 1 else 0) lst)

degrees :: Eq a => Graph a -> [(a, Int)]
degrees (int, pairs)
  = map (\x -> (x, num x)) int
  where
    num x = count x (map fst pairs) + count x (map snd pairs)

neighbours :: Eq a => a -> Graph a -> [a]
neighbours ref (_, pairs)
  = [x|(x,y) <- pairs, y == ref] ++ [y|(x,y) <- pairs, x == ref]

removeNode :: Eq a => a -> Graph a -> Graph a
removeNode rm (refs, pairs)
  = ([x|x <- refs, x /= rm], [(a,b)|(a,b) <- pairs, a /= rm, b /= rm])

------------------------------------------------------
--
-- Part II
--
colourGraph :: (Ord a, Show a) => Int -> Graph a -> Colouring a
colourGraph numCol graph@(refs, edges)
  |length refs == 2 = zip (least: [x | x<-refs, x/=least]) [2,1]
  |minValue > numCol = (least, 0):cMap
  |otherwise = (least, minValue):cMap
    where
      minValue = head ([1..] \\ [x|x <- [b | (a, b) <- cMap, a `elem` currentNeighbours], x/=0])
      currentNeighbours = neighbours least graph
      least = fst (head (sortOn snd (degrees graph)))
      cMap = colourGraph numCol newGraph
        where newGraph = removeNode least graph

------------------------------------------------------
--
-- Part III
--
buildIdMap :: Colouring Id -> IdMap
buildIdMap inp
  = ("return", "return") : map (\(a,b) -> if b > 0 then (a, "R" ++ show b) else (a, a)) inp


buildArgAssignments :: [Id] -> IdMap -> [Statement]
buildArgAssignments refs idmap
  = map (\x -> Assign (lookUp x idmap) (Var x)) refs

renameExp :: Exp -> IdMap -> Exp
-- Pre: A precondition is that every variable referenced in 
-- the expression is in the idMap. 
renameExp (Const x) _ = Const x
renameExp (Var x) idmap = Var (lookUp x idmap)
renameExp (Apply op exp1 exp2) idmap = Apply op (renameExp exp1 idmap) (renameExp exp2 idmap)

renameBlock :: Block -> IdMap -> Block
-- Pre: A precondition is that every variable referenced in 
-- the block is in the idMap. 
renameBlock block idmap = checkEquiv (map (renameBlock' idmap) block)
  where
    renameBlock' idmap (Assign x exp) = Assign (lookUp x idmap) (renameExp exp idmap)
    renameBlock' idmap (If exp b1 b2) = If (renameExp exp idmap) (renameBlock (checkEquiv b1) idmap) (renameBlock (checkEquiv b2) idmap)
    renameBlock' idmap (While exp b1) = While (renameExp exp idmap) (renameBlock (checkEquiv b1) idmap)

    checkEquiv [] = []
    checkEquiv (whole@(Assign x (Var y)):xs)
      | x==y = checkEquiv xs
      | otherwise = whole:checkEquiv xs
    checkEquiv (whole@(Assign x _):xs) = whole:checkEquiv xs
    checkEquiv ((If x b1 b2):xs) = If x (checkEquiv b1) (checkEquiv b2):checkEquiv xs
    checkEquiv ((While x b1):xs) = While x (checkEquiv b1):checkEquiv xs


renameFun :: Function -> IdMap -> Function
renameFun (f, as, b) idMap
  = (f, as, buildArgAssignments as idMap ++ renameBlock b idMap)

-----------------------------------------------------
--
-- Part IV
--
buildIG :: [[Id]] -> IG
buildIG
  = undefined

-----------------------------------------------------
--
-- Part V
--
liveVars :: CFG -> [[Id]]
liveVars
  = undefined

buildCFG :: Function -> CFG
buildCFG
  = undefined
