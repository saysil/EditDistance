
module EditDistance where

{- Description:
 -
 - We can view the longest common subsequence as
 - the shortest path on an edit graph where a diagonal edge
 - of length 0 exists when the mapping tuple of a vertex 
 - has two equal pairs and horizontal 
 - edges of length 1 exist everywhere else
 - The A* Path Finding algorithm is used with the euclidean distance heuristic
 -}

{- Example Graph:
 - X A T C G A T C G
 - A \ + + + \ + + +
 - T + \ + + + \ + +
 - G + + + \ + + + \
 - G + + + \ + + + \
 -
 -
 - X A T C G A T C G
 - A X
 - T   X X
 - G       X X X X
 - G               X
 -
 - |LCS| = 4, LCS = A T G G
 -}

import Data.HashSet as H
import Data.Sequence as S
import Data.Graph.AStar
import Data.Maybe

lcs :: (Eq a) => Seq a -> Seq a -> [a]
-- Prepend origin to a star list
lcs xs ys = evaluateList $ maybe [] (\l->(-1, -1):l) (aStar (H.fromList.getNeighbors) (getDistance) (getHeuristic) (isFinished) (-1, -1))
        where getNeighbors :: (Int, Int) -> [(Int, Int)]
              getNeighbors (a, b) = (right:down:diagonal)
                        where right = (a+1, b)
                              down = (a, b+1)
                              diagonal = if ((S.lookup (a+1) xs) == (S.lookup (b+1) ys)) then [(a+1, b+1)] else []

              getDistance :: (Int, Int) -> (Int, Int) -> Int
              getDistance (a1, b1) (a2, b2)
                        | a1 + 1 == a2 && b1 + 1 == b2 = 0
                        | otherwise = 1

              getHeuristic :: (Int, Int) -> Int
              getHeuristic (a, b) = (floor.sqrt.fromIntegral) (((S.length xs) - a)^2 + ((S.length ys) - b)^2)

              isFinished :: (Int, Int) -> Bool
              isFinished (a, b)
                        | a + 1 == S.length xs && b + 1 == S.length ys = True
                        | otherwise = False

              evaluateList [] = []
              evaluateList (_:[]) = []
              evaluateList ((a1, b1):(s@((a2, b2):rest)))
                        | a1 + 1 == a2 && b1 + 1 == b2 = (index xs a2):(evaluateList s)
                        | otherwise = evaluateList s





