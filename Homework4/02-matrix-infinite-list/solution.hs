import Data.List

matrixInfList :: [[a]] -> [a]
matrixInfList matrix = cycle (mergeList (transpose matrix))
  where
    mergeList matrix = (foldl1 (++) matrix)

main = (print (matrixInfList [[1,2],[3,4],[5,6]]))
