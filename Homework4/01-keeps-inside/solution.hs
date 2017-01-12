rowsToCols :: [[Int]] -> [[Int]]
rowsToCols ([]:_) = []
rowsToCols x = (map head x) : rowsToCols (map tail x)

keepsInside :: [[Int]] -> (Int -> Int) -> [[Int]]
keepsInside matrix func = filter isContained (rowsToCols matrix)
  where
    isContained :: [Int] -> Bool
    isContained x = isContainedIter x x
      where
        isContainedIter [] checkArr = True
        isContainedIter arr checkArr =
          if (elem (func (head arr)) checkArr)
            then isContainedIter (tail arr) checkArr
            else False

main = (print (keepsInside [[1,0,5],[-1,0,2]] (\x -> x^2))) -- => [[1,-1],[0,0]]

