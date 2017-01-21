isUnique :: Int -> [Int] -> Int -> Bool
isUnique _ [] count
  | count == 0 || count == 1 = True
  | otherwise = False

isUnique number list count
  | number == last list = isUnique number (init list) (count + 1)
  | otherwise = isUnique number (init list) count

selectUnique :: [Int] -> [Int]
selectUnique list = filter (\x -> isUnique x list 0) list

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

sumUnique :: [[Int]] -> Int
sumUnique [] = 0
sumUnique (l:ls) = sumList (selectUnique l) + sumUnique ls

main = do
  (print (sumUnique [[1,2,3,2], [-4,-4], [5]]))
  (print (sumUnique [[2,2,2],[3,3,3],[4,4,4]]))
  (print (sumUnique [[1,2,3],[4,5,6],[7,8,9]]))
