isUnique :: Int -> [Int] -> Int -> Bool
isUnique x [] count
  | count == 0 || count == 1 = True
  | otherwise                = False

isUnique x l count
  | x == last l = isUnique x (init l) (count + 1)
  | otherwise   = isUnique x (init l) count

selectUnique :: [Int] -> [Int]
selectUnique xs = [x | x <- xs, (isUnique x xs 0)]

sumList :: [Int] -> Int
sumList []     = 0
sumList (x:xs) = x + sumList xs

sumUnique :: [[Int]] -> Int
sumUnique []  = 0
sumUnique (l:ls) = (sumList (selectUnique l)) + sumUnique ls

main = (print (sumUnique [[1,2,3,2], [-4,-4], [5]]))
