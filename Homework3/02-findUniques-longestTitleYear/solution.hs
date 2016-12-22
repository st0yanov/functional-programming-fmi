isUnique :: Int -> [(String, Int)] -> Int -> Bool
isUnique x [] count
  | count == 0 || count == 1 = True
  | otherwise                = False

isUnique x l count
  | x == (snd (last l)) = isUnique x (init l) (count + 1)
  | otherwise           = isUnique x (init l) count

findUniques :: [(String, Int)] -> [String]
findUniques xs = [fst x | x <- xs, (isUnique (snd x) xs 0)]

longestTitleYear :: [(String, Int)] -> Int
longestTitleYear xs = longestTitleIter xs 0 0
  where
    longestTitleIter [] saved_year saved_length = saved_year
    longestTitleIter l saved_year saved_length =
      if (length (fst (last l))) > saved_length
        then longestTitleIter (init l) (snd (last l)) (length (fst (last l)))
        else longestTitleIter (init l) saved_year saved_length

lib=[("SICP",1996),
 ("Learn You a Haskell for Great Good",2011),
 ("Real World Haskell",2008),
 ("Programming in Haskell", 2011)]
main = do
        (print (findUniques lib))
        (print (longestTitleYear lib))
