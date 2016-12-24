extractRLEHelper :: (Eq a) => (a, Int) -> [a]
extractRLEHelper (_, 0) = []
extractRLEHelper (char, counter) = (char : (extractRLEHelper (char, counter - 1)))

extractRLE :: (Eq a) => [(a,Int)] -> [a]
extractRLE [] = []
extractRLE ((char, rep) : rest) = (extractRLEHelper (char, rep)) ++ (extractRLE rest)

compressRLEHelper :: (Eq a) => a -> [a] -> Int
compressRLEHelper _ [] = 0
compressRLEHelper ch (x : xs) = if (x /= ch)
  then 0
  else 1 + compressRLEHelper ch xs

compressRLE :: (Eq a) => [a] -> [(a,Int)]
compressRLE [] = []
compressRLE (x : xs) =
  ((x, position) : compressRLE (drop position (x : xs)))
  where position = compressRLEHelper x (x : xs)

getRLE :: (Eq a) => [(a,Int)] -> Int -> a
getRLE ((char, rep) : rest) index = getRLEIter ((char, rep) : rest) index 1
  where
    getRLEIter ((char, rep) : rest) index position
      | position == index = char
      | rep == 0          = getRLEIter rest index position
      | otherwise         = getRLEIter ((char, rep - 1) : rest) index (position + 1)

main = do
  (print (extractRLE [("m",1),("i",1),("s",2),("i",1),("s",2),("i",1),("p",2),("i",1)]))
  (print (compressRLE "mississippi"))
  (print (getRLE [("m",1),("i",1),("s",2),("i",1),("s",2),("i",1),("p",2),("i",1)] 5))
