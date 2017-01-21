type Product = (String,Double)
type StoreAvailability = [Product]

sumProductsPrices :: StoreAvailability -> Double
sumProductsPrices [] = 0.0
sumProductsPrices (p:ps) = (snd p) + sumProductsPrices ps

findAveragePrice :: Double -> Int -> Double
findAveragePrice sum count = sum / (fromIntegral count)

setPriceToDistanceFromAverage :: StoreAvailability -> Double -> StoreAvailability
setPriceToDistanceFromAverage [] _ = []
setPriceToDistanceFromAverage (p:ps) avg = (fst p, (abs ((snd p) - avg))) : (setPriceToDistanceFromAverage ps avg)

sortByLowestPrice :: StoreAvailability -> StoreAvailability
sortByLowestPrice products = bubbleSort products 0
  where
    bubbleSort products i
      | i == length products = products
      | otherwise = bubbleSort (sortByLowestPriceIter products) (i + 1)

    sortByLowestPriceIter (x:y:products)
      | (snd x) > (snd y) = y : sortByLowestPriceIter (x:products)
      | otherwise = x : sortByLowestPriceIter (y:products)
    sortByLowestPriceIter (x) = (x)

closestToAverage :: StoreAvailability -> String
closestToAverage products = (fst (head newProducts))
  where
    pricesSum = sumProductsPrices products
    pricesAvg = findAveragePrice pricesSum (length products)
    newProducts = sortByLowestPrice (setPriceToDistanceFromAverage products pricesAvg)

main = do
  (print (closestToAverage [("Skumriq", 10.50), ("Bob", 3.20), ("Leshta", 5.80)]))
  (print (setPriceToDistanceFromAverage [("Skumriq", 10.50), ("Bob", 3.20), ("Leshta", 5.80)] 6.50))
  (print (sortByLowestPrice (setPriceToDistanceFromAverage [("Skumriq", 10.50), ("Bob", 3.20), ("Leshta", 5.80)] 6.50)))
