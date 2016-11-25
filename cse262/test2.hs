myreverse :: [a] -> [a]
myreverse a = foldl(\acc x -> x : acc) [] a

lengthLambda :: [a] -> Integer
lengthLambda = (\x ->  1 + (lengthLambda (tail x)))
