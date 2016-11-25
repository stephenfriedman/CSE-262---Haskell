--Stephen Friedman
--CSE262 Hw3
appendEvens :: Integral x => [x] -> [x] -> [x]
appendEvens x y
	    |y == []	= x
	    |(head y) `mod` (2) == 0	= appendEvens (x ++ [head y]) (tail y)
	    |otherwise	    	   	= appendEvens (x) (tail y)

infiniteBuzz = [x | x <- [1,2..], (x `mod` 5 == 0) || (x `mod` 7 ==0)]

evenGrabber :: Int -> Int -> [Int]
evenGrabber x y = let z = [2,4..]
	      	  in if (y - x) >= 0
	      	     then drop x (take (y+1) z)
		     else []
patternChoice :: [Int] -> [Int]
patternChoice x
	      |null x	= []
	      |head x  == 0	 = tail x
	      |(head x) `mod` 2 == 1   	 = drop ((head (tail x))) x
	      |(head(tail x)) `mod` 2 == 0 	= take 1 (drop 2 x)
	      |otherwise   = [y - (head x) | y <- x]

	      
partialFunc ::   (a->b->c)-> a -> b -> c
partialFunc x y = x y
