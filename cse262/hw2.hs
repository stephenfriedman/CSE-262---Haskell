--Stephen A. Friedman
--CSE 262 
--Homework #2

--(1) Prelude> :show bindings
--x:: Num t => t = _

--(2)
--main = interact wordCount
  --  where wordCount input = show (length (words input))++  "\n"
--(3)
--main = interact wordCount
   -- where wordCount input = show ((length  input) - length(lines
--input))++  "\n"

--plusFive :: Int -> Int 
plusFive :: Double -> Double
plusFive x = x + 5

plusTen :: Double -> Double
plusTen x = (plusFive (plusFive x))

distance :: Double -> Double -> Double
distance x y = let a = x * x  
	       in let b = y *y 
	       	  in let c = a + b 
	       	     in sqrt c
decideModify :: Int -> Int -> Int
decideModify x y = if (x `mod` 2 == 0)
		   then let c = x - y
			in c
		   else let c = x + y 
		   	in c 
bothFirsts :: [a] -> [a] -> [a] 
bothFirsts x y  = [head x]++[head y]

badDouble :: Double -> Double
--data X = Float | Int
--X :: Float|Int
--badDouble :: X -> X
badDouble x
	| x<=0	= 0
	| otherwise	= 2 + (badDouble (x-1))	       


myReverse :: [a] -> [a]
myReverse x
	|null x	       	= []  
	|otherwise	= myReverse(tail x) ++ [head x]

foreverIncrement :: [Double] -> [Double]
foreverIncrement x = foreverIncrement (take 4 (x ++ [(head (myReverse x))+1]))
		     --in c take 3 c
		     --then take 3 (c)
		    -- then foreverIncrement(c)
--foreverIncrement x = foreverIncrement ( (x ++ [(head (myReverse x))+1])
