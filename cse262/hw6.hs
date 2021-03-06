--Stephen Friedman
--CSE262
--Hw6

--extract :: x -> x-> [x] -> [x]
extract _ _ [] =[]
extract min  max x
    	|length x == 0	   =[]
	|(head x) > max	   = (head x) : (extract min max (tail x))	
	|(head x) < min    = (head x) : (extract min max (tail x))
	|otherwise 	   = (extract min max  (tail x))

superimpose _ [] = []
superimpose [] _ = []
superimpose x y = zipWith (\a b -> (((max a b)*2)+(min a b))/3) x y

supersuperimpose _ [[]] = []
supersuperimpose [[]] _ = []
supersuperimpose x y = zipWith superimpose x y

maxAndCount [] = error "Empty List"
maxAndCount x = (m , length [a | a <- x, a == m])
	    where m = maximum x

lengthLambda  = (\l -> length l)

adjuster :: [x] -> Int -> x -> [x]
adjuster myList len app
	 |len < 0		=error "Invalid Size"
	 |length myList == len	=myList
	 |length myList > len   = adjuster (tail myList) len app
	 |length myList < len 	= adjuster (myList ++ [app]) len app

--insertSort :: [z] -> z -> [z]
--insertSort (m:myList) add
	  -- |add == [] 		=[]
--	   |m <= add	  	= m : (insertSort myList add)
--	   |otherwise		= add : myList


insertSort [] z = [z]
insertSort (x:xs) z 
	   |z > x	= [x] ++ insertSort xs z
	   |z < x 	= [z] ++ (x:xs)
	   |z == x 	= [z] ++ insertSort xs z

--wordMaker :: [Char] -> [[Char]]
--wordMaker (s:str)
--	  |s == null	=[]
 --	  |s /=	' '	=(s ++ o) ++ (wordMaker str)
--	  |otherwise		= o ++ (wordMaker str)
--	  where o = [[]]
  	   
lengthFold l = foldl (\acc x -> acc + 1) 0 l
