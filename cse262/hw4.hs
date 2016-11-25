--Stephen A Friedman
--HW4
--CSE262
priceEval :: Double -> Double -> Double -> Double
priceEval x y z
	  |y<1		 =error "Turtles never lose value"
          |z>0		 =priceEval (x*y) y (z-1)
          |z==0     	 =x
	  |otherwise	 =x

mysum :: [Int] -> Int
mysum x
    |x/=[]              =(head x) + (sum (tail x))
    |otherwise          =0


numWeird :: [Int] -> Int
numWeird x 
	 |length [y | y <- x,odd y] == 3	=mysum([y|y<-x])
	 |otherwise   	   =length [y | y <- x, odd y] - length [y | y
	 <- x, even y] 
	 

data List a = Nil | Cons a (List a)
     deriving (Show)

(+++) :: List a -> List a -> List a
(+++) x Nil = x
(+++) Nil x = x
(+++) (Cons a b) e@(Cons c d) = (Cons a (b +++ e))

--makeList :: [Int] -> List
makeList (x:xs) 
	 |(length xs) > 0		=Cons (x)(makeList(xs))
	 |otherwise     		=(Cons x Nil)

maxList (Nil) = error "Empty List"  
maxList (Cons a (Nil)) = a  
maxList (Cons a (b))
	|a > maxList b	 =a
	|otherwise	         =maxList b
    
removeList elemRemove Nil = Nil
removeList elemRemove (Cons a list)
	   |elemRemove ==a	=list
	   |otherwise  		= Cons a (removeList elemRemove list)

removeListAll elemRemove Nil = Nil
removeListAll elemRemove (Cons a list)
	      |elemRemove ==a		=(removeListAll elemRemove list)
	      |otherwise		=Cons a (removeListAll elemRemove list)


data (Ord a, Eq a)=>Tree a = Empty |Node a (Tree a) (Tree a)
     	  deriving (Show, Eq)

finder(Node a b _)
	       |b == Empty	=a
	       |otherwise	= finder b
insertTree a Empty = Node a Empty Empty
insertTree a (Node b l r)
	   |a <= b = Node b (insertTree a l) r
	   |a >b = Node b l (insertTree a r)

findTree a Empty = False
findTree a (Node b l r)
	 |a == b	=True
	 |a <b		=findTree a l
	 |a>b		=findTree a r
