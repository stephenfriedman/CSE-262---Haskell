

helper []		=[]
helper x:xs 
       |null xs		=[x]       
       |otherwise	=  [x] ++ helper (filter (/=x)xs)
