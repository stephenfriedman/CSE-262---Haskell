--Stephen Friedman
--Hw5



data Type = Normal | Water | Fire | Grass | Electric 
     deriving (Show,Enum, Eq)


data Pokemon = Pokemon {name :: String
     	       	       	, lifePoints :: Int
			, hitPoints :: Int
			, pokeType :: Type
			} deriving (Show,Eq)


data Trainer = Trainer {tname :: String
     	       	        , pokedex :: [Pokemon]
			} deriving (Show)

--damage - 30
isResistant :: Type -> Type -> Bool
isResistant attacker defender
	    |(defender == Water) && (attacker == Water)		=True
	    |(defender == Water) && (attacker == Fire)		=True
	    |(defender == Fire) && (attacker == Grass)		=True
	    |(defender == Fire) && (attacker == Fire)   	=True
	    |(defender == Grass) && (attacker == Grass)		=True 
	    |(defender == Grass) && (attacker == Water)		=True
	    |(defender == Grass) && (attacker == Electric)	=True
	    |(defender == Electric) && (attacker == Electric)	=True
	    |otherwise	 	     	      	 		=False

--damage * 2
isVulnerable :: Type -> Type -> Bool
isVulnerable attacker defender
	     |(defender == Water) && (attacker == Grass)	=True
	     |(defender == Water) && (attacker == Electric)	=True
	     |(defender == Fire) && (attacker == Water)		=True
	     |(defender == Grass) && (attacker == Fire)		=True
	     |otherwise	   	     	       	  		=False

pikachu = Pokemon "Pikachu" 50 15 Electric
squirtle = Pokemon "Squirtle" 50 20 Water
wartortle = Pokemon "Wartortle" 80 40 Water
snivy = Pokemon "Snivy" 60 20 Grass
tepig = Pokemon "Tepig" 70 30 Fire
jigglypuff = Pokemon "Jigglypuff" 60 20 Normal
 


group = Trainer "PGroup" [pikachu, snivy, tepig]
turtles = Trainer "Turtles" [squirtle, wartortle, wartortle]
puff = Trainer "Puffs!" [jigglypuff, jigglypuff, jigglypuff]



combat:: Trainer -> Trainer -> String
combat t1 t2 	=combat2 t1 t2 50
      


combat2 :: Trainer -> Trainer -> Int -> String
combat2 t1 t2 turn
	|turn <= 0							="Combat too long!"
      	|rip (pokedex t1) == [] && rip (pokedex t2) == []		="No Winner"
        |(not (rip (pokedex t1) == [])) && (rip (pokedex t2) == [])	=tname t1
        |(rip (pokedex t1) == []) && (not (rip (pokedex t2) == []))	=tname t2
        |(turn `mod` 2) ==  0	      	     	       		       	=combat2 t1 (exa t1 t2)  (turn-1)   	    	     	    		
	|otherwise   	    						=combat2 (exa t2 t1) t2 (turn-1)
       
exa t1 t2 = Trainer (tname t2) (rip(updatePokedex (pokedex t1) (pokedex t2)))    

updatePokedex p1 ((Pokemon a b c d) :xs) =  (Pokemon a (b- (hitDamage(head p1)(Pokemon a b c d)) ) c d):xs
	      

     	 
vulnHit :: Pokemon -> Pokemon -> Int
vulnHit p1 p2
	|(isVulnerable (pokeType p1) (pokeType p2))==True     =(hitPoints  p1)*2
	|otherwise    		    	      		    =hitPoints p1

resisHit :: Pokemon -> Pokemon -> Int
resisHit p1 p2
	 |(isResistant (pokeType p1) (pokeType p2))==True		= -30
	 |otherwise    		     	       				=0

hitDamage :: Pokemon -> Pokemon -> Int
hitDamage p1 p2 = (vulnHit p1 p2) + (resisHit p1 p2)

rip :: [Pokemon] -> [Pokemon]
rip pd
    |length pd == 0				=[]
    |(lifePoints (head pd)) <=0		=rip (tail pd)
    |(lifePoints (head pd)) >=0		=pd


       
