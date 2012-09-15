import Control.Monad

type Generation = [Int]

data RulePattern = TwoDim (Int,Int,Int) Int
  deriving (Show, Eq)

type Rule = [RulePattern]

rp1 = TwoDim (1,1,1) 0
rp2 = TwoDim (1,1,0) 0
rp3 = TwoDim (1,0,1) 1
rp4 = TwoDim (1,0,0) 1
rp5 = TwoDim (0,1,1) 1
rp6 = TwoDim (0,1,0) 1
rp7 = TwoDim (0,0,1) 0
rp8 = TwoDim (0,0,0) 0
rule = [rp1,rp2,rp3,rp4,rp5,rp6,rp7,rp8] :: Rule

main =  do
    dd rule gen next
    where 
        gen = [0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0]
        next = []

dd :: Rule -> Generation -> Generation -> IO ()
dd rule gen next = do
        print $ generation rule gen next
        dd rule ( generation rule gen next ) [] 
       


-- given a list of rules and a sample, get the resulting output for the sample.
getVal :: [RulePattern] -> [Int] -> Int
getVal [] (v) = 0
getVal [r] (v) = getVal' r v
getVal (rs) (v) 
    | getVal' (head rs) v < 0 = getVal (tail rs) v  -- rule doesn't match, recurse to next rule
    | otherwise               = getVal' (head rs) v -- rule matches, return the result

-- worker for getVal.  
getVal' :: RulePattern -> [Int] -> Int
getVal' (TwoDim x y) z | matches x z = y
                       | otherwise   = -1
-- take a rule set, initial generation and return convoluted generation
generation :: Rule -> Generation -> Generation -> Generation 
generation rule origGen nextGen 
    | length origGen == length nextGen = nextGen
    | otherwise                        = generation rule ( shiftL origGen )  (nextGen ++ [getVal rule $ take 3 origGen] )

-- go :: Rule -> Generation -> [Generation]
-- go rule gen 
--    | 

shiftL :: [Int] -> [Int]
shiftL [] = []
shiftL xs = (tail xs) ++ [head xs]

showRuleTpl :: RulePattern -> Int
showRuleTpl (TwoDim x y) = y

matches :: Eq a => (a,a,a) -> [a] -> Bool
matches x (y) = ( myToList x ) == y 

myToList :: (a,a,a) -> [a] 
myToList (x,y,z) = [x,y,z]

myFst :: (x,y,z) -> x
myFst (x,y,z) = x

mySnd :: (x,y,z) -> y
mySnd (x,y,z) = y

myTrd :: (x,y,z) -> z
myTrd (x,y,z) = z









