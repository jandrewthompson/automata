import Control.Monad

type Generation = [Char]

data RulePattern = TwoDim (Char,Char,Char) Char
  deriving (Show, Eq)

type Rule = [RulePattern]

rp1 = TwoDim ('X','X','X') '_'
rp2 = TwoDim ('X','X','_') '_'
rp3 = TwoDim ('X','_','X') 'X'
rp4 = TwoDim ('X','_','_') 'X'
rp5 = TwoDim ('_','X','X') 'X'
rp6 = TwoDim ('_','X','_') 'X'
rp7 = TwoDim ('_','_','X') '_'
rp8 = TwoDim ('_','_','_') '_'
rule = [rp1,rp2,rp3,rp4,rp5,rp6,rp7,rp8] :: Rule

main =  do
    print gen
    dd rule gen next
    where 
        gen = ['_','_','_','_','_','_','_','_','_','_','_','_','_','_','_','X','_','_','_','_','_','_','_','_','_','_','_','_','_','_','_']
        next = []

dd :: Rule -> Generation -> Generation -> IO ()
dd rule gen next = do
        print $ generation rule gen next
        dd rule ( generation rule gen next ) [] 
       


-- given a list of rules and a sample, get the resulting output for the sample.
getVal :: [RulePattern] -> [Char] -> Char
getVal [] (v) = '_'
getVal [r] (v) = getVal' r v
getVal (rs) (v) 
    | getVal' (head rs) v == 'E' = getVal (tail rs) v  -- rule doesn't match, recurse to next rule
    | otherwise               = getVal' (head rs) v -- rule matches, return the result

-- worker for getVal.  
getVal' :: RulePattern -> [Char] -> Char
getVal' (TwoDim x y) z | matches x z = y
                       | otherwise   = 'E'
-- take a rule set, initial generation and return convoluted generation
generation :: Rule -> Generation -> Generation -> Generation 
generation rule origGen nextGen 
    | length origGen == length nextGen = nextGen
    | otherwise                        = generation rule ( shiftL origGen )  (nextGen ++ [getVal rule $ take 3 origGen] )

-- go :: Rule -> Generation -> [Generation]
-- go rule gen 
--    | 

shiftL :: [Char] -> [Char]
shiftL [] = []
shiftL xs = (tail xs) ++ [head xs]

showRuleTpl :: RulePattern -> Char
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









