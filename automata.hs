import Control.Monad
import Data.Maybe (catMaybes)
import System.IO
import System.Environment

type Generation = [Char]

data RulePattern = TwoDim (Char,Char,Char) Char
  deriving (Show, Eq)

type Rule = [RulePattern]

rp1 = TwoDim ('X','X','X') '_'
rp2 = TwoDim ('X','X','_') '_'
rp3 = TwoDim ('X','_','X') '_'
rp4 = TwoDim ('X','_','_') 'X'
rp5 = TwoDim ('_','X','X') 'X'
rp6 = TwoDim ('_','X','_') 'X'
rp7 = TwoDim ('_','_','X') 'X'
rp8 = TwoDim ('_','_','_') '_'
rule = [rp1,rp2,rp3,rp4,rp5,rp6,rp7,rp8] :: Rule

main = do
      (getem:_) <- getArgs
      putStrLn $ unlines (take (read getem :: Int) (kickstart rule gen) )
      where 
          gen = replicate 50 '_' ++ ['X'] ++ replicate 50 '_'

kickstart :: Rule -> Generation -> [Generation]
kickstart rule gen = kickstart' rule gen [] [gen]

kickstart' :: Rule -> Generation -> Generation -> [Generation] -> [Generation]
kickstart' rule gen next results 
                | results == []             = kickstart' rule (generation rule gen next) [] [generation rule gen next]
                | length results == 1       = results ++ kickstart' rule (generation rule gen next) [] [] 


-- given a rule (list of rule patterns) and a sample, 
-- calculate the resulting output for the sample.
-- in this version we try to apply all of our patterns to the sample, and
-- return the first one that matched.  thanks to laziness, this is an efficient
-- way to do it.
-- catMaybes :: [Maybe a] -> [a] is used to filter the list down to non-Nothing values.
applyRule :: [RulePattern] -> [Char] -> Char
applyRule pats samp = 
    case catMaybes (map (`applyPat` samp) pats) of
        []         -> '_'     -- nothing matched
        (result:_) -> result  -- got a result

-- try to apply a pattern, returning Just a result if it matches, otherwise Nothing.
applyPat :: RulePattern -> [Char] -> Maybe Char
applyPat (TwoDim x y) zs | matches x zs = Just y
                         | otherwise    = Nothing

-- take a rule set, initial generation and return convoluted generation
generation :: Rule -> Generation -> Generation -> Generation 
generation rule origGen nextGen 
    | length origGen == length nextGen = ['_'] ++ nextGen
    | otherwise                        = generation rule ( shiftL origGen )  (nextGen ++ [applyRule rule $ take 3 origGen])

-- go :: Rule -> Generation -> [Generation]
-- go rule gen 
--    | 

shiftL :: [Char] -> [Char]
shiftL []     = []
shiftL (x:xs) = xs ++ [x]

showRuleTpl :: RulePattern -> Char
showRuleTpl (TwoDim _ y) = y

matches :: Eq a => (a,a,a) -> [a] -> Bool
matches (x,y,z) v = [x,y,z] == v










