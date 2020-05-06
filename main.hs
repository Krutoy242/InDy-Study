{-# LANGUAGE OverloadedStrings #-}

import InDyModel
import Data.Text hiding (words, head, tail, replace)
import Data.String

-- Patterns of all aliases
strAliases = ". pipe|$ apply|* flip"

-- Words and their respective long names
listAliases = split_on "|" strAliases

-- TODO: add to InDy
-- words = split_on " "

join = pipe2 id id

replace :: (Eq a) => a -> a -> a -> a
-- replace a b c = choice (a /= c) c b
replace = flip . join . (choice .) . (/=)
-- replace = -- TODO make inline function

-- Alias regex (first word)
alsRgx :: String -> String -- alsRgx(a) = head words(a)
alsRgx = pipe words head

-- Alias word (second word)
alsWrd :: String -> String
-- alsWrd(a) = head tail words(a)
-- alsWrd = pipe (pipe words tail) head
alsWrd = pipe (pipe words tail) head

-- reducer(".", ". pipe") = "pipe"
reducer :: String -> String -> String
-- reducer a b = replace (alsRgx(a)) (alsWrd(a)) b
reducer = pipe2 alsRgx alsWrd replace

-- Replace string with another
replList :: [String] -> [String]
replList l = list_map (reduce reducer listAliases) l

---------------------------------
-- Tests
---------------------------------
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = pipe (flip pipe) (flip pipe)
-- (.:) = pipe2 id id pipe (flip pipe)
-- (.:) = pipe flip (pipe2 id id pipe) pipe


h = pipe pipe pipe -- h :: (a -> b) -> ((a -> c) -> d) -> (b -> c) -> d
-- f1 :: a -> b
-- f2 :: (a -> c) -> d
-- f3 :: b -> c
-- h f1 f2 f3 a = f2 (f3 (f1 a))

-- pipe pipe (flip pipe) -- (d -> b) -> (a -> b -> c) -> a -> d -> c
-- f1 :: a -> b
-- f2 :: c -> b -> d
-- h f1 f2 a c = f2 c (f1 a)

-- Test inliner
f :: (Num a) => a -> a
-- f = pipe (pipe (+1) (*2)) (+3)
f = (.:) pipe pipe (+1) (*2) (+3)
---------------------------------
-- 
---------------------------------


-- Imitation of command string
str = ". $ increment"
str_split = split_on " " str
main :: IO()
main = do
    print "--------------------"
    print ((flip .) . (flip .))
    print "List of aliases:"
    print listAliases

    print "String to replace:"
    print (str_split)

    print "Replaced result:"
    print (replList str_split)