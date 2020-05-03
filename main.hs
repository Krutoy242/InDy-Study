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

-- Imitation of command string
str = ". $ increment"
str_split = split_on " " str

flip_pipe = flip pipe
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = pipe flip_pipe flip_pipe

-- Test inliner
f :: (Num a) => a -> a
-- f = pipe (pipe (+1) (*2)) (+3)
f = (.:) pipe pipe (+1) (*2) (+3)

-- (a -> b) -> ((a -> c) -> d) -> (b -> c) -> d

main :: IO()
main = do
    print "--------------------"
    print (f 5)
    print "List of aliases:"
    print listAliases

    print "String to replace:"
    print (str_split)

    print "Replaced result:"
    print (replList str_split)