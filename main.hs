{-# LANGUAGE OverloadedStrings #-}

import InDyModel
import Data.Text hiding (words, head, tail, replace)
import Data.String
-- import Data.Text (words)
-- import Data.String.Utils


-- Patterns of all aliases
strAliases = ". pipe|$ apply|* flip"

-- Words and their respective long names
listAliases = split_on "|" strAliases

join = pipe2 id id

replace :: (Eq a) => a -> a -> a -> a
-- replace a b c = choice (a /= c) c b
replace = flip . join . (choice .) . (/=)
-- replace = -- TODO make inline function

-- Alias regex (first word)
alsRgx :: String -> String -- alsRgx(a) = head words(a)
alsRgx = pipe words head

-- Alias word (second word)
alsWrd :: String -> String -- alsWrd(a) = head tail words(a)
alsWrd = pipe (pipe words tail) head

-- "." -> ". pipe" -> "pipe"
reducer :: String -> String -> String
-- reducer a b = replace (alsRgx(a)) (alsWrd(a)) b
reducer = pipe2 alsRgx alsWrd replace

-- Replace each short aliase with long
-- Replace string with another
-- Folding each aliase pair, changing if it match pattern
replList :: [String] -> [String]
replList l = list_map (reduce reducer listAliases) l

-- Imitation of command string
str = ". $ increment"
str_split = split_on " " str

main :: IO()
main = do
    print "--------------------"
    print "List of aliases:"
    print listAliases

    print "String to replace:"
    print (str_split)

    print "Replaced result:"
    print (replList str_split)