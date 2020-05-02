import InDyModel
import Data.Text

-- Imitation of command string
str = ". toInt increment"

strAliases = "^\. pipe|$ apply|' flip"

-- Words and their respective long names
listAliases = split_on '|' strAliases

-- reduce1 :: op -> list -> any

-- Replace regex toStr inStr

replaceWhole :: a -> a -> a -> a
-- replaceWhole a b c = choice (a == c) b a
replaceWhole = replace

alsRgx :: String -> String -- alsRgx(a) = head words(a)
alsRgx = pipe words head

alsWrd :: String -> String -- alsWrd(a) = head tail words(a)
alsWrd = pipe (pipe words tail) head

--words :: String -> [String]

-- "." -> "^\. pipe" -> "pipe"
reducer :: String -> String -> String
-- r a b = replaceWhole (alsRgx(b)) (alsWrd(b)) a
reducer = flip (pipe2 alsRgx alsWrd replaceWhole)

-- Replace each short aliase with long
-- Replace string with another
-- Folding each aliase pair, changing if it match pattern
replList :: [String] -> [String]
replList = list_map (reduce reducer listAliases)

main :: IO()
main = do
    print listAliases
    print (words str)