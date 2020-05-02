import InDyModel

-- Imitation of command string
str = ". toInt increment"

strAliases = ". pipe|$ apply|' flip"

-- Words and their respective long names
listAliases = split '|' strAliases

-- reduce1 :: op -> list -> any

-- Replace string with another
repl :: String -> String
-- r s = get_or_default (filter makePredicate(s) listAliases) 0 s
-- r s = reduce makePredicate(s) listAliases s
repl = 

-- filter predicate list

-- Replace each short aliase with long
replList :: [String] -> [String]
-- r(l) = map repl l
replList = map repl
-- replList = pipe repl (flip map enumAliases)

main :: IO()
main = do
    print listAliases
    print (words str)