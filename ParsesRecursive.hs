{-# LANGUAGE OverloadedStrings #-}

import InDyModel


------------------------------------------
-- Parser level 1
--   simple parsing of word sequences
------------------------------------------

-- String wrap for ridicuilous operator name
_wrap = "operator.operators.integrateddynamics.@@@.name"
_wrapReplace = "@@@"

-- Makes an operator from string
parseWord :: String -> operator
parseWord = pipe (flip (replace _wrapReplace) _wrap) opByName
--parseWord str = opByName (replace _wrapReplace str _wrap)

-- Parse list of operators
-- need to apply word parsing op
parseListOp :: (String -> operator) -> [String] -> operator
parseListOp = pipe2 (pipe (pipe head) (pipe2 tail)) (pipe pipe reduce) apply
-- parseList op list = reduce (pipe op) (tail list) (op (head list))

-- Parse list of operators
parseListOne :: [String] -> operator
parseListOne = parseListOp parseWord

-- Split string on words or catching groups
split :: String -> [String]
split = regex_scan "\(.+?\)|[^ ]+" 0

-- Parse whole code
parseOne :: String -> operator
parseOne = pipe split parseListOne


------------------------------------------
-- Parser level 2
--   one-level parenthesis parsing
------------------------------------------

-- If string contains spaces
isCompound :: String -> Bool
isCompound = stringContains " "


-- Removes top level catching
remCatchAndParse :: String -> String
remCatchAndParse = pipe (pipe (pipe (regex_groups "(^\w+.*|[^()]+)") tail) head) parseOne
-- remCatchAndParse str = head (tail (regex_groups "^\((.*)\)$" str))

-- Takes short string and returns operator
-- If string contain complex operator, parse it first
-- "pipe" -> operatorPipe OR ("(flip pipe)" -> parse "(flip pipe)")
parseWrdOrCatch :: String -> operator
parseWrdOrCatch = pipe2 id id (apply2 (flip (pipe isCompound (flip choice remCatchAndParse)) parseWord))
-- parseWrdOrCatch = pipe2 (pipe2 isCompound remCatchAndParse choice) parseWord apply
-- parseWrdOrCatch str = choice (isCompound str) (remCatchAndParse str) (parseWord str)


-- Parse list of operators
-- Just reduce list one by one
parseListTwo :: [String] -> operator
parseListTwo = parseListOp parseWrdOrCatch


-- Parse whole code but with parenthesis ()
-- Split result by words and catching groups
-- \([^()]+\)|\w+ captures one-level parenthesis or words
parseTwo :: String -> operator
parseTwo = pipe split parseListTwo
-- parseTwo str = pipe trimAndSplit(str) parseListTwo


------------------------------------------
-- Parser level 3
--   support of aliases
------------------------------------------


------------------------------------------
-- Parser tests
--   
------------------------------------------

-- parseOne test
print (parseOne "operator.pipe parse.valuetype.valuetypes.integrateddynamics.integer integer.increment")

-- Test string
-- Result 5 >> 15
print (parseTwo "pipe (flip pipe) (flip pipe) pipe pipe" (+1) (*2) (+3))
print (parseTwo "pipe increment multiply")


-- 5 >> 7
print (parseTwo "operator.pipe parse.valuetype.valuetypes.integrateddynamics.integer (operator.pipe integer.increment integer.increment)")
