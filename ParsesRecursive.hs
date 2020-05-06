-- String wrap for ridicuilous operator name
_wrap = "operator.operators.integrateddynamics.@@@.name"
_wrapReplace = "@@@"

-- Makes an operator from string
parseWord :: String -> operator
parseWord = pipe (flip (replace _wrapReplace) _wrap) opByName
--parseWord str = opByName (replace _wrapReplace str _wrap)

isCompound :: String -> Bool
isCompound = stringContains " "

-- Takes short string and returns operator
-- If string contain complex operator, parse it first
-- "pipe" -> operatorPipe OR ("(flip pipe)" -> parse "(flip pipe)")
parseWrdOrCatch :: String -> operator
parseWrdOrCatch = pipe2 (pipe2 isCompound parseTwo choice) parseWord id
-- parseWrdOrCatch str = choice (isCompound str) (parseTwo str) (parseWord str)

-- Parse list of operators
-- Just reduce list one by one
parseList :: [String] -> operator
parseList = pipe2 tail (pipe head parseWrdOrCatch) (reduce (pipe parseWrdOrCatch))

-- Parse whole code but with parenthesis ()
-- Split result by words and catching groups
-- \([^()]+\)|\w+ captures one-level parenthesis or words
parseTwo :: String -> operator
parseTwo = flip pipe parseList (regex_groups "(?!^\()(\([^()]+\)|\w+)")
-- parseTwo str = pipe trimAndSplit(str) parseList


------------------------------------------
--
------------------------------------------

-- Test string
-- Result 5 >> 15
print (parseTwo "pipe (flip pipe) (flip pipe) pipe pipe" (+1) (*2) (+3))
print (parseTwo "pipe increment multiply")
print (parseTwo "operator.pipe parse.valuetype.valuetypes.integrateddynamics.integer (operator.pipe integer.increment integer.increment)")
