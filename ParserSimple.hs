-- String wrap for ridicuilous operator name
wrap = "operator.operators.integrateddynamics.@@@.name"
wrapReplace = "@@@"

-- Makes an operator from string
parseSimple :: String -> operator
parseSimple = pipe (apply (flip (apply replace wrapReplace)) wrap) opByName
--parseSimple str = opByName (replace wrapReplace str wrap)

-- Simple reduce operator appling [n-1] element to [n] of list
opReduce :: (a->b) -> String -> c
opReduce = pipe parseSimple
-- opReduce op s = op(parseSimple s)

-- Parse list of operators
parseList :: [a] -> operator
parseList = pipe2 tail (pipe head parseSimple) (reduce opReduce)
-- parseList list = reduce opReduce (tail list) (parseSimple (head list))

-- Parse whole code
parse :: String -> operator
parse = pipe (split_on " ") parseList