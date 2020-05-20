
c_random    // Extra-Dimensional Reader
c_itemsList // Inventory Reader (common storage)

noNullLen = Pipe (Apply Filter IsNotNull) ListLength
devide_2 = Apply (Flip RightZeroShift) 1
rnd = Apply Modulus (RightZeroSHift c_random 1)
mult_2 = Apply (Flip LeftShift) 1
rndPosInputs = Pipe (Pipe (Pipe noNullLen devide_2) rnd) mult_2

lift2M = Apply2 Pipe2 Id Id
get_flip = Flip ListGet
rndInput  = Apply lift2M (Pipe rndPosInputs get_flip)
rndOutput = Apply lift2M (Pipe (Pipe rndPosInputs Increment) get_flip)

resCount = Apply ItemListCount c_itemsList
stackPower = Pipe ItemSize (Apply LeftShift 32)
isResEnoughItem = Pipe2 resCount stackPower GreaterThan
isResEnough = Pipe rndInput isResEnoughItem
isMatNotEnough = Negate (Pipe rndOutput isResEnoughItem)

c_emptyItem // Empty Item Card
emptyChoice = Apply (Flip Choice) c_emptyItem
exportItem = Pipe2 (Negate (Conjunction isResEnough isMatNotEnough)) rndInput emptyChoice