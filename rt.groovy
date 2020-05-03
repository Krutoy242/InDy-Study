
// Tempelate:
name=([a,b]={ operatorBody(A,B) }) => resultActions(a,b)

F(a,b) = ___
G(a,b) = ___

=([a,b,c]={ Pipe(F,G) }) =>
	Apply(G, Apply(F, a), b, c)
	G(Apply(F, a), b, c)
	G(F(a), b, c)

appFirst[a,b]:
	= G(a, F(a,b))
	= Apply3(Pipe, Apply2(F,a), Apply2(G,a), b)
	= Apply2(G, a, Apply2(F,a,b))
	= Apply(Ga, Apply(Fa, b))
	<= Pipe2(Apply2(F), Apply2(G), Pipe)

[a,b]:
H(I(b), a(F(b)), a(G(b)))
  = H(I(b), Apply(a, F(b)), Apply(a, G(b)))
	= Apply3(H, I(b), Apply(a, F(b)), Apply(a, G(b)))
	= Apply3(H, I(b), F`(a,b), G`(a,b)) 
		F`(a,b) = Apply(a, F(b))
		G`(a,b) = Apply(a, G(b))
	= Apply2(Apply(H, I(b)), F`(a,b), G`(a,b))
	= Apply2(HI`(b), F`(a,b), G`(a,b))
		HI`(b) = Apply(H, I(b))
	= Apply(Apply(HI`(b), F`(a,b)), G`(a,b))
	= Apply(HIF(a,b), G`(a,b)) 
		HIF(a,b) = Apply(HI`(b), F`(a,b))
	<= Pipe2(G`, HIF, Apply(Pipe2, id))
	<= Pipe2(G`, Pipe2(Apply2(F`), Flip(Apply2(HI`)), Pipe), Apply(Pipe2, id))
	// <= Pipe2(Pipe*(G), Pipe2(Apply2(Pipe*(F)), Flip(Apply2(HI`)), Pipe), Apply(Pipe2, id))
	// <= Pipe2(Pipe*(G), Pipe2(Apply2(Pipe*(F)), Flip(Apply2(Pipe(I, H))), Pipe), Apply(Pipe2, id))
	// <= Pipe2(Apply(Flip(Pipe), G), Pipe2(Apply2(Apply(Flip(Pipe), F)), Flip(Apply2(Pipe(I, H))), Pipe), Apply(Pipe2, id))


H[a,b]: a(F(b))
	= Apply(a, Apply(F, b))
	<= Apply(Pipe(F))


H[a,b]: G(a, b, F(a,b))
	[b]   <= Pipe2(id, F(a), G(a))
	[a,b] <= Pipe2(F, G, Apply(Pipe2, id))


Pipe3[F,G,H,I]:
	= Apply3(I, F(a), G(a), H(a))

h a = f(g(a))


// -------------------------------------------------------------------------------------
// Useful operators
// -------------------------------------------------------------------------------------
H[a]	: G(x, x) 		  <= Pipe2(id, id, G)
H[a]  : G(a, F(a))    <= Pipe2(id, F, G)
H[a,b]: G(b,a)        <= Flip(Apply2(G))
H[a,b]: G(F(a,b))     <= Apply(Flip(Pipe(F, Pipe)), G)
H[a,b]: G(a, F(a,b))  <= Pipe2(Apply2(F), Apply2(G), Pipe)
H[a,b]: G(a, F(b,a))  <= Pipe2(Flip(Apply2(F)), Apply2(G), Pipe)
H[a,b]: G(b, F(a,b))  <= Pipe2(Apply2(F), Flip(Apply2(G)), Pipe)
H[a,b]: G(b, F(b,a))  <= Pipe2(Flip(Apply2(F)), Flip(Apply2(G)), Pipe)
H[a,b]: G(a,b,F(a,b)) <= Pipe2(F, G, Apply(Pipe2, id))


// -------------------------------------------------------------------------------------
// Standart functions
// -------------------------------------------------------------------------------------
Apply(F,a)     = F(a)
Apply*(a,F)    = F(a)
Apply2(F,a,b)  = F(a,b)
Apply2*(a,F,b) = F(a,b)
Pipe(F1,F2,c)  = Apply(F2, Apply(F1, c))
Pipe(F, G)     = Apply(G, F(a))
Pipe2(F, H, G) = Apply2(G, F(a), H(a))

// -------------------------------------------------------------------------------------
// Parsing
// -------------------------------------------------------------------------------------

// Example full name of operator
// "operator.operators.integrateddynamics.parse.valuetype.valuetypes.integrateddynamics.integer.name"

// Test string for stage 1
"operator.pipe parse.valuetype.valuetypes.integrateddynamics.integer integer.increment"

// Test string for Stage 2
"A* 5"
"Flip A 5"

// Another test line
"integer.modulus* 5 . integer.increment"
"Apply*5 integer.modulus* . integer.increment"
"Flip integer.modulus 5 . integer.increment"
"Pipe Flip integer.modulus 5 integer.increment"
"Pipe Flip integer.modulus 5 integer.increment"

// -------------------------------------------------------------------------------------
// Working parsing
// -------------------------------------------------------------------------------------


// String wrap for ridicuilous operator name
wrap = "operator.operators.integrateddynamics.@@@.name"
wrapReplace = "@@@"

// Makes an operator from string
parseSimple(str)
	= OpByName(Replace(wrapReplace, str, wrap))
	<= Pipe(Apply(Flip(Apply(Replace, wrapReplace)), wrap), OpByName)

// Simple reduce operator appling [n-1] element to [n] of list
opReduce(op,s)::op
	= op(parseSimple(s))
	<= Apply(Pipe(parseSimple))

// Parse list of operators
parseList(list)::op
	= Reduce(opReduce, Tail(list), parseSimple(Head(list)))
	<= Pipe2(Tail, Pipe(Head, parseSimple), Apply(Reduce, opReduce))

// -------------------------------------------------------------------------------------

// Get List of commands from string
words(str)::[str]
  = Split_on(" ", str) 
	<= Apply(Split_on, " ")

// Parse whole code
parse(str)::op
  <= Pipe(words, parseList)

// -------------------------------------------------------------------------------------
"pipe (flip pipe) (flip pipe) pipe pipe (+1) (*2) (+3)"

opReduce :: (a->b) -> String -> (a->c)
opReduce op s = op(parseRcrsv(s))

parseRcrsv str = choice (isCompound str) (parse str) (parseSimple str)

parse str = pipe removeCatch(str) parseRcrsv


// -------------------------------------------------------------------------------------
// Outdated parsing operators
// -------------------------------------------------------------------------------------

// Get integer from string like "#5"
// getInt("#5") >> 5 : int
// getInt        =([str]={ }) => Pipe(getRgxInt ,op_parse_integer)

// prefix  = "operator.operators.integrateddynamics."
// postfix = ".name"
// matchOpName   = Apply2(regex_group, "[\w\.]+", 0)
// matchInt      = Apply2(regex_group, "\d+", 0)
// parseFlip         = ([]={ Flip(parseSimple) }) => 
// parseSimpleInt    = ([str]={ }) => Apply(parseSimple(matchOpName(str)), getInt(matchInt(str)))
// parseFlipInt      = ([str]={ }) => Apply(parseFlip(matchOpName(str)), getInt(matchInt(str)))
// listParsers   = List(parseSimple, parseFlip, parseSimpleInt, parseFlipInt)

// Regex checkers for operator
// mSimple       =([str]={ }) => Apply(matches_regex, "[\w\.]+")
// mFlip         =([str]={ }) => Apply(matches_regex, "[\w\.]+\*")
// mSimpleInt    =([str]={ }) => Apply(matches_regex, "[\w\.]+#\d+")
// mFlipInt      =([str]={ }) => Apply(matches_regex, "[\w\.]+\*#\d+")

// selectParser  =([str]={ }) => Choice(mFlipInt(str), 3, Choice(mSimpleInt(str), 2, Choice(mFlip(str), 1, Choice(mSimple(str), 0, ))))

// rgxList       = List("[\w\.]+", "[\w\.]+\*", "[\w\.]+#\d+", "[\w\.]+\*#\d+")
// filterRgxMatch= ([str, n]={ }) => Apply2(matches_regex, Get(rgxList, n), str)
// enum0_3       = List(0, 1, 2, 3)
// selectParser  = ([str]={ }) => Get(filter(enum0_3, Apply(filterRgxMatch, str)), 0)

// Complicated Get operator from string
// parseWord(str)::any
// 	= Get(listParsers, selectParser(str))

// Complicated Reduce based on presence of integers
// opReduce(op,s)::op
// 	= Choose(G(s), Apply(op, parseWord(s)), Apply(op, getInt(s)))
// 	= Choose(G(s), op(B(s)), op(C(s)))
// 	= Apply3(Choose, G(s), Apply(op, Apply(B,s)), Apply(op, Apply(C, s)))
// 	= Apply3(Choose, G(s), B`(op,s), C`(op,s))
// 	= Apply2(Apply(Choose, G(s)), B`(op,s), C`(op,s))
// 	= Apply2(G`(s), B`(op,s), C`(op,s))
// 	= G`(s, B`(op,s), C`(op,s))
// 	= G`(s, BC(op,s))
// 	  	BC[op,s] = B`(op,s), C`(op,s)
// 	<= Pipe2(Apply2(BC), Flip(Apply2(G`)), Pipe)

// opReduce(s1,s2)::op(op,str) = ; Pipe2(parseWord(s1), parseWord(s2), Apply)
// 		(a,b) =
// 			F(a, F(b))
// 			Apply2(F, a, F(b))
// 			Apply(PipeF(F(a)), b)
// 			(b) => Pipe(F, F(a))
// 			(b) => Apply2(Pipe, F, F(a))
// 			(b) => Apply(PipeF, F(a))
// 			(b) => PipeF(F(a))
// 			(b,a) => Pipe(F, PipeF)

// 			(b,a){ Pipe(F, Apply(Pipe, F)) } => F(a, F(b))

// G(1,2) = Pipe(F, PipeF) =>
// 		Apply(Pipe, F)(F(1), 2)
// 		PipeF(F(1), 2)
// 		Pipe(F, F(1))(2)
// 		Pipe(F(2), F(1))

