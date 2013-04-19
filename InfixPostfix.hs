-------------------------------------
--A Haskell implementation of Project #3
--
--This Program defines functions that can
--take an infix expression and convert it
--to a postfix expression. It also defines
--functions to evaluate postfix expressions.
--
--Due Date: April 19, 2013
--Authors: Matt Shrider and Steven Hoffman
--------------------------------------

--Converts the infix String to postfix and then evaluates it
evaluateInfix :: String -> Int
evaluateInfix infx = evaluatePostfix (infixToPostfix infx)

--Returns the postfix representation of an infix expression
infixToPostfix :: String -> String
infixToPostfix [] = ""
infixToPostfix infx = do	
	let tokens = words infx --split infx by whitespace
	if (length tokens == 0) then "" --if no tokens, end
	else tail (iToPHelper (head tokens) (tail tokens) [] "")

--Recursive helper method for infixToPostfix
--Prameters: current token, remaining tokens, stack, current postfix expr
--Return: postfix expression
iToPHelper :: String -> [String] -> [String] -> String -> String
iToPHelper token [] stack postfix = do
	if (operand token)
		then appendStack (postfix ++ " " ++ token) stack
	else if (rightParen token) 
		then  foundRightParen [] stack postfix
	else " Error: the input string was not infix. It ended in " ++ token
iToPHelper token tokens stack postfix = do
	if (operand token)
		then iToPHelper (head tokens) (tail tokens) stack (postfix ++ " " ++ token)
	else if (leftParen token)
		then iToPHelper (head tokens) (tail tokens) (token:stack) postfix
	else if (operator token)
		then popOperators token tokens stack postfix
	else if (rightParen token)
		then foundRightParen tokens stack postfix
	else " Error: the input string was not infix. It contained " ++ token

--Evaluates the postfix string and returns the result
evaluatePostfix :: String -> Int
evaluatePostfix [] = 0
evaluatePostfix postfx = do
	let tokens = words postfx --split postfx by whitespace
	if (length tokens == 0) then 0 --if no tokens, end
	else read (ePHelper (head tokens) (tail tokens) [])

--Recursive helper method for evaluatePostfix
--Prameters: current token, remaining tokens, stack
--Return: the evaluation of the postfix expression represented by tokens
ePHelper :: String -> [String] -> [String] -> String
ePHelper token [] [] = "0"
ePHelper token [] stack = do
	let y = (head stack)
	let x = (head (tail stack))
	applyOperator x y token
ePHelper token tokens stack = do
	if (operand token)
		then ePHelper (head tokens) (tail tokens) (token:stack)
	else if (operator token)
		then do
			let y = (head stack)
		     	let x = (head (tail stack))
                   	let z = applyOperator x y token
			let sk = tail (tail stack)
		     	ePHelper (head tokens) (tail tokens) (z:sk)
	else "0"

--Recursive helper method for iToPHelper.
--Pops operators from the stack as needed and adds the passed operator onto stack
popOperators :: String -> [String] -> [String] -> String -> String
popOperators op tokens [] postfix = 
	iToPHelper (head tokens) (tail tokens) (op:[]) postfix
popOperators op tokens stack@(x:xs) postfix = do
	if (stackPrecedence x >= inputPrecedence op)
		then popOperators op tokens xs (postfix ++ " " ++ x)
	else iToPHelper (head tokens) (tail tokens) (op:stack) postfix

--Recursive helper method for iToPHelper.
--Pops from stack and appends to postfix
--until a left parenthesis is found.
foundRightParen :: [String] -> [String] -> String -> String
foundRightParen _ [] _ = " Error: No left paren to match all right parens."
foundRightParen [] (x:xs) postfix = if (leftParen x)
	then appendStack postfix xs
	else foundRightParen [] xs (postfix ++ " " ++ x)
foundRightParen tokens (x:xs) postfix = if (leftParen x)
	then iToPHelper (head tokens) (tail tokens) xs postfix
	else foundRightParen tokens xs (postfix ++ " " ++ x)

--Recursive helper method for iToPHelper. Adds items from stack to end of string
appendStack :: String -> [String] -> String
appendStack postfix [] = postfix
appendStack postfix (x:xs) = postfix ++ " " ++ x ++ (appendStack "" xs)

--Returns true if the input is an operator and false otherwise
operator :: String -> Bool
operator [] = False
operator "+" = True
operator "-" = True
operator "*" = True
operator "/" = True
operator "%" = True
operator "^" = True
operator x = False

--Returns ture if the input is an operand (integer) and false otherwise
operand :: String -> Bool
operand [] = False
operand all@(x:xs) = if (x == '-' && length all > 1)
			then ((sum [1 | a <- xs, a `elem` ['0'..'9']]) 
				== (length xs))
			else ((sum [1 | a <- all, a `elem` ['0'..'9']])
				== (length all))

--Returns true if the input is a left paren and false otherwise
leftParen :: String -> Bool
leftParen "(" = True
leftParen x = False


--Returns true if the input is a right paren and false otherwise
rightParen :: String -> Bool
rightParen ")" = True
rightParen x = False

--Returns the stack precedence of the input operator
stackPrecedence :: (Num a) => String -> a
stackPrecedence "+" = 1
stackPrecedence "-" = 1
stackPrecedence "*" = 2
stackPrecedence "/" = 2
stackPrecedence "%" = 2
stackPrecedence "^" = 3
stackPrecedence x = -1

--Returns the input precedence of the input operator
inputPrecedence :: (Num a) => String -> a
inputPrecedence "+" = 1
inputPrecedence "-" = 1
inputPrecedence "*" = 2
inputPrecedence "/" = 2
inputPrecedence "%" = 2
inputPrecedence "^" = 4
inputPrecedence x = 5

--Applies the operators to 2 numbers and returns the result, cast as a string
applyOperator :: String -> String -> String -> String
applyOperator x y "+" = show((read x)   + 	(read y))
applyOperator x y "-" = show((read x)   - 	(read y))
applyOperator x y "*" = show((read x)   * 	(read y))
applyOperator x y "/" = show((read x) `div` 	(read y))
applyOperator x y "%" = show((read x) `mod` 	(read y))
applyOperator x y "^" = show((read x)   ^ 	(read y))
applyOperator x y op = show(-1)
