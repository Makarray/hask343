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

-----------------------------Still working on the below

--Applies the operators to 2 numbers and returns the result
--applyOperator :: String -> String -> String -> Int
--applyOperator x y "+" = (read x :: Int) + (read y :: Num)
--applyOperator x y op = 0
