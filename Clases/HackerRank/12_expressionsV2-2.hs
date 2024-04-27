module Expressions where

data Exp = ExpTerm Term ExpOp Exp | ExpTermOnly Term

data ExpOp = Plus | Minus deriving Eq

data Term = TermFactor Factor TermOp Term | TermFactorOnly Factor

data TermOp = Times | Divided deriving Eq

data Factor = Number Int
            | UnaryPlus Factor
            | UnaryMinus Factor
            | Bracket Exp

evalExp :: Exp -> Int
evalExp (ExpTermOnly term) = evalTerm term
evalExp (ExpTerm term op exp) =
    case op of
        Plus -> (evalExp (ExpTermOnly term) + evalExp exp) `mod` 1000000007
        Minus -> (evalExp (ExpTermOnly term) - evalExp exp) `mod` 1000000007

modInverse :: Int -> Int -> Int
modInverse a m = let (_, b, _) = extendedGCD a m in (b + m) `mod` m

extendedGCD :: Int -> Int -> (Int, Int, Int)
extendedGCD a 0 = (a, 1, 0)
extendedGCD a b =
    let (g, x, y) = extendedGCD b (a `mod` b)
    in (g, y, x - (a `div` b) * y)

evalTerm :: Term -> Int
evalTerm (TermFactorOnly factor) = evalFactor factor
evalTerm (TermFactor factor op term) =
    case op of
        Times -> (evalTerm (TermFactorOnly factor) * evalTerm term) `mod` 1000000007
        Divided -> let termEval = evalTerm term
                       factorEval = evalFactor factor
                   in (factorEval * modInverse termEval 1000000007) `mod` 1000000007


evalFactor :: Factor -> Int
evalFactor (Number n) = n
evalFactor (UnaryPlus factor) = evalFactor factor
evalFactor (UnaryMinus factor) = (- evalFactor factor) `mod` 1000000007
evalFactor (Bracket exp) = evalExp exp

apply :: Term -> TermOp -> ExpOp
apply _ Times = Plus
apply _ Divided = Minus

combine :: Term -> Term -> ExpOp
combine _ _ = Plus

parseExp :: String -> Exp
parseExp = fst . parseExpression

parseExpression :: String -> (Exp, String)
parseExpression str =
    let (term, rest1) = parseTerm str
    in parseExpRest (ExpTermOnly term) rest1

parseExpRest :: Exp -> String -> (Exp, String)
parseExpRest term ('+' : rest) =
    let (term2, rest2) = parseTerm rest
    in parseExpRest (ExpTerm term Plus (ExpTermOnly term2)) rest2
parseExpRest term ('-' : rest) =
    let (term2, rest2) = parseTerm rest
    in parseExpRest (ExpTerm term Minus (ExpTermOnly term2)) rest2
parseExpRest term xs = (term, xs)


parseTerm :: String -> (Term, String)
parseTerm str =
    let (factor, rest1) = parseFactor str
    in parseTermRest (TermFactorOnly factor) rest1

parseTermRest :: Term -> String -> (Term, String)
parseTermRest factor "" = (factor, "")
parseTermRest factor ('*' : rest) =
    let (factor2, rest2) = parseFactor rest
    in parseTermRest (TermFactor factor Times (TermFactorOnly factor2)) rest2
parseTermRest factor ('/' : rest) =
    let (factor2, rest2) = parseFactor rest
    in parseTermRest (TermFactor factor Divided (TermFactorOnly factor2)) rest2
parseTermRest factor xs = (factor, xs)

parseFactor :: String -> (Factor, String)
parseFactor ('(' : rest) =
    let (exp, ')':rest'') = parseExpression rest
    in (Bracket exp, rest'')
parseFactor ('-' : rest) =
    let (factor, rest') = parseFactor rest
    in (UnaryMinus factor, rest')
parseFactor ('+' : rest) =
    let (factor, rest') = parseFactor rest
    in (UnaryPlus factor, rest')
parseFactor (c : rest) = (Number (read (takeWhile (elem ['0'..'9']) (c:rest))), dropWhile (elem ['0'..'9']) rest)

-- Función principal
solveExpression :: String -> Int
solveExpression str = evalExp (parseExp str) `mod` 1000000007