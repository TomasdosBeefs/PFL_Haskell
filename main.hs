module Stack (Stack, stack2Str,createEmptyStack, isEmpty, push) where

import Data.List (intercalate)
import qualified Data.Map as Map

data StackValue = I Integer | B Bool  deriving Eq
newtype Stack = St [StackValue]

tt :: StackValue
tt = B True

ff :: StackValue
ff = B False
instance Show StackValue where
  show (I i) = show i
  show (B b) = show b


class Stackable a where
  toStackValue :: a -> StackValue

instance Stackable Integer where
  toStackValue = I

instance Stackable Bool where
  toStackValue = B

instance Stackable StackValue where
  toStackValue = id



instance Show Stack where
  show (St []) = "Empty Stack"
  show (St xs) = "Stack " ++ show xs
-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show

type Code = [Inst]
type State = Map.Map String StackValue

-- STACK FUNCTIONS



createEmptyStack :: Stack
createEmptyStack = St []

stack2Str :: Stack -> String
stack2Str (St s) = intercalate "," . map show $ s

isEmpty :: Stack  -> Bool
isEmpty (St []) = True
isEmpty _ = False


push :: Stackable a => a -> Stack -> Stack
push x (St xs) = St (toStackValue x : xs)


fetchX :: String -> Stack -> State -> Stack
fetchX x (St xs) s = case Map.lookup x s of
  Just val -> push val (St xs)
  --Nothing -> St xs
  Nothing -> error "Run-time error: Variable not found"

storeX :: String -> Stack -> State -> (Stack,State)
storeX x (St (v:vs)) s = (St vs , Map.insert x v s)
storeX _ _ _ = error "Run-time error: Store with empty stack"

branch c1 c2 s st =
  case s of
    St (x : xs) | x == tt -> (c1, St xs, st)
                | x == ff -> (c2, St xs, st)
    _ -> error "Run-time error"

loop :: Code -> Code -> Code -> Stack -> State -> (Code, Stack, State)
loop c1 c2 rest stack state =(c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]] ++ rest, stack, state)


  -- error "Run-time error: Loop with non-boolean value on top of the stack"

--loop :: Code -> Code -> Stack -> State -> (Code, Stack, State)
--loop c1 c2 s st = case s of
--  St (B True : xs) -> (c1 ++ [Loop c1 c2], St xs, st)
--  St (B False : xs) -> (c2, St xs, st)
--  _ -> error "Run-time error"

-- PROCESSING FUNCTIONS

processInst :: Inst -> Stack -> State -> (Code,Stack, State)
processInst (Push x) s st = ([], push x s, st)
processInst Add s st = ([], add s, st)
processInst Mult s st = ([], mult s, st)
processInst Sub s st = ([], sub s, st)
processInst Tru s st = ([], push tt s, st)
processInst Fals s st = ([], push ff s, st)
processInst And s st = ([], andd s, st)
processInst Equ s st = ([], equ s, st)
processInst Le s st = ([], le s, st)
processInst Neg s st = ([], neg s, st)
processInst (Fetch x) s st = ([], fetchX x s st, st)
processInst (Store x) s st = ([], fst $ storeX x s st, snd $ storeX x s st)
processInst Noop s st = ([], s, st)
processInst (Branch c1 c2) s st = branch c1 c2 s st
--processInst (Loop c1 c2) s st = loop c1 c2 s st
processInst (Loop c1 c2) s st = (c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]], s, st)


-- STATE CODE

createEmptyState :: State
createEmptyState = Map.empty

state2Str :: State -> String
state2Str s = intercalate "," . map (\(x,y) -> x ++ "=" ++ show y) $ Map.toList s

createState :: Stackable a => [(String, a)] -> State
createState = Map.fromList . map (\(k, v) -> (k, toStackValue v))



-- ARITHMETIC FUNCTIONS

add :: Stack -> Stack
add (St (I x : I y : xs)) = St (I (x + y) : xs)
add _ = error "Run-time error"

mult :: Stack -> Stack
mult (St (I x : I y : xs)) = St (I (x * y) : xs)
mult _ = error "Run-time error"

sub :: Stack -> Stack
sub (St (I x : I y : xs)) = St (I (x - y) : xs)
sub _ = error "Run-time error"

neg :: Stack -> Stack
neg (St (I x : xs)) = St (I (-x) : xs)
neg (St (B x : xs)) = St (B (not x) : xs)

neg _ = error "Run-time error"

equ :: Stack -> Stack
equ (St (I x : I y : xs)) = St (B (x == y) : xs)
equ (St (B x : B y : xs)) = St (B (x == y) : xs)


equ _ = error "Run-time error"

le :: Stack -> Stack
le (St (I x : I y : xs)) = St (B (x <= y) : xs)
le _ = error "Run-time error"

andd :: Stack -> Stack
andd (St (B x : B y : xs)) = St (B (x && y) : xs)
andd _ = error "Run-time error"

-- RUN FUNCTION


--run :: (Code, Stack, State) -> (Code, Stack, State)
--run ([], s, st) = ([], s, st)
--run (i:is, s, st) = run (is', s', st')
 -- where
  --  (is', s', st') = processInst i s st

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], s, st) =  ([], s, st)
run (i:is, s, st) = do
  let (is', s', st') = processInst i s st
  --putStrLn $ "After instruction " ++ show i ++ ":"
  --putStrLn $ "Stack: " ++ show s'
  --putStrLn $ "State: " ++ show st'
  run (is' ++ is, s', st')

--run :: (Code, Stack, State) -> (Code, Stack, State)
--run (is, s, st) = foldl process (is, s, st) is
 -- where
  -- process (_, s, st) i = (is', s', st')
  --  where
   --    (is', s', st') = processInst i s st


testAssembler :: [Inst] -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (code, createEmptyStack, createEmptyState)

-- Examples:
 -- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- compA :: Aexp -> Code
--compA = undefined -- TODO

-- compB :: Bexp -> Code
--compB = undefined -- TODO

-- compile :: Program -> Code


-- parse :: String -> Program
--parse = undefined -- TODO

-- To help you test your parser
--testParser :: String -> (String, String)
--testParser programCode = (stack2Str stack, state2Str store)
 -- where (_,stack,store) = run (compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "x := 0 - 2;" == ("","x=-2")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")

data Aexp = Num Integer | Var String | AddP Aexp Aexp | SubP Aexp Aexp | MultP Aexp Aexp deriving Show
data Bexp = BP Bool | Eq Aexp Aexp | LeP Aexp Aexp | Not Bexp | AndP Bexp Bexp deriving Show
data Stm = Assign String Aexp | Skip | Comp Stm Stm | If Bexp Stm Stm | While Bexp Stm deriving Show

type Program = Stm

compile :: Program -> Code
compile = compStm

compA :: Aexp -> Code
compA (Num n) = [Push n]
compA (Var x) = [Fetch x]
compA (AddP a1 a2) = compA a1 ++ compA a2 ++ [Add]
compA (SubP a1 a2) = compA a1 ++ compA a2 ++ [Sub]
compA (MultP a1 a2) = compA a1 ++ compA a2 ++ [Mult]

compB :: Bexp -> Code
compB (BP b) = if b then [Tru] else [Fals]
compB (Eq a1 a2) = compA a1 ++ compA a2 ++ [Equ]
compB (LeP a1 a2) = compA a1 ++ compA a2 ++ [Le]
compB (Not b) = compB b ++ [Neg]
compB (AndP b1 b2) = compB b1 ++ compB b2 ++ [And]

compStm :: Stm -> Code
compStm (Assign x a) = compA a ++ [Store x]
compStm Skip = [Noop]
compStm (Comp s1 s2) = compStm s1 ++ compStm s2
compStm (If b s1 s2) = compB b ++ [Branch (compStm s1) (compStm s2)]
compStm (While b s) = [Loop (compB b) (compStm s)]

parse :: String -> [Stm]
parse = buildData.lexer

buildData :: [String] -> [Program]
buildData [] = []
buildData (x:xs) = parseStm (x:xs) : buildData (drop 1 (dropWhile (/= ";")  xs))


parseStm :: [String] -> Program
parseStm (x:y:xs)
    | isVariable x && head xs == ":=" = if y == Aexp then             else Assign x (parseAexp (drop 1 xs))
    | x == "skip" = Skip
    | x == "if" = If (parseBexp (drop 1 xs)) (parseStm (drop 1 (dropWhile (/= "then") xs))) (parseStm (drop 1 (dropWhile (/= "else") xs)))
    | x == "while" = While (parseBexp (drop 1 xs)) (parseStm (drop 1 (dropWhile (/= "do") xs)))
    | otherwise = error "Syntax error"



parseAexp :: [String] -> Aexp
parseAexp (x:y:xs)
    | isVariable x = Var x
    | isDigit (head x) = Num (read x :: Integer) 
    | x == "(" = parseAexp (takeWhile (/= ")") xs)
    | x == "-" = SubP (Num 0) (parseAexp xs)
    | x == "+" = AddP (Num 0) (parseAexp xs)
    | x == "*" = MultP (Num 1) (parseAexp xs)
    | otherwise = error "Syntax error"

parseBexp :: [String] -> Bexp
parseBexp (x:xs)
    | x == "true" = BP True
    | x == "false" = BP False
    | x == "not" = Not (parseBexp xs)
    | x == "(" = parseBexp (takeWhile (/= ")") xs)
    | x == "¬" = Not (parseBexp xs)
    | x == "and" = AndP (parseBexp xs) (parseBexp (drop 1 (dropWhile (/= "and") xs)))
    | x == "=" = Eq (parseAexp xs) (parseAexp (drop 1 (dropWhile (/= "=") xs)))
    | x == "<=" = LeP (parseAexp xs) (parseAexp (drop 1 (dropWhile (/= "<=") xs)))
    | otherwise = error "Syntax error"


-- LEXER

isSpace :: Char -> Bool
isSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

isAlpha :: Char -> Bool
isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

isOperator :: Char -> Bool
isOperator c = elem c "+-*/=;<>^:¬"


lexer :: String -> [String]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | isDigit c = let (num, rest) = span isDigit (c:cs) in num : lexer rest
    | isAlpha c = let (letter, rest) = span isAlpha (c:cs) in letter : lexer rest
    | isOperator c = let (op, rest) = span isOperator (c:cs) in op : lexer rest
    | c == '(' = ['('] : lexer cs
    | c == ')' = [')'] : lexer cs
    | otherwise = let (other, rest) = span (not . isSpace) (c:cs) in other : lexer rest

isKeyword :: String -> Bool
isKeyword str = elem str ["while", "if", "then", "else", "do", "not"]


isVariable :: String -> Bool
isVariable (c:cs) = isAlpha c && all (\x -> isAlpha x || isDigit x) cs
isVariable _ = False

-- Examples:
-- lexer "x := 5; x := x - 1;" == ["x",":=","5",";","x",":=","x","-","1",";"]
-- lexer "x := 0 - 2;" == ["x",":=","0","-","2",";"]
main :: IO ()  5 + (9*4) - 3 * (4+4)
main = do
    let c = "x := 5"
    let (x:stt) = lexer c

    let b = isVariable x && head stt == ":="
    let sbb = drop 1 stt
    print sbb
    let d = Assign x (parseAexp (drop 1 stt))

    print stt


