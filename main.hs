module Stack (Stack, stack2Str,createEmptyStack, isEmpty, push) where

import Data.List (intercalate)
import qualified Data.Map as Map

data StackValue = I Integer | B Bool | TT | FF
newtype Stack = St [StackValue]

instance Show StackValue where
  show (I i) = show i
  show (B b) = show b
  show TT  =  "tt"
  show FF  =  "ff"

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
  Nothing -> St xs

storeX :: String -> Stack -> State -> (Stack,State)
storeX x (St (v:vs)) s = (St vs , Map.insert x v s)

branch :: Code -> Code -> Stack -> State -> (Code, Stack, State)
branch c1 c2 s st =
  case s of
  St (TT : xs) -> (c1, St xs, st)
  St (FF : xs) -> (c2, St xs, st)
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
processInst Tru s st = ([], push TT s, st)
processInst Fals s st = ([], push FF s, st)
processInst Equ s st = ([], equ s, st)
processInst Le s st = ([], le s, st)
processInst Neg s st = ([], neg s, st)
processInst (Fetch x) s st = ([], fetchX x s st, st)
processInst (Store x) s st = ([], fst $ storeX x s st, snd $ storeX x s st)
processInst Noop s st = ([], s, st)
processInst (Branch c1 c2) s st = branch c1 c2 s st
--processInst (Loop c1 c2) s st = loop c1 c2 s st
--processInst (Loop c1 c2) s st = (c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]] ++ rest, s, st)
  --where
    --(rest, s, st) = processInst (Loop c1 c2) s st

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
neg _ = error "Run-time error"

equ :: Stack -> Stack
equ (St (I x : I y : xs)) = St (B (x == y) : xs)
equ _ = error "Run-time error"

le :: Stack -> Stack
le (St (I x : I y : xs)) = St (B (x <= y) : xs)
le _ = error "Run-time error"

and :: Stack -> Stack
and (St (B x : B y : xs)) = St (B (x && y) : xs)
and _ = error "Run-time error"

-- RUN FUNCTION


--run :: (Code, Stack, State) -> (Code, Stack, State)
--run ([], s, st) = ([], s, st)
--run (i:is, s, st) = run (is', s', st')
 -- where
  --  (is', s', st') = processInst i s st

run :: (Code, Stack, State) -> (Code, Stack, State)
run (is, s, st) = foldl process (is, s, st) is
  where
   process (_, s, st) i = (is', s', st')
    where
       (is', s', st') = processInst i s st

-- To help you test your assembler
testAssembler :: Code -> (String, String)
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
compA = undefined -- TODO

-- compB :: Bexp -> Code
compB = undefined -- TODO

-- compile :: Program -> Code
compile = undefined -- TODO

-- parse :: String -> Program
parse = undefined -- TODO

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str store)
  where (_,stack,store) = run (compile (parse programCode), createEmptyStack, createEmptyState)

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


myIsSpace :: Char -> Bool
myIsSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

myIsDigit :: Char -> Bool
myIsDigit c = c >= '0' && c <= '9'

myIsAlpha :: Char -> Bool
myIsAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

myIsOperator :: Char -> Bool
myIsOperator c = elem c "+-*/=;<>^:¬"


lexer :: String -> [String]
lexer [] = []
lexer (c:cs)
    | myIsSpace c = lexer cs
    | myIsDigit c = let (num, rest) = span myIsDigit (c:cs) in num : lexer rest
    | myIsAlpha c = let (letter, rest) = span myIsAlpha (c:cs) in letter : lexer rest
    | myIsOperator c = let (op, rest) = span myIsOperator (c:cs) in op : lexer rest
    | c == '(' = ['('] : lexer cs
    | c == ')' = [')'] : lexer cs
    | otherwise = let (other, rest) = span (not . myIsSpace) (c:cs) in other : lexer rest


main :: IO ()
main = do
    let input = "y := 1; while ¬(x = 1) do (y := y * x; x := x - 1)"
    let result = lexer input
    putStrLn $ "Lexer Result: " ++ show result