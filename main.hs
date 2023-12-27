import Data.List (intercalate)
import qualified Data.Map as Map
-- Part One

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
data StackElement = Integer Int | Boolean Bool deriving Show

type Code = [Inst]
type Stack =  [StackElement]
type Storage = Map.Map String Integer
type State = (Stack, Code, Storage)

tt :: Int
tt = 1 -- TRUE

ff :: Int
ff = 0 -- FALSE

-- STACK CODE

createEmptyStack :: Stack
createEmptyStack =  []

stack2Str :: Stack -> String
stack2Str = intercalate "," . map show

-- STACK FUNCTIONS

push :: StackElement -> Stack -> Stack
push x xs = x : xs

fetchX :: String -> Storage -> Integer
fetchX x s = case Map.lookup x s of
  Just n -> n
  Nothing -> error "Variable not found"

storeX :: String -> Integer -> Storage -> Storage
storeX = Map.insert



-- ARITHMETIC FUNCTIONS

add :: Stack -> Maybe Stack
add (Integer x:Integer y:xs) = Just $ Integer (x+y) : xs
add _ = error "Run-time error"

mult :: Stack -> Maybe Stack
mult (Integer x:Integer y:xs) = Just $ Integer (x*y) : xs
mult _ = error "Run-time error"

sub :: Stack -> Maybe Stack
sub (Integer x:Integer y:xs) = Just $ Integer (y-x) : xs
sub _ = error "Run-time error"

eq :: Stack -> Maybe Stack
eq (Integer x:Integer y:xs) = Just $ Boolean (x==y) : xs
eq (Boolean x:Boolean y:xs) = Just $ Boolean (x==y) : xs
eq _ = error "Run-time error"

le :: Stack -> Maybe Stack
le (Integer x:Integer y:xs) = Just $ Boolean (y<=x) : xs
le _ = error "Run-time error"

neg :: Stack -> Maybe Stack
neg (Boolean x:xs) = Just $ Boolean (not x) : xs
neg _ = error "Run-time error"



-- STATE CODE

createEmptyState :: State
createEmptyState = ([],[],Map.empty)

state2Str :: State -> String
state2Str (st, s) = show st ++ "\n" ++ show s






run :: (Code, Stack, State) -> (Code, Stack, State)
run = undefined -- TODO

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
testParser programCode = (stack2Str stack, store2Str store)
  where (_,stack,store) = run (compile (parse programCode), createEmptyStack, createEmptyStore)

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