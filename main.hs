module Stack (Stack, stack2Str,createEmptyStack, isEmpty, push) where 

import Data.List (intercalate)
import qualified Data.Map as Map
import Main (StackElement(Integer))

data StackValue = I Integer | B Bool 
newtype Stack = St [StackValue] deriving Show

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

fetch :: String -> Stack -> State -> Stack
fetch x (St xs) s = case Map.lookup x s of
  Just val -> push val (St xs)
  Nothing -> St xs
-- STATE CODE

createEmptyState :: State
createEmptyState = Map.empty

state2Str :: State -> String
state2Str s = intercalate "," . map (\(x,y) -> x ++ "=" ++ show y) $ Map.toList s





-- ARITHMETIC FUNCTIONS

branch :: Code -> Code -> Code -> Stack -> State -> (Code, Stack, State)
branch c1 c2 rest (top:restStack) state =
  case top of
    True -> run (c1 ++ rest, restStack, state)
    False -> run (c2 ++ rest, restStack, state)
    _ -> error "Run-time error: Branch with non-boolean value on top of the stack"

loop :: Code -> Code -> Code -> Stack -> State -> (Code, Stack, State)
loop c1 c2 rest (top:restStack) state =
  case top of
    Boolean True -> run (c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]] ++ rest, restStack, state)
    Boolean False -> run (rest, restStack, state)
    _ -> error "Run-time error: Loop with non-boolean value on top of the stack"



run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)  -- End of execution
run (inst:rest, stack, state) =
  case inst of
    Add -> case add stack of
      Just newStack -> run (rest, newStack, state)
      Nothing -> error "Run-time error: Addition failed"

    Mult -> case mult stack of
      Just newStack -> run (rest, newStack, state)
      Nothing -> error "Run-time error: Multiplication failed"

    Sub -> case sub stack of
      Just newStack -> run (rest, newStack, state)
      Nothing -> error "Run-time error: Subtraction failed"

    Tru -> run (rest, push True stack, state)
    Fals -> run (rest, push False stack, state)

    Equ -> case eq stack of
      Just newStack -> run (rest, newStack, state)
      Nothing -> error "Run-time error: Equality comparison failed"

    Le -> case le stack of
      Just newStack -> run (rest, newStack, state)
      Nothing -> error "Run-time error: Less than or equal comparison failed"

    And -> case neg stack of
      Just newStack -> run (rest, newStack, state)
      Nothing -> error "Run-time error: Logical AND failed"

    Push n -> run (rest, push n stack, state)

    Fetch x -> run (rest, fetch x stack, state)

    Branch c1 c2 -> branch c1 c2 rest stack state

    Loop c1 c2 -> loop c1 c2 rest stack state

    Noop -> run (rest, stack, state)

    _ -> error "Run-time error: Unhandled instruction"


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