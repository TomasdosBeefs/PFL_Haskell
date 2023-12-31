
module Stack (Stack, stack2Str,createEmptyStack, isEmpty, push) where

import Data.List (intercalate)
import qualified Data.Map as Map

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Expr (buildExpressionParser, Operator (..))
import Text.Parsec.Expr (Assoc(AssocLeft))
import GHC.RTS.Flags (DebugFlags())


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
type Statte = Map.Map String StackValue

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


fetchX :: String -> Stack -> Statte -> Stack
fetchX x (St xs) s = case Map.lookup x s of
  Just val -> push val (St xs)
  --Nothing -> St xs
  Nothing -> error "Run-time error: Variable not found"

storeX :: String -> Stack -> Statte -> (Stack,Statte)
storeX x (St (v:vs)) s = (St vs , Map.insert x v s)
storeX _ _ _ = error "Run-time error: Store with empty stack"

branch c1 c2 s st =
  case s of
    St (x : xs) | x == tt -> (c1, St xs, st)
                | x == ff -> (c2, St xs, st)
    _ -> error "Run-time error"

loop :: Code -> Code -> Stack -> Statte -> (Code, Stack, Statte)
loop c1 c2 s st =  (c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]], s, st)
  
  -- error "Run-time error: Loop with non-boolean value on top of the stack"


  -- error "Run-time error: Loop with non-boolean value on top of the stack"

--loop :: Code -> Code -> Stack -> Statte -> (Code, Stack, Statte)
--loop c1 c2 s st = case s of
--  St (B True : xs) -> (c1 ++ [Loop c1 c2], St xs, st)
--  St (B False : xs) -> (c2, St xs, st)
--  _ -> error "Run-time error"

-- PROCESSING FUNCTIONS

processInst :: Inst -> Stack -> Statte -> (Code,Stack, Statte)
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

processInst (Loop c1 c2) s st = loop c1 c2 s st
 

createEmptyState :: Statte
createEmptyState = Map.empty

state2Str :: Statte -> String
state2Str s = intercalate "," . map (\(x,y) -> x ++ "=" ++ show y) $ Map.toList s

createState :: Stackable a => [(String, a)] -> Statte
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
le (St (I x : I y : xs)) = St (B (y <= x) : xs)
le _ = error "Run-time error"

andd :: Stack -> Stack
andd (St (B x : B y : xs)) = St (B (x && y) : xs)
andd _ = error "Run-time error"

-- RUN FUNCTION


--run :: (Code, Stack, Statte) -> (Code, Stack, Statte)
--run ([], s, st) = ([], s, st)
--run (i:is, s, st) = run (is', s', st')
 -- where
  --  (is', s', st') = processInst i s st

run :: (Code, Stack, Statte) -> (Code, Stack, Statte)
run ([], s, st) =  ([], s, st)
run (i:is, s, st) = do
  let (is', s', st') = processInst i s st
  --putStrLn $ "After instruction " ++ show i ++ ":"
  --putStrLn $ "Stack: " ++ show s'
  --putStrLn $ "Statte: " ++ show st'
  run (is' ++ is, s', st')

--run :: (Code, Stack, Statte) -> (Code, Stack, Statte)
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

 --To help you test your parser
--testParser :: String -> (String, String)
--testParser programCode = (stack2Str stack, State2Str store)
 --where (_,stack,store) = run (compile (parse programCode), createEmptyStack, createEmptyState)

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


-- Arithmetic expressions
data Aexp
  = Num Integer                  -- for numbers
  | Var String               -- for variables
  | AddP Aexp Aexp            -- for addition
  | SubP Aexp Aexp            -- for subtraction
  | MultP Aexp Aexp            -- for multiplication
  deriving (Show)

-- Boolean expressions
data Bexp
  = BP Bool                       -- for false
  | Not Bexp                 -- for negation
  | AndP Bexp Bexp            -- for logical and            -- for logical or
  | EqA Aexp Aexp            -- for equality comparison of Aexp
  | LeA Aexp Aexp             -- for less than or equal to comparison of Aexp
  | EqB Bexp Bexp            -- for equality comparison of Bexp
  deriving (Show)


-- Statements
data Stm
  = Skip                     -- for doing nothing
  | Assign String Aexp       -- for variable assignment
  | Seq [Stm]            -- for sequencing of Statements
  | If Bexp Stm Stm         -- for if-then-else Statements
  | While Bexp Stm
                      -- for while loops
  deriving (Show)

type Program = [Stm]

compile :: Program -> Code
compile = concatMap compStm

compA :: Aexp -> Code
compA (Num n) = [Push n]
compA (Var x) = [Fetch x]
compA (AddP a1 a2) = compA a1 ++ compA a2 ++ [Add]
compA (SubP a1 a2) = compA a1 ++ compA a2 ++ [Sub]
compA (MultP a1 a2) = compA a1 ++ compA a2 ++ [Mult]

compB :: Bexp -> Code
compB (BP b) = if b then [Tru] else [Fals]
compB (EqA a1 a2) = compA a1 ++ compA a2 ++ [Equ]
compB (LeA a1 a2) = compA a1 ++ compA a2 ++ [Le]
compB (Not b) = compB b ++ [Neg]
compB (AndP b1 b2) = compB b1 ++ compB b2 ++ [And]

compStm :: Stm -> Code
compStm (Assign x a) = compA a ++ [Store x]
compStm Skip = [Noop]

compStm (If b s1 s2) = compB b ++ [Branch (compStm s1) (compStm s2)]
compStm (While b s) = [Loop (compB b) (compStm s)]

compStm (Seq []) = []
compStm (Seq (s:ss)) = compStm s ++ compStm (Seq ss)



lexer = Token.makeTokenParser emptyDef

integer    = Token.integer    lexer
parens     = Token.parens     lexer
semi       = Token.semi       lexer
identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
whiteSpace = Token.whiteSpace lexer

aexp :: Parser Aexp
aexp = buildExpressionParser aOperators aTerm

aOperators = [ [Infix (reservedOp "*" >> return MultP) AssocLeft]
             , [Infix (reservedOp "+" >> return AddP) AssocLeft,
                Infix (reservedOp "-" >> return SubP) AssocLeft]
             ]
aTerm =  parens aexp
     <|> Var <$> identifier
     <|> Num <$> integer

bexp :: Parser Bexp
bexp = buildExpressionParser bOperators bTerm
bOperators = [ [Prefix (Not <$ reservedOp "not")]
              , [Infix  (AndP <$ reservedOp "and") AssocLeft
              , Infix  (EqB   <$ reservedOp "==") AssocLeft]
              ]
bTerm =  parens bexp
      <|> (reserved "True" >> return (BP True))
      <|> (reserved "False" >> return (BP False))
      <|> rExp

rExp :: Parser Bexp
rExp = do
  a1 <- aexp
  op <- relation
  op a1 <$> aexp

relation :: Parser (Aexp -> Aexp -> Bexp)
relation = (reservedOp "==" >> return EqA)
       <|> (reservedOp "<=" >> return LeA)

stm :: Parser Stm
stm =  statement'

statement :: Parser Stm
statement = stmtSeq <|> parens statement


stmtSeq :: Parser Stm
stmtSeq = f <$> endBy1 statement' semi
  where f l = if length l == 1 then head l else Seq l
        --semi' = try (semi >> notFollowedBy (reserved "else")) <|> try (semi >> notFollowedBy (reserved "do"))


statement' :: Parser Stm
statement' = ifStm <|> whileStm <|> skipStm <|> assignStm

ifStm :: Parser Stm
ifStm = do
  reserved "if"
  b <- bexp
  reserved "then"
  s1 <- statement
  reserved "else"
  s2 <- statement
  
  return $ If b s1 s2

whileStm :: Parser Stm
whileStm = do
  reserved "while"
  b <- bexp
  reserved "do"
  s <- statement
  
  return $ While b s

assignStm :: Parser Stm
assignStm = do
  x <- identifier
  reservedOp ":="
  a <- aexp
  return $ Assign x a

skipStm :: Parser Stm
skipStm = reserved "skip" >> return Skip









program :: Parser Program
program = whiteSpace >> statement `sepBy` semi


-- Your parser definitions go here



-- Assuming your other parser definitions are in scope...
testParser :: IO ()
testParser = do
    let input = "if 1 == 1 then x := 1 else x := 2"
    case parse statement "" input of
        Left e  -> print e
        Right r -> print r


main :: IO ()
main = do
    testParser