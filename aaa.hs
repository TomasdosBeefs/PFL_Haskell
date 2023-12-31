
module Main (main) where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of the type ‘String’
import qualified Text.Megaparsec.Lexer as L

 Define the lexer
lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser emptyDef
  { Token.identStart = letter
  , Token.identLetter = alphaNum
  , Token.reservedNames = ["if", "then", "else", "while", "do", "skip", "true", "false", "not", "and"]
  , Token.reservedOpNames = ["+", "-", "*", "/", ":=", "==", "<=", "(", ")"]
 }

-- Get the parsers provided by the lexer
identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
integer = Token.integer lexer
whiteSpace = Token.whiteSpace lexer

-- Define the parser
aexp :: Parser Aexp
aexp = buildExpressionParser aOperators aTerm

aOperators = [ [Infix (reservedOp "*" >> return Mul) AssocLeft]
             , [Infix (reservedOp "+" >> return Add) AssocLeft]
             , [Infix (reservedOp "-" >> return Sub) AssocLeft]
             ]

aTerm = parens aexp
  Text.Parsec.<|> fmap Var identifier
  Text.Parsec.<|> fmap Num integer

bexp :: Parser Bexp
bexp = buildExpressionParser bOperators bTerm

bOperators = [ [Prefix (reservedOp "not" >> return Not)]
             , [Infix (reservedOp "and" >> return And) AssocLeft]
             , [Infix (reservedOp "==" >> return Eq) AssocLeft]
             , [Infix (reservedOp "<=" >> return Le) AssocLeft]
             ]

bTerm = parens bexp
  Text.Parsec.<|> (reserved "true" >> return T)
  Text.Parsec.<|> (reserved "false" >> return F)

stm :: Parser Stm
stm = parens stm
  Text.Parsec.<|> sequenceOfStm

sequenceOfStm =
  do list <- (Text.Parsec.sepBy1 stm' semi)
     -- If there's only one statement return it without using Seq.
     return $ if length list == 1 then head list else foldr1 Seq list

stm' :: Parser Stm
stm' = ifStm
  Text.Parsec.<|> whileStm
  Text.Parsec.<|> skipStm
  Text.Parsec.<|> assignStm

ifStm :: Parser Stm
ifStm =
  do reserved "if"
     cond <- bexp
     reserved "then"
     stm1 <- stm
     reserved "else"
     If cond stm1 <$> stm

whileStm :: Parser Stm
whileStm =
  do reserved "while"
     cond <- bexp
     reserved "do"
     While cond <$> stm

assignStm :: Parser Stm
assignStm =
  do var <- identifier
     reservedOp ":="
     Assign var <$> aexp

skipStm :: Parser Stm
skipStm = reserved "skip" >> return Skip

semi :: Parser String
semi = Token.semi lexer

parseString :: String -> Stm
parseString str =
  case parse whileParser "" str of
    Left e -> error $ show e
    Right r -> r

whileParser :: Parser Stm
whileParser = whiteSpace >> stm








