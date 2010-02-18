{-# LANGUAGE NoMonomorphismRestriction #-}
module Parser where 

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as PTok 
import Control.Applicative hiding (many,(<|>),Const)
import Prelude hiding (lex)

import Syntax 


parser  = parse $ do 
  PTok.whiteSpace lex
  c <- program
  eof
  return c


lang = PTok.LanguageDef {
       PTok.commentStart = "(*",
       PTok.commentEnd = "*)",
       PTok.commentLine = "--",
       PTok.nestedComments = True,
       PTok.identStart = letter,
       PTok.identLetter = alphaNum <|> char '_',
       PTok.opStart = PTok.opLetter lang,
       PTok.opLetter = oneOf . concat $ PTok.reservedOpNames lang,
       PTok.reservedOpNames = ["+", "-", "*", "/", "==", "/=","?",":",
                              "<", "<=", ">", ">=", "&&", "||", "!", "="],
       PTok.reservedNames = ["read", "print", "if", "then", "else","while", 
			     "do", "fun","inline","var","return"],
       PTok.caseSensitive = True }


lex = PTok.makeTokenParser lang
identifier = PTok.identifier lex
parens = PTok.parens lex 
reserved = PTok.reserved lex
reservedOp = PTok.reservedOp lex
commaSep = PTok.commaSep lex


program = many1 function 

function = do
   reservedOp "fun"
   i <- option False $ reserved "inline" >> return True 
   name <- identifier
   args <- parens $ commaSep identifier 
   b <- block 
   return $ F i name args b 

block =  PTok.braces lex $  instr `sepEndBy` PTok.semi lex

addSemi = updateParserState (\(State input pos user) -> State (';':input) pos user) >> return ()
  
instr = choice [
    Read <$> (reserved "read" *> identifier),
    If <$> (reserved "if" *> parens bExpr ) <*> block <*> (option Nothing ( Just <$> (reserved "else" *> block)) <* addSemi) ,
    (::=) <$> (reserved "var" *> identifier) <*> option Nothing (Just <$> (reserved "=" *> expr)),
    (:=) <$> identifier <*> (reserved "=" *> expr ),
    Print <$> (reserved "print" *> ( Left <$> PTok.stringLiteral lex <|> Right <$> expr )) ,
    While True <$> (reserved "while" *> parens bExpr ) <*> (block <* addSemi) ,
    flip (While False) <$> (reserved "do" *>( block <* addSemi ) ) <*> parens bExpr,
    Return <$> (reserved "return" *> option Nothing (Just <$> expr))]


expr = try condExpr <|> try bExpr <|> try aExpr -- <|> condExpr 

condExpr = Cond <$> (bExpr <* reservedOp "?") <*> (expr <* reservedOp ":") <*> expr 

aExpr = buildExpressionParser [[op "*" AssocLeft, op "/" AssocLeft], 
			       [op "+" AssocLeft, op "-" AssocLeft ]
	] primAExpr where
 op name = Infix $  do
    reservedOp  name
    return $ Expr $ case name of
              "*"   -> Mul
              "/"   -> Div
              "+"   -> Add
              "-"   -> Sub
 primAExpr = choice [
    parens expr,
    Const <$> double ,
    try funCall,
    Var <$> identifier]
 double = (toEnum . fromEnum <$> try (PTok.integer lex)) <|> PTok.float lex

funCall = Call <$> identifier <*> ( parens $commaSep expr  )


bExpr = buildExpressionParser  table  primBExpr where
  table = [[op "||" AssocLeft], [op "&&" AssocLeft]] -- ++ [map (flip op AssocNone)  ["==","/=",">",">=","<","<="]]
  op name  = Infix $ do
       reservedOp name         
       return $ Expr $ case name of     
			"&&" -> And
			"||" -> Or 
    {-  neg  = Prefix $ do
         PTok.reservedOp lex name
         return INot -}
  primBExpr = choice [try $ parens expr,flip Expr  <$> aExpr <*> relOps  <*> aExpr,try funCall,Var <$> identifier ]
  relOps = choice $ map (uncurry pRelOp) [("==",Eq),("/=",NEq),(">",Gt),(">=",Ge),("<",Lt),("<=",Le)]
  pRelOp n o = reserved n >> return o 

