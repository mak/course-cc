module Syntax where

import qualified Data.Set as S 

type Program = [Function] 

data Function = F {
    inline :: Bool
   ,name :: Name
   ,args :: [Name]
   ,code :: Block }  deriving (Eq,Show) -- True -> Inline, False -> don't inline

type Block = [Instruction] 

type Name = String 

data Instruction = 
    Name := Expr
  | Name ::= Maybe (Expr)
  | If Expr Block (Maybe Block)
  | While Bool Expr Block 
  | Print (Either String Expr)
  | Read Name 
  | Return (Maybe Expr)  deriving (Eq,Show)

data Expr = Cond Expr Expr Expr | Expr Op Expr Expr | Call Name [Expr] | Const Double | Var Name | Not Expr  deriving (Eq,Show)
data Op = Add | Mul | Sub | Div | Lt | Gt | Le | Ge | Eq | NEq | And | Or deriving (Eq,Show,Ord,Enum)

vars (Var v) = S.singleton v
vars (Expr _ e1 e2) = vars e1 `S.union` vars e2
vars (Call _ es)  = foldr S.union S.empty . map vars $ es 
vars (Cond e1 e2 e3) = vars e1 `S.union` vars e2 `S.union` vars e3
vars _ = S.empty
