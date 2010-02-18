module Optimize.ConstFolding where 

import qualified Data.Map as M 
import Data.List (foldl')
import Control.Arrow
import Syntax 
 
computeConstExpr _  (Const n) = Const n
computeConstExpr constMap v@(Var a) = maybe v Const $ flip M.lookup constMap a
computeConstExpr constMap e@(Expr op e1 e2 )= 
  case (computeConstExpr constMap e1,computeConstExpr constMap e2) of
       (Const 1,e) | op == Mul -> e 
       (Const 0,e) | op == Add -> e
       (e,Const 1) | op == Mul -> e
       (Const 0,e) | op == Add -> e  
       (Const n,Const m)-> Const $ interpOp op n m
       (e1,e2)		-> Expr op e1 e2
computeConstExpr constMap (Call name es)  = Call name $ map (computeConstExpr constMap) es
computeConstExpr constMap (Cond e1 e2 e3) = 
  case computeConstExpr constMap e1 of
       Const n -> if 0 == n then computeConstExpr constMap e2 else computeConstExpr constMap e3
       e       -> Cond e (computeConstExpr constMap e2 ) (computeConstExpr constMap e3)


interpOp Add  = (+)
interpOp Sub  = (-)
interpOp Div  = (/)
interpOp Mul  = (*)
interpOp Lt   = \x y -> toEnum . fromEnum $ x < y
interpOp Le   = \x y -> toEnum . fromEnum $ x <= y
interpOp Gt   = \x y -> toEnum . fromEnum $ x > y
interpOp Ge   = \x y -> toEnum . fromEnum $ x >= y
interpOp Eq   = \x y -> toEnum . fromEnum $ x == y
interpOP NEq  = \x y -> toEnum . fromEnum $ x /= y

constFolding :: Function -> Function 
constFolding f@(F {code = c} )= f {code = reverse . fst . constFolding' False M.empty $ c}
constFolding' inB map  = foldl' (uncurry (step inB))  ([],map)  where
  step inB code map (n := e) = 
    let map' = if inB then M.delete n map else map in 
    case computeConstExpr map' e of 			      
      Const m -> (n := Const m : code, M.insert n m  map')
      e' ->  (n := e' :code, map')
  step inB code map ( n ::= Just e ) = 
    let map' = if inB then M.delete n map else map in
    case computeConstExpr map' e of 			      
      Const m -> (n ::= Just (Const m) : code, M.insert n m map')
      e' ->  (n ::= Just e' :code, map')
  step _ code map (While f e b) = 
    case computeConstExpr map e of 
      Const n | n == 0 -> if f then (code,map) else catCode code $ constFolding' True map b
      e' -> addCode code . first ( While f e' . reverse)  $ constFolding' True map b
  step _ code map (If e b b') = 
    case computeConstExpr map e of
      Const n | n == 0 -> maybe (code,map) (catCode code)  $ constFolding' False map `fmap` b' 
      Const n ->  catCode code $ constFolding' False map b
      e'      ->	 let (cb,m1) = inBlock map b; mb = inBlock map `fmap` b'; newIf = If e' cb 
		 in addCode code $ maybe (newIf Nothing, m1) ((newIf . Just ) *** M.union m1) mb
  step _ code map (Return e) = addCode code . flip (,) map $  Return $ computeConstExpr map `fmap` e
  step _ code map s = (s:code,map)
  catCode  code = first (++ code ) -- *** (`M.union` map )
  addCode c = first  (:c)
  inBlock map = first reverse . constFolding' True map
			  
	     

