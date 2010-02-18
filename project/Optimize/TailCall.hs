module Optimize.TailCall where

import Syntax 
import qualified Data.Map as M
import qualified Data.Set as S 
import Control.Arrow
import Data.List (sortBy) 

haveSelfCall name (Expr _ e1 e2) = not $ haveSelfCall name e1 || haveSelfCall name e2
haveSelfCall name (Call name' es) = name == name' -- || any (haveSelfCall name) es
haveSelfCall name (Cond e1 e2 e3) = any (haveSelfCall name) [e1,e3]
haveSelfCall _ _ = False 

haveSelfCall' name (Call name' _ ) = name == name'
haveSelfCall' name (Cond _ e1 e2) = any (haveSelfCall' name) [e1,e2]
haveSelfCall' _ _ = False 


getRets = foldr step [] where 
  step (Return (Just e)) = (e:)
  step (While _ _ b) = (getRets b ++)
  step (If _ b1 b2)  = let rs = getRets b1 
		       in maybe (rs ++) (\b2' -> ((rs ++ getRets b2' ) ++))  b2
  step _ = id 

isTailRec (F{name = n,code = b}) = uncurry (&&) $ 
  all (haveSelfCall n) *** (not . null . filter (haveSelfCall' n)) $ dup $ getRets b 

dup x = (x,x)

transfom name args e@(If f  b1 (Just b2)) =  
  case (getRets b1, getRets b2)  of 
    ([],[e1]) | haveSelfCall' name e1  -> While True (Not f ) (b2 >>= transform name args ) : b1
    ([e1],[]) | haveSelfCall' name e1  -> While True (Not f ) (b1 >>= transform name args ) : b2
    _ -> [e]
transform name args (Return (Just (Cond b e (Call name' args')))) = 
  [While True (Not b) (sort $ zip args args'),Return $ Just e]
transform name args (Return (Just (Cond b (Call name' args') e))) = 
  [While True (Not b) (sort $ zip args args'),Return $ Just e]
transform name args (Return (Just (Call name' args'))) | name == name' =  sort $ zip args args' 
transform _ _ s = [s]

tailCallElim = map (ifFun isTailRec tailCallElim' id)
tailCallElim' f@(F{code = b,name = n ,args = a}) = f{code = init b ++ transform n a (last b)}

sort = map (uncurry (:=)) . sortBy depend  where 
  depend (n,e) (n1,e1) = let v1 = vars e; v2 = vars e1
			 in S.member n v2 `compare` S.member n1 v1

ifFun bf tf ff x = if bf  x then tf  x  else ff x
