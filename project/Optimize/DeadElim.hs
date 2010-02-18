module Optimize.DeadElim where 

import Syntax 
import qualified Data.Set as S
import Control.Arrow

deadElim :: Function -> Function 
deadElim f@(F{code=c}) = f {code = fst . deadElim' S.empty $ c }
deadElim' set = foldr (\s (c,s') -> step c s' s ) ([],set) where
  step code set (n := _)   | S.notMember n set = (code,set)
  step code set s@(_ := e)	      =  (s:code, set `S.union` vars e)
  step code set (n ::= _ ) | S.notMember n set = (code,set) 
  step code set s@(_ ::= Just e)      =  (s:code, set `S.union` vars e)
  step code set (While f e b)	      =	let set' = set `S.union` vars e 
					in first ((:code) . While f e ) $  deadElim' set' b
  step code set s@(Print (Right e))   = (s:code,set `S.union` vars e)
  step code set s@(Return (Just e))   = (s:code,set `S.union` vars e)
  step code set (If e b Nothing)      =	let set' = set `S.union` vars e 
					in first ((:code) . flip (If e) Nothing) $  deadElim' set' b
  step code set (If e b1 (Just b2) )  = let set' = set `S.union` vars e 
					    (b1',m)  = deadElim' set' b1
					in ((:code) . If e b1' . Just) *** (`S.union` m) $  deadElim' set' b2
  step code set (Read v)	      = (Read v:code, S.insert v set)
  step code set s  = (s:code,set)


