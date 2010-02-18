module Optimize.TailCall where

import Syntax 
import qualified Data.Map as M

hasEName name (Expr _ e1 e2) = hasName name e1 || hasName name e2
hasEName name (Call name' es) = name == name' || any (hasName name) es
hasName name (Cond e1 e2 e2 = any (hasName name) [e1,e2,e3]
hasName _ = False

isTailRec (F{name = n 
