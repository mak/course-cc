module Optimize (deadElim,constFolding,optimize,isTailRec,tailCallElim)  where

import Optimize.ConstFolding 
import Optimize.DeadElim 
import Optimize.Inline
import Optimize.TailCall
-- inliner?, inline only funtion with return?

optimize = map (deadElim . constFolding) 


