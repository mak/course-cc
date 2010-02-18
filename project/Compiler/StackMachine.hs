{-# LANGUAGE PackageImports #-}
module Compiler.StackMachine where 
 
import Syntax
import qualified Data.Map as M 
import StackMachine hiding (Op(..),Call,Read,Neg) 
import qualified StackMachine as SM 
import "mtl" Control.Monad.State 
import Control.Arrow
import Data.Maybe (fromJust)
import Data.Char (ord)
import Data.List (foldl')
import "mtl" Control.Monad.Trans (lift)
-- compileExpr :: Expr -> [SM.Expr]
--
type CompilerState = State (Int,Map) 
type Map = M.Map String Int 

translateOp op = BinOp $ fromJust $ M.lookup op $ M.fromList map  where
  map =  [(Add,SM.Add),(Mul,SM.Mul),(Sub,SM.Sub),(Div,SM.Div),(Lt,SM.Lt),(Gt,SM.Gt),(Le,SM.Le),(Ge,SM.Ge),(Eq,SM.Eq),(NEq,SM.NEq),(And,SM.And),(Or,SM.Or)]


(<$>) = fmap 
inc1 = inc (+1) 
inc = modify  . first 
getFst = fst <$> get 
getSnd = snd <$> get 

compileExpr :: Expr -> StateT Int CompilerState [Instr] 
compileExpr (Const n)	    = lift inc1 >> modify (+1) >> return [Push n]
compileExpr (Var n)	    = do 
  map <- lift getSnd
  m <- get  
  let k = getPos map n 
  modify (+1) >> lift inc1 >> return [SM.Read $ k + m ] 
compileExpr (Expr op e1 e2) = do 
  e2' <- compileExpr e2 
  e1' <- compileExpr e1 
  lift inc1
  modify (subtract 1)
  return $ e2' ++ e1' ++  [translateOp op]
compileExpr (Cond e1 e2 e3) = do 
  m <- get 
  e1' <- compileExpr e1 
  lift inc1
  s1 <- lift getFst 
  put m 
  e2' <- compileExpr e2
  lift inc1
  s2 <- lift getFst 
  put m 
  let n = s2 - s1 + 1 
  e3' <- compileExpr e3
  s3 <- lift getFst 
  put (m+1)
  return $ e1' ++ [JNZ n] ++ e2' ++ (J (s3 - s2 +1):e3')
compileExpr (Not e) =  do
   e' <- compileExpr e
   lift inc1 
   return $ e' ++ [SM.Neg] 
compileExpr  (Call name es)  = do
  n <- get
  es' <- concat `fmap`  mapM  compileExpr es
  lift inc1 >> put  (n+1)
  return $ es' ++ [SM.Call $Left name]

compileStmt :: Instruction -> CompilerState [Instr]
compileStmt (Return e) =  do 
   map <- getSnd 
   let n = M.size map 
       retCode = (++[Ret] ) . take (2*n) $  cycle  [Write 1,Pop]
       retFun e = do 
	e' <- evalStateT (compileExpr e) 0 
	inc (+ (2*n)) >> inc1 
	return $ e' ++ retCode 
   maybe (inc1 >> return [Ret]) retFun  e 
compileStmt (If e b mb) = do
   e' <- evalStateT (compileExpr e) 0 
   inc1 
   s1 <- getFst 
   b1  <- concat <$> mapM compileStmt  b 
   s2 <- getFst
   let n = s2 - s1 +1
   b2  <- maybe (return []) (( concat <$>) . mapM compileStmt ) $ mb
   return $ e' ++ [JNZ n] ++ b1 ++ b2
compileStmt (While True e b) = do
   s  <- getFst 
   e' <- evalStateT (compileExpr e) 0
   inc1
   s1 <- getFst 
   b' <- concat <$> mapM compileStmt b
   inc1
   s2 <- getFst  
   let n = s2 - s1
   return $ e' ++ [JNZ $ n+1] ++ b' ++ [J $ s - s2 +1 ]
compileStmt (Print ese ) = 
   case ese of
      Left str -> let n = length str in inc (+ (3 * n)) >>  return ( str >>= (:[OutputChar,Pop]) . Push . toEnum . ord)
      Right e  -> do {e' <- evalStateT (compileExpr e) 0 ;inc1; return $ e' ++ [Output] }
compileStmt (Read n) = do 
    map <- getSnd  
    let m = getPos map n
    inc (+(m+3)) >> return ([Input,Write $m +1,Pop] ++ replicate m (Write 1))
compileStmt (n := e) =  do
   map <- getSnd 
   let k = getPos map n
   e' <- evalStateT (compileExpr e ) 0
   inc (+(k+2))
   return $ e' ++ [Write $ k+1,Pop] ++ replicate k (Write 1)
compileStmt (n ::= me ) = do 
  let e = maybe (Const 0) id me
  map <- getSnd 
  e' <- evalStateT (compileExpr e ) 0
  let map' = M.insert n 0  $ M.map (+1)  map
  modify (second (const map'))
  return e' 

getPos map n = maybe (error $ "undeclared variable " ++ n ) id . flip  M.lookup map $ n


      
calcAddr map (SM.Call (Left name)) = maybe (error $ "unknown label: " ++ name) (SM.Call . Right) $ M.lookup name map
calcAddr map i = i 

compileFun (F _ _ args code) = second fst $  runState (concat <$> mapM compileStmt code ) (0,M.fromList  $ zip (reverse args) [0..] )

compiler fs = uncurry fillAddr $ foldl' (\a -> glue  a . compileFun) ([],[0]) fs where 
 f  %% g = \(x,y) (x',y') -> (x `f` x',y `g` y')
 glue = flip (:) %% (\(n':ns) n -> n+n':n':ns)
 fillAddr cs bs  = let bs' = map (+3)  $ reverse  bs  
		       m = M.fromList $ zipWith (\a b -> (name b,a)) bs' fs 
		       main = fromJust $ M.lookup "main" m
		   in map (map (calcAddr m)) cs ++ [[SM.Call (Right main),Ret]]



