{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
module StackMachine (Instr(..),Addr,Op(..),run) where

import Data.Record.Label
  -- for nicer records manipulation
import Control.Applicative 
import Control.Arrow 
import Prelude hiding (mod)
import Data.Array
import Data.Char
import "mtl" Control.Monad.Writer 

data StackMachine = SM {
    _stack :: [Double ]
   ,_rets :: [ Int] 
   ,_output :: String
   ,_input :: [Double] 
   ,_code :: CodeBase
   ,_codePtr :: Int 
   }


type CodeBase = Array Int Instr
    

data Instr = Push Double | Pop | Read Int  | Write Int  | 
	     JZ Int  | JNZ Int | J Int  | Call Addr  | Ret |
	     BinOp Op | Neg | Input | Output | OutputChar deriving Show 
type Addr = Either String Int

data Op = Add | Sub | Mul | Div | Lt | Gt | Ge | Le | Eq | NEq | And | Or  deriving Show 

$(mkLabels [''StackMachine])



run cts code = -- do 
  -- cts <- (map read . words) <$> getContents 
  let (sm,trace) = runWriter $ interpret cts code
  in (trace,reverse . get output $ sm)

interpret cts cs = worker emptySM  where 
  emptySM = SM [] [] [] cts code' 0
  len = length cs
  code' = listArray (0,len) cs
{-  
}worker sm = let cp = get codePtr sm 
		  i  = get code sm ! cp 
	      in if cp <= len 
		  then let sm' = step i sm; cp' = get codePtr sm'; 
		       in if cp' == cp then worker $ mod codePtr (+1) sm' else worker sm'
		  else sm 
  -}
  worker sm = do 
    let cp = get codePtr sm
	i  = get code sm ! cp	
    if cp >= len 
       then return sm
       else do
	  tell $ "current stack: " ++ show (get stack sm) ++ "\n"
	  tell $ "executing -> " ++ show cp ++ ": " ++ show i ++ "\n"
	  let sm' = step i sm; cp' = get codePtr sm';
	  if cp' == cp then worker $ mod codePtr (+1) sm' else worker sm'

	
arith op (x:y:xs) = x `f`  y:xs where
  f = interpOp op


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

       

step Pop = mod stack tail 
step (Read n)  = mod stack ((:) . head  . drop n <*> id  ) 
step (Write n) = mod stack ((flip (++) . tail . snd  <*> uncurry (flip (:)) . second head) . splitAt n)
step (Push d) =  mod stack (d:) 
step (BinOp op) =  mod stack (arith op)
step Neg =  mod stack (((:). fromBool . not . toBool  . head) <*>  tail ) 
step Ret = \sm -> let (r:rs) = get rets sm in  sm {_rets = rs,_codePtr = r}
step (J a ) = mod codePtr (+a)
step (Call a) = jumpCall a 
step (JZ a) = jumpCond (+a) id 
step (JNZ a) = jumpCond id (+a) 
step Output = \sm -> let n = head $ get stack sm in  mod output ((put n)++) . mod stack tail $ sm
step OutputChar = \sm -> let n = head $ get stack sm in  mod output ((chr $ fromEnum  n):) $ sm
step Input = \sm -> let n = head $ get input sm in mod stack (n:) . mod input tailIn $ sm
  
tailIn [] = error "not too leazy enouth?"
tailIn (_:xs) = xs

toBool :: Double -> Bool 
toBool = toEnum . fromEnum -- Double -> Int -> Bool 
fromBool = toEnum . fromEnum  -- same steps, but in other way Bool -> Int -> Double 

put = ('\n':) . reverse . show 

jumpCond tf ff sm = mod codePtr (if f then tf else ff) $ mod stack tail $ sm
  where f = toBool . head . get stack $ sm
jumpCall (Right a) sm = set codePtr (a-1) .  mod rets (r:) $ sm where 
  r = get codePtr sm + 1 
jumpCall _ _ = error $ "imporoper addres, must be integer, forget to translate?"
