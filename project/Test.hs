{-# LANGUAGE DoRec #-}
-- module Test where

import Parser
import Optimize
import Compiler
import StackMachine 
import System.IO

test f = do
  p <- getFile f
  putStrLn "\n\n"
  mapM_ print $ map (deadElim . constFolding) $ p

getFile f =   do 
  s <- readFile f
  case parser f s of
      Right p ->  return p
      Left e  -> error $ show  e 

test2 f = do
   p <- getFile f
   putStrLn "\n\n"
   mapM_ print $ tailCallElim p 

test3 f = do 
  p <- getFile f
  putStrLn "\n\n"
  -- writeFile (f++".out")  $ concatMap (("\n\n"++) . concatMap (("\n"++) . show)) $ reverse $ compiler p 
  let cp = compiler p
      p' = tailCallElim . map (deadElim . constFolding) $ p
      cp' = compiler p'
      cpl = sum  $ map length cp
      cpl' = sum  $ map length cp'
  mapM_ print p'
  writeFile (f++".opt.out")  $ drop 2 . concatMap (("\n"++).concatMap (("\n"++).show)) $ reverse $ cp'
  writeFile (f++".out")  $ drop 2 . concatMap (("\n"++).concatMap (("\n"++).show)) $ reverse $  cp
  putStrLn $ "Unoptimzed length: " ++ show cpl
  putStrLn $ "Optimized length: " ++ show cpl'

test4 f = do 
 -- hSetBuffering stdin (BlockBuffering Nothing)
 -- cts <- (map read . words ) `fmap` getContents 
  p <- getFile f 
  -- putStrLn "\n\n"
  let op =  tailCallElim . map (deadElim . constFolding) $ p
      cp = compiler op
  writeFile (f++".opt") $ concatMap (("\n\n"++) . show) op 
  writeFile (f++".S")  $ drop 2 . concatMap (("\n"++).concatMap (("\n"++).show)) $ reverse $  cp
  cts <-  (map read . words ) `fmap` getContents 
  let (trace,out) = run (0:cts) .  concat $ reverse $ cp
  putStrLn out
  writeFile (f ++".trace") trace

main = test4 "test.asdf"

