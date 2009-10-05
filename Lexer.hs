module Lexer where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (unfoldr)
import Data.Char (isDigit)
import Control.Arrow

syms = "+-()*/"

data Token = Sym Char | Num Int | Error String deriving Show

lexer :: B.ByteString -> [Token]
lexer = unfoldr lex
  where lex bs | B.null bs = Nothing
               | h `elem` syms = Just (Sym h,t)
               | isDigit h =  fmap (first Num) $ B.readInt bs
               | otherwise = Just (err h, t)
          where h = B.head bs
                t = B.tail bs
                err = Error . ("unexpected symbol: "++) . (:[])


