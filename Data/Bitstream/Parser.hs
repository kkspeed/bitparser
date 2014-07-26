{-# LANGUAGE OverloadedStrings #-}
module Data.Bitstream.Parser
    ( satisfy, bit1Bool
    ) where

import Text.Parsec.Pos
import Text.Parsec.Prim
import Text.Printf

import Text.Parsec.Bitstream.Lazy
import qualified Data.Bitstream.Lazy as LS
import Data.Word

satisfy :: (LS.Bitstream LS.Right -> Bool) -> Parser (LS.Bitstream LS.Right)
satisfy f = tokenPrim format
                      (\p _ _ -> incSourceColumn p 1)
                      (\t -> if f t
                             then Just t
                             else Nothing)
    where format = printf "0x%x" . (LS.toBits :: LS.Bitstream LS.Right -> Word8)

bit1Bool :: Bool -> Parser (LS.Bitstream LS.Right)
bit1Bool t = satisfy ((==t) . LS.head)
