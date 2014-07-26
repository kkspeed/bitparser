{-# LANGUAGE OverloadedStrings #-}
module Data.Bitstream.Parser
    ( satisfy, bit1Bool, bits, getBits, getWord8, getWord16Be, getWord32Be, getWord64Be
    , getWord1
    ) where

import Text.Parsec.Pos
import Text.Parsec.Prim
import Text.Printf

import Text.Parsec.Bitstream.Lazy

import qualified Data.Bitstream.Lazy as LS
import qualified Data.Bits as Bits
import Data.Word

import Control.Applicative ((<$>))

satisfy :: (LS.Bitstream LS.Right -> Bool) -> Parser (LS.Bitstream LS.Right)
satisfy f = tokenPrim format
                      (\p _ _ -> incSourceColumn p 1)
                      (\t -> if f t
                             then Just t
                             else Nothing)
    where format = printf "0x%x" . (LS.toBits :: LS.Bitstream LS.Right -> Word8)

bit1Bool :: Bool -> Parser (LS.Bitstream LS.Right)
bit1Bool t = satisfy ((==t) . LS.head)

bits :: (Bits.Bits a, Integral a, Show a) => a -> Parser (LS.Bitstream LS.Right)
bits bts = do
  r <- getBits (Bits.bitSize bts)
  if LS.toBits r == bts
  then return r
  else fail $ "Does not match: " ++ show bts

getBits :: Int -> Parser (LS.Bitstream LS.Right)
getBits n = do
  State input pos user <- getParserState
  let bbs = LS.take n input
  let rst = LS.drop n input
  let l' = LS.length bbs
  _ <- setParserState $ State rst (incSourceColumn pos l') user
  if (l' < n)
  then fail "Unexpected EOF!"
  else return bbs

getWord1 :: Parser Word8
getWord1 = LS.toBits <$> getBits 1

getWord8 :: Parser Word8
getWord8 = LS.toBits <$> getBits 8

getWord16Be :: Parser Word16
getWord16Be = LS.toBits <$> getBits 16

getWord32Be :: Parser Word32
getWord32Be = LS.toBits <$> getBits 32

getWord64Be :: Parser Word64
getWord64Be = LS.toBits <$> getBits 64
