{-# LANGUAGE OverloadedStrings #-}
module Data.Bitstream.Parser
    ( satisfy, bit1Bool, bits, getBits, getWord8, getWord16be, getWord32be, getWord64be
    , getWord1, word8, word16be, string, skipNbytes, satisfyWord8, getBitsWord, shiftToNextByte
    ) where

import Text.Parsec.Pos
import Text.Parsec.Prim
import Text.Printf

import Text.Parsec.Bitstream.Lazy

import qualified Data.Bitstream.Lazy as LS
import qualified Data.Bits as Bits
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.Word

import Control.Monad
import Control.Applicative ((<$>))

satisfy :: (LS.Bitstream LS.Right -> Bool) -> Parser (LS.Bitstream LS.Right)
satisfy f = tokenPrim format
                      (\p _ _ -> incSourceColumn p 1)
                      (\t -> if f t
                             then Just t
                             else Nothing)
    where format = printf "0x%x" . (LS.toBits :: LS.Bitstream LS.Right -> Word8)

satisfyWord8 :: (Word8 -> Bool) -> Parser Word8
satisfyWord8 f = getWord8 >>= \b ->
                 if f b then return b
                 else fail $ "Does not match: " ++ show b

bit1Bool :: Bool -> Parser (LS.Bitstream LS.Right)
bit1Bool t = satisfy ((==t) . LS.head)

bits :: (Bits.Bits a, Integral a, Show a) => a -> Parser (LS.Bitstream LS.Right)
bits bts = do
  r <- getBits (Bits.bitSize bts)
  if LS.toBits r == bts
  then return r
  else fail $ "Does not match: " ++ show bts

string :: String -> Parser String
string s = do
  let bytes = B8.pack s
      nbits = fromIntegral $ B8.length bytes
  pbytes <- LS.toByteString <$> getBits (nbits * 8)
  if pbytes == bytes
  then return s
  else fail $ "Does not match: " ++ show s

getBits :: Int -> Parser (LS.Bitstream LS.Right)
getBits n = do
  State input pos user <- getParserState
  let bbs = LS.take n input
      rst = LS.drop n input
      l'  = LS.length bbs
  _ <- setParserState $ State rst (incSourceColumn pos l') user
  if (l' < n)
  then fail "Unexpected EOF!"
  else return bbs

getBitsWord :: (Bits.Bits a, Integral a) => Int -> Parser a
getBitsWord n = LS.toBits <$> getBits n

word8 :: Word8 -> Parser Word8
word8 w = satisfyWord8 (== w)

word16be :: Word16 -> Parser Word16
word16be w = LS.toBits <$> bits w

getWord1 :: Parser Word8
getWord1 = LS.toBits <$> getBits 1

getWord8 :: Parser Word8
getWord8 = LS.toBits <$> getBits 8

getWord16be :: Parser Word16
getWord16be = LS.toBits <$> getBits 16

getWord32be :: Parser Word32
getWord32be = LS.toBits <$> getBits 32

getWord64be :: Parser Word64
getWord64be = LS.toBits <$> getBits 64

skipNbytes :: Int -> Parser ()
skipNbytes n = replicateM_ n getWord8

shiftToNextByte :: Parser ()
shiftToNextByte = do
  State _ pos _ <- getParserState
  _ <- getBits $ (9 - (sourceColumn pos) `mod` 8) `mod` 8
  return ()
