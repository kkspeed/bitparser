{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.Parsec.Bitstream.Lazy
    ( Parser, GenParser, parseFromFile
    ) where

import Text.Parsec.Error
import Text.Parsec.Prim

import qualified Data.Bitstream.Lazy as LS

instance (Monad m) => Stream (LS.Bitstream LS.Right) m (LS.Bitstream LS.Right) where
    uncons s | LS.null s = return $ Nothing
             | otherwise = return $ Just (LS.take (1::Int) s, LS.drop (1::Int) s)

type Parser = Parsec (LS.Bitstream LS.Right) ()

type GenParser t st = Parsec (LS.Bitstream LS.Right) st

parseFromFile :: Parser a -> String -> IO (Either ParseError a)
parseFromFile p fname
    = do input <- LS.readFile fname
         return (runP p () fname input)
