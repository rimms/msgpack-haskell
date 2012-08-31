{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Php (
  Config(..),
  generate,
  ) where

import Data.Char
import Data.List
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import System.FilePath
import Text.Shakespeare.Text
import Data.Monoid

import Language.MessagePack.IDL.Syntax

data Config
  = Config
    { configFilePath :: FilePath
    }
  deriving (Show, Eq)

generate:: Config -> Spec -> IO ()
generate _ _ = return ()

