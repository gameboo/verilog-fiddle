{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import System.Environment
import qualified Data.Map as M

import VerilogFiddle.Types
import VerilogFiddle.Parse
import VerilogFiddle.PrettyQSYS
import VerilogFiddle.InterfaceInference

main :: IO ()
main = do
  fileName <- (!! 0) <$> getArgs
  allVerilogModules <- parseVerilogFile fileName
  let allModIfcs = inferInterfaces <$> allVerilogModules
  forM_ allModIfcs print
