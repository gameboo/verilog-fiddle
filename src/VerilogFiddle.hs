{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import System.IO
import System.Exit
import Control.Monad
import System.Environment
import System.Console.GetOpt
import qualified Data.Map as M

import VerilogFiddle.Types
import VerilogFiddle.Parse
import VerilogFiddle.PrettyQSYS
import VerilogFiddle.InterfaceInference

-- command line arguments
--------------------------------------------------------------------------------

data OutputFormat = QSYSTCL | TXT

instance Read OutputFormat where
  readsPrec _ s =    [(QSYSTCL, r) | ("qsys_tcl", r) <- lex s]
                  ++ [(    TXT, r) | (     "txt", r) <- lex s]

data Options = Options { optOutputFile   :: Maybe FilePath
                       , optOutputFormat :: OutputFormat
                       , optHelpAndQuit  :: Bool }

defaultOptions :: Options
defaultOptions = Options { optOutputFile   = Nothing
                         , optOutputFormat = TXT
                         , optHelpAndQuit  = False }

options :: [OptDescr (Options -> Options)]
options = [
    Option ['o'] ["output-file"]
           (ReqArg (\arg opts -> opts { optOutputFile = Just arg })
                   "FILEPATH")
           "specify a FILEPATH for the output file"
  , Option ['f'] ["output-format"]
           (ReqArg (\arg opts -> opts { optOutputFormat = read arg })
                   "OUTPUTFORMAT")
           "specify desired OUTPUTFORMAT, one of qsys_tcl, txt (default)"
  , Option ['h'] ["help"] (NoArg \opts -> opts { optHelpAndQuit = True })
           "display help"
  ]

helpMsg :: String
helpMsg = usageInfo header options
  where header = "Usage: verilog-fiddle [OPTION...] files..."

commandOpts :: [String] -> IO (Options, [String])
commandOpts argv =
  case getOpt Permute options argv of
    (optUpdtFns, posArgs, []) ->
      return (foldl (flip id) defaultOptions optUpdtFns, posArgs)
    (_, _, errs) ->
      ioError (userError (concat errs ++ helpMsg))

main :: IO ()
main = do
  -- parse command line arguments
  rawArgs <- getArgs
  (opts, posArgs) <- commandOpts rawArgs
  -- handle help case
  when (optHelpAndQuit opts) do putStrLn helpMsg
                                exitSuccess
  --
  (inptHandle, inptFileName) <-
    case posArgs of f:_ -> openFile f ReadMode >>= \x -> return (x, f)
                    [] -> return (stdin, "stdin")
  outHandle <- case optOutputFile opts of Just f -> openFile f WriteMode
                                          Nothing -> return stdout
  let pretty = case optOutputFormat opts of QSYSTCL -> prettyQSYSTCL
                                            _ -> show
  --
  allVerilogModules <- parseVerilog inptFileName <$> hGetContents inptHandle
  let allModIfcs = inferInterfaces <$> allVerilogModules
  forM_ allModIfcs \x -> do
    hPutStrLn outHandle $ pretty x
