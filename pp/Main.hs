{-# LANGUAGE ViewPatterns #-}
module Main where

import Debug.Trace
import GHC.Plugin.LensThRewrite
import System.Environment
import System.Exit

import Language.Haskell.GHC.ExactPrint.Parsers
import Language.Haskell.GHC.ExactPrint

import GHC                                     hiding (parseModule)
import CoreSyn
import GhcPlugins                              hiding ((<>))
import HsDecls
import HsDumpAst
import HsExtension
import HsSyn
import OccName
import RdrName
import TcEvidence
import Var

main :: IO ()
main = do
  args <- getArgs
  case args of
    original:input:output:_ ->
      go input output

go :: String -> String -> IO ()
go input output = do
  putStrLn "running....."
  result <- parseModule input
  case result of
    Right (k, L x s) -> do
      let n = rewriteModule s
          txt = postProcess $ showSDocUnsafe (ppr n)
      writeFile output txt
    Left s -> print s >> exitFailure

postProcess
  :: String
  -> String
postProcess
  = unlines
  . fmap rewriteLine
  . lines

rewriteLine
  :: String
  -> String
rewriteLine original@(words -> xs) =
  if "::" `elem` xs && "Lens'" `elem` xs
    then
      case xs of
        name : "::" : "Lens'" : innerType : rest ->
          unwords $
          [ name
          , "::"
          , "Lens'"
          , innerType
          ] ++ ("(" : rest ++ [")"])
        _ -> error $ "rewriteLine: Bad match: " <> original
    else
      if "Lens" `elem` xs
        then traceShow ("failed: " ++ original) original
        else original
