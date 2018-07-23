module Main where

import Idris.AbsSyntax
import Idris.ElabDecls
import Idris.Main
import Idris.Options
import IRTS.Compiler

import IRTS.CodegenCommon
import System.Environment
import System.Exit
import IRTS.Simplified
import Idris.Core.TT
-- generated bin :  .stack-work/dist/x86_64-linux-tinfo6/Cabal-2.0.1.0/build/idris-emptycg/idris-emptycg 
data Opts = Opts {inputs :: [FilePath],
                  output :: FilePath }

showUsage = do putStrLn "A code generator which is intended to be called by the compiler, not by a user."
               putStrLn "Usage: idris-codegen-sdecl <ibc-files> [-o <output-file>]"
               exitWith ExitSuccess

getOpts :: IO Opts
getOpts = do xs <- getArgs
             return $ process (Opts [] "main.js") xs
  where
    process opts ("-o":o:xs) = process (opts { output = o }) xs
    process opts (x:xs) = process (opts { inputs = x:inputs opts }) xs
    process opts [] = opts
    
codeGenSdecls :: CodeGenerator
codeGenSdecls ci = do
  putStrLn "codegen template sdecls"
  writeFile (outputFile ci) $
    foldl1 (++) $ fmap (\(a,b)->sdecls2str a b) $ simpleDecls ci

--use IRTS.Simplified decl
-- data SDecl = SFun Name [Name] Int SExp
sdecls2str :: Name -> SDecl -> String
sdecls2str fname aaa@(SFun _ fArgs i fBody) = (show fname) ++ "--->"++ (show aaa)

jsMain :: Opts -> Idris ()
jsMain opts = do elabPrims
                 loadInputs (inputs opts) Nothing
                 mainProg <- elabMain
                 ir <- compile (Via IBCFormat "sdecl") (output opts) (Just mainProg)
                 runIO $ codeGenSdecls ir

main :: IO ()
main = do opts <- getOpts
          if (null (inputs opts))
             then showUsage
             else runMain (jsMain opts)

{-
module IRTS.CodegenCommon where

data CodegenInfo = CodegenInfo {
    outputFile    :: String
  , outputType    :: OutputType
  , targetTriple  :: String
  , targetCPU     :: String
  , includes      :: [FilePath]
  , importDirs    :: [FilePath]
  , compileObjs   :: [String]
  , compileLibs   :: [String]
  , compilerFlags :: [String]
  , debugLevel    :: DbgLevel
  , simpleDecls   :: [(Name, SDecl)]
  , defunDecls    :: [(Name, DDecl)]
  , liftDecls     :: [(Name, LDecl)]
  , interfaces    :: Bool
  , exportDecls   :: [ExportIFace]
  , ttDecls       :: [(Name, TTDecl)]
  }

type CodeGenerator = CodegenInfo -> IO ()

-}
