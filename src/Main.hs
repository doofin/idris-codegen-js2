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
  putStrLn "codegen sdecls"
  let res = foldl1 (\x y->x++"\n"++y) $ fmap (\(a,b)->sdecls2str a b) $ simpleDecls ci
  putStrLn res
  let ofn = outputFile ci
  putStrLn $ "outputFile : " ++ ofn
  writeFile ofn res


--use IRTS.Simplified decl
-- data SDecl = SFun Name [Name] Int SExp
sdecls2str :: Name -> SDecl -> String
sdecls2str fname aaa@(SFun _ fArgs i fBody) = (show fname) ++ "--->"++ (show aaa)

sexp2str :: SExp -> String
sexp2str x = ""


sdeclMain :: Opts -> Idris ()
sdeclMain opts = do elabPrims
                    loadInputs (inputs opts) Nothing
                    mainProg <- elabMain
                    ir <- compile (Via IBCFormat "sdecl") (output opts) (Just mainProg)
                    runIO $ codeGenSdecls ir

main :: IO ()
main = do opts <- getOpts
          if (null (inputs opts))
             then showUsage
             else runMain (sdeclMain opts)

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
  , simpleDecls   :: [(Name, SDecl)] -- most low level
  , defunDecls    :: [(Name, DDecl)]
  , liftDecls     :: [(Name, LDecl)]
  , interfaces    :: Bool
  , exportDecls   :: [ExportIFace]
  , ttDecls       :: [(Name, TTDecl)]
  }

type CodeGenerator = CodegenInfo -> IO ()

-}
