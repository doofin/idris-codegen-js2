module IRTS.CodegenEmpty (codegenEmpty) where

{-
module IRTS.CodegenCommon -- at package idris 

type CodeGenerator = CodegenInfo -> IO ()

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

-}

import IRTS.CodegenCommon

codegenEmpty :: CodeGenerator
codegenEmpty ci = putStrLn "Not implemented"


