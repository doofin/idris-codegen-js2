{-# LANGUAGE OverloadedStrings #-}
module Main where

import IRTS.Compiler
import Idris.AbsSyntax
import Idris.ElabDecls
import Idris.Main
import Idris.Options

import IRTS.Lang
import IRTS.CodegenCommon
import IRTS.Simplified
import Idris.Core.TT

import System.Environment
import System.Exit

import Data.List
import Data.Char
import Debug.Trace

data Opts = Opts
  { inputs :: [FilePath]
  , output :: FilePath
  }

showUsage = do
  putStrLn
    "A code generator which is intended to be called by the compiler, not by a user."
  putStrLn "Usage: idris-codegen-js2 <ibc-files> [-o <output-file>]"
  exitWith ExitSuccess

getOpts :: IO Opts
getOpts = do
  xs <- getArgs
  return $ process (Opts [] "main.js") xs
  where
    process opts ("-o":o:xs) = process (opts {output = o}) xs
    process opts (x:xs) = process (opts {inputs = x : inputs opts}) xs
    process opts [] = opts


main :: IO ()
main = do
  opts <- getOpts
  if (null (inputs opts))
    then showUsage
    else runMain (sdeclMain opts)

sdeclMain :: Opts -> Idris ()
sdeclMain opts = do
  elabPrims
  loadInputs (inputs opts) Nothing
  mainProg <- elabMain
  ir <- compile (Via IBCFormat "js2") (output opts) (Just mainProg)
  runIO $ codeGenSdecls ir

codeGenSdecls :: CodeGenerator
codeGenSdecls ci = do
  putStrLn "codegen sdecls"
  let res =
        foldl1 (\x y -> x ++ "\n" ++ y) $
        fmap (\(a, b) -> sdecls2str a b) $ simpleDecls ci
  let resWithMain = res ++ mainEntry
  putStrLn resWithMain
  let ofn = outputFile ci
  putStrLn $ "outputFile : " ++ ofn
  writeFile ofn resWithMain

mainEntry = jsnm (sMN 0 "runMain") ++ "();" -- main entry!
--use IRTS.Simplified decl
-- data SDecl = SFun Name [Name] Int SExp
sdecls2str :: Name -> SDecl -> String
sdecls2str fname aaa@(SFun _ fArgs i fBody) = cgFun fname fArgs fBody
-- sdecls2str fname decls | show fname =="Main.main" = ""

jsnm s = "idrisGen_" ++
  let nm = (showCG s)
      nameok c = or [isLetter c,isDigit c]
  in fmap (\x->if nameok x then x else '_') nm

cgFun :: Name -> [Name] -> SExp -> String
cgFun fname args fbody =
  "function " ++
  jsnm fname ++
  "(" ++
  (mconcat $ intersperse "," $ fmap jsnm args) ++
  ") " ++ "{\n" ++ sexp2str fbody ++ "\n}\n\n"

sexp2str :: SExp -> String
sexp2str (SV (Glob n)) = jsret $ jsnm n ++ "()"
sexp2str (SV (Loc i)) = jsret $ loc i 
sexp2str (SApp _ fname args) = -- function application
  (jsnm fname) ++ "("++
  (mconcat $
   fmap
     cgVar
     args)++")"
sexp2str x@(SLet (Loc i) x2 x3) = show x
sexp2str (SUpdate x1 x2) = "SUpdate"
sexp2str (SCon x1 x2 x3 x4) = "SCon"
sexp2str (SCase x1 x2 x3) = ""
sexp2str (SChkCase x1 x2) = ""
sexp2str (SProj x1 x2) = ""
sexp2str (SConst x) = jsret $ cgConst x
sexp2str (SForeign x1 x2 x3) = ""
sexp2str (SOp primfn lvars) = cgOp primfn $ fmap cgVar lvars
sexp2str SNothing = jsret "null"
sexp2str (SError x) = ""

var :: Name -> String
var n = "$" ++ jsnm n

loc :: Int -> String
loc i = "$loc" ++ show i

cgVar :: LVar -> String
cgVar (Loc i) = loc i
cgVar (Glob n) = var n

jsret :: [Char] -> [Char]
jsret s = "return (" ++ s ++ ")"

cgConst :: Const -> String
cgConst (I i) = show i
cgConst (Ch i) = show i -- Treat Char as ints, because PHP treats them as Strings...
cgConst (BI i) = show i
cgConst (Str s) = show s
cgConst TheWorld = "0"
cgConst x | isTypeConst x = "0"
cgConst x = error $ "Constant " ++ show x ++ " not compilable yet"

cgOp :: PrimFn -> [String] -> String
cgOp (LPlus (ATInt _)) [l, r] = "(" ++ l ++ " + " ++ r ++ ")"
cgOp (LMinus (ATInt _)) [l, r] = "(" ++ l ++ " - " ++ r ++ ")"
cgOp (LTimes (ATInt _)) [l, r] = "(" ++ l ++ " * " ++ r ++ ")"
cgOp (LEq (ATInt _)) [l, r] = "(" ++ l ++ " == " ++ r ++ ")"
cgOp (LSLt (ATInt _)) [l, r] = "(" ++ l ++ " < " ++ r ++ ")"
cgOp (LSLe (ATInt _)) [l, r] = "(" ++ l ++ " <= " ++ r ++ ")"
cgOp (LSGt (ATInt _)) [l, r] = "(" ++ l ++ " > " ++ r ++ ")"
cgOp (LSGe (ATInt _)) [l, r] = "(" ++ l ++ " >= " ++ r ++ ")"
cgOp LStrEq [l,r] = "(" ++ l ++ " == " ++ r ++ ")"
cgOp LStrRev [x] = "strrev(" ++ x ++ ")"
cgOp LStrLen [x] = "strlen(utf8_decode(" ++ x ++ "))"
cgOp LStrHead [x] = "ord(" ++ x ++ "[0])"
cgOp LStrIndex [x, y] = "ord(" ++ x ++ "[" ++ y ++ "])"
cgOp LStrTail [x] = "substr(" ++ x ++ ", 1)"
cgOp (LIntStr _) [x] = "\"" ++ x ++ "\""
cgOp (LChInt _) [x] = x
cgOp (LIntCh _) [x] = x
cgOp (LSExt _ _) [x] = x
cgOp (LTrunc _ _) [x] = x
cgOp LWriteStr [_,str] = "console.log(" ++ str ++ ")"
cgOp LReadStr [_] = "idris_readStr()"
cgOp LStrConcat [l,r] = "idris_append(" ++ l ++ ", " ++ r ++ ")"
cgOp LStrCons [l,r] = "idris_append(chr(" ++ l ++ "), " ++ r ++ ")"
cgOp (LStrInt _) [x] = x
cgOp op exps = "error(\"OPERATOR " ++ show op ++ " NOT IMPLEMENTED!!!!\")"

{-
module IRTS.CodegenCommon where

data CodegenInfo = CodegenInfo {
  , simpleDecls   :: [(Name, SDecl)] -- most low level
  , defunDecls    :: [(Name, DDecl)]
  , liftDecls     :: [(Name, LDecl)]
  , interfaces    :: Bool
  , exportDecls   :: [ExportIFace]
  , ttDecls       :: [(Name, TTDecl)]
  }

SFun world [{arg_0}] 1 (SV (Loc 0))
SFun {APPLY_0} [{fn_0},{arg_0}] 1 SNothing
SFun {APPLY2_0} [{fn_0},{arg0_0},{arg1_0}] 1 SNothing
SFun {EVAL_0} [{arg_0}] 1 (SChkCase (Loc 0) [SDefaultCase (SV (Loc 0))])
SFun {runMain_0} [] 1 (SLet (Loc 0) (SLet (Loc 0) SNothing (SApp False Main.main [Loc 0])) (SApp True {EVAL_0} [Loc 0]))

-}
