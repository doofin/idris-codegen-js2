{-# LANGUAGE OverloadedStrings #-}

module Main where

import Idris.AbsSyntax
import Idris.ElabDecls
import Idris.Main
import Idris.Options

import IRTS.CodegenCommon
import IRTS.Compiler
import IRTS.Lang
import IRTS.Simplified
import Idris.Core.TT

import System.Environment
import System.Exit

import Control.Monad.State
import Data.Char
import Data.List
import Debug.Trace

import Data.Hashable

data Opts = Opts
  { inputs :: [FilePath]
  , output :: FilePath
  }

data Info = Info
  { newname :: String
  , oldnm :: String
  , jsstmts :: [String]
  }

type Gen a = State Info a

jsStmtConcat ss = mconcat $ intersperse "\n" ss

jsStmtEmpty = ""

var :: Name -> String
var n = "$varglob" ++ jsName n

loc :: Int -> String
loc i = "$local" ++ show i

cgVar :: LVar -> String
cgVar (Loc i) = loc i
cgVar (Glob n) = var n

jsret :: [Char] -> [Char]
jsret s = "return (" ++ s ++ ")"

cgSExp :: SExp -> Gen String
cgSExp (SV (Glob n)) = return $ (jsName n ++ "()")
cgSExp (SV (Loc i)) = return $ (loc i)
cgSExp (SApp _ fname args) =
  return $
  (jsName fname) ++
  "(" ++ (mconcat $ intersperse "," $ fmap cgVar args) ++ ");\n"
cgSExp (SLet _ valE bodyE) = do
  val <- cgSExp valE
  modify
    (\(Info newnm oldnm stmts) ->
       Info ((++) "v" $ take 6 $ show $ abs $ hash $ newnm ++ "1") newnm $
       stmts ++ [" var " ++ newnm ++ "=" ++ val])
  cgSExp bodyE
cgSExp (SOp primfn lvars) = do
  Info nm old st <- get
  return $ cgPrimFn primfn $ ["", old]
cgSExp (SConst x) = return $ cgConst x
cgSExp SNothing = return $ "None"
cgSExp (SCon Nothing x2 x3 x4) = return $ "con None"
cgSExp _ = return jsStmtEmpty

cgConst :: Const -> String
cgConst (I i) = show i
cgConst (Ch i) = show i -- Treat Char as ints, because PHP treats them as Strings...
cgConst (BI i) = show i
cgConst (Str s) = show s
cgConst TheWorld = "0"
cgConst x
  | isTypeConst x = "0"
cgConst x = error $ "Constant " ++ show x ++ " not compilable yet"

cgPrimFn :: PrimFn -> [String] -> String
cgPrimFn (LPlus (ATInt _)) [l, r] = "(" ++ l ++ " + " ++ r ++ ")"
cgPrimFn (LMinus (ATInt _)) [l, r] = "(" ++ l ++ " - " ++ r ++ ")"
cgPrimFn (LTimes (ATInt _)) [l, r] = "(" ++ l ++ " * " ++ r ++ ")"
cgPrimFn (LEq (ATInt _)) [l, r] = "(" ++ l ++ " == " ++ r ++ ")"
cgPrimFn (LSLt (ATInt _)) [l, r] = "(" ++ l ++ " < " ++ r ++ ")"
cgPrimFn (LSLe (ATInt _)) [l, r] = "(" ++ l ++ " <= " ++ r ++ ")"
cgPrimFn (LSGt (ATInt _)) [l, r] = "(" ++ l ++ " > " ++ r ++ ")"
cgPrimFn (LSGe (ATInt _)) [l, r] = "(" ++ l ++ " >= " ++ r ++ ")"
cgPrimFn LStrEq [l, r] = "(" ++ l ++ " == " ++ r ++ ")"
cgPrimFn LStrRev [x] = "strrev(" ++ x ++ ")"
cgPrimFn LStrLen [x] = "strlen(utf8_decode(" ++ x ++ "))"
cgPrimFn LStrHead [x] = "ord(" ++ x ++ "[0])"
cgPrimFn LStrIndex [x, y] = "ord(" ++ x ++ "[" ++ y ++ "])"
cgPrimFn LStrTail [x] = "substr(" ++ x ++ ", 1)"
cgPrimFn (LIntStr _) [x] = "\"" ++ x ++ "\""
cgPrimFn (LChInt _) [x] = x
cgPrimFn (LIntCh _) [x] = x
cgPrimFn (LSExt _ _) [x] = x
cgPrimFn (LTrunc _ _) [x] = x
cgPrimFn LWriteStr [_, str] = "console.log(" ++ str ++ ")"
cgPrimFn LReadStr [_] = "idris_readStr()"
cgPrimFn LStrConcat [l, r] = "idris_append(" ++ l ++ ", " ++ r ++ ")"
cgPrimFn LStrCons [l, r] = "idris_append(chr(" ++ l ++ "), " ++ r ++ ")"
cgPrimFn (LStrInt _) [x] = x
cgPrimFn op exps = "error(\"OPERATOR " ++ show op ++ " NOT IMPLEMENTED!!!!\")"

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
  let genDecls =
        foldl1 (\x y -> x ++ "\n" ++ y) $
        fmap (\(a, b) -> sdecls2str a b) $ simpleDecls ci
  let declsWithMain = genDecls ++ mainEntry
  putStrLn declsWithMain
  let ofn = outputFile ci
  putStrLn $ "outputFile : " ++ ofn
  writeFile ofn declsWithMain

mainEntry = jsName (sMN 0 "runMain") ++ "();" -- main entry!

--use IRTS.Simplified decl
-- data SDecl = SFun Name [Name] Int SExp
sdecls2str :: Name -> SDecl -> String
sdecls2str fname aaa@(SFun _ fArgs i fBody) = cgDecls fname fArgs fBody

jsName s =
  "idr_" ++
  let nm = (showCG s)
      nameok c = or [isLetter c, isDigit c]
   in fmap
        (\x ->
           if nameok x
             then x
             else '_')
        nm

cgDecls :: Name -> [Name] -> SExp -> String
cgDecls fname args fbody =
  let (exp, Info _ _ stmts) = runState (cgSExp fbody) (Info "nm1" "nm1" [])
      exp2 = mconcat $ intersperse "\n" $ stmts
   in "function " ++
      jsName fname ++
      "(" ++
      (mconcat $ intersperse "," $ fmap jsName args) ++
      ") " ++ "{\n" ++ exp2 ++ "}\n"
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
{-

cgSexpOld :: (String -> String) -> SExp -> String
cgSexpOld f (SV (Glob n)) = f $ jsName n ++ "()"
cgSexpOld f (SV (Loc i)) = f $ loc i
--sexpRec f (SApp True fname args) = ""-- function application,gen garbish
cgSexpOld f (SApp _ fname args) -- function application
 =
  (jsName fname) ++
  "(" ++ (mconcat $ intersperse "," $ fmap cgVar args) ++ ");\n"
cgSexpOld f aa@(SLet (Loc i) v sc) =
  cgSexpOld (\x -> "\n var " ++ loc i ++ " = " ++ x ++ ";\n") v ++ cgSexpOld f sc
cgSexpOld f (SUpdate x1 x2) = ""
cgSexpOld f (SCon x1 x2 x3 x4) = ""
cgSexpOld f (SCase x1 x2 x3) = ""
cgSexpOld f (SChkCase x1 x2) = ""
cgSexpOld f (SProj x1 x2) = ""
cgSexpOld f (SConst x) = f $ cgConst x
cgSexpOld f (SForeign x1 x2 x3) = ""
cgSexpOld f (SOp primfn lvars) = cgOp primfn $ fmap cgVar lvars
cgSexpOld f SNothing = ""
cgSexpOld f (SError x) = ""

--sexpRec f (SApp True fname args) = ""-- function application,gen garbish
-- function application

sexpRec2 f (SLet (Loc v1) a@(SLet (Loc v2) e2 e3) e1) =
  let s1 = "var " ++ (loc v1) ++ " = " ++ (sexpRec2 f e3) -- second last
      s3 = sexpRec2 f a -- first
   in jsStmtConcat $ f ++ [s3,s1]
sexpRec2 f (SLet (Loc v1) e1 e2) =
  let s1 = "var " ++ (loc v1) ++ " = " ++ (sexpRec2 f e1)
      s2 = jsret $ sexpRec2 f e2
  in jsStmtConcat [s1,s2]

sexpRec2 f (SUpdate x1 x2) = jsStmtEmpty
sexpRec2 f (SCon x1 x2 x3 x4) = jsStmtEmpty
sexpRec2 f (SCase x1 x2 x3) = jsStmtEmpty
sexpRec2 f (SChkCase x1 x2) = jsStmtEmpty
sexpRec2 f (SProj x1 x2) = jsStmtEmpty
sexpRec2 f (SConst x) = ""
sexpRec2 f (SError x) = ""
sexpRec2 f (SForeign x1 x2 x3) = ""

sexpRec2 f e = "error!"++ (show e)
-}
