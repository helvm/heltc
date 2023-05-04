module HelVM.HelTC.Calculators.LC.CalculatorSpec (spec) where

import           HelVM.HelTC.Calculators.LC.API.GeneratorType
import           HelVM.HelTC.Calculators.LC.API.ParserType

import           HelVM.HelTC.Calculators.LC.Generators.ExpressionGenerator

import           HelVM.HelTC.Calculators.LC.FileUtil

import           HelVM.HelTC.Calculators.LC.Calculator
import           HelVM.HelTC.Calculators.LC.Lambda
import           HelVM.HelTC.Calculators.LC.LambdaReducer
import           HelVM.HelTC.Calculators.LC.Parser

import           HelVM.HelTC.Calculator.Value

import           HelVM.HelIO.Control.Control
import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.Extra
import           HelVM.HelIO.ZipA

import           HelVM.Expectations
import           HelVM.GoldenExpectations

import           Data.Char                                                 (toLower)

import           System.FilePath.Posix

import           Test.Hspec                                                (Spec, describe, it)


spec :: Spec
spec = describe "parse" $ do
  describe "golden" $ forM_ ((
    [ "exp"
    , "factorial"
    , "quote"
    , "shallow"
    , "supriseme-evaluate"
    , "supriseme-reduce"
    ] |><| ["blynn"]
    ) <> (
    [ "string"
    , "string-with-define"
    , "list-of-char"
    , "list-of-nat"
    , "list-of-int"
    , "list-of-nil"
    ] |><| ["list"]
    ) <> (
    [ "bool"
    ] |><| ["lloyd"]
    ) <> (
    [ "prelude"
    , "reverse"
    , "rmk35"
    , "wikipedia"
    , "zhiayang"
    ] |><| ["other"]
    )) $ \(fileName , dirName) -> do
    let path = dirName </> fileName
    let absolutePath = buildAbsolutePathToSourceFile "mlc" path
    let parsedFile = (controlTToIOWithoutLogs $ parseAssemblyFile Meta absolutePath) :: IO InstructionList
    describe "partial" $ do
      let ext = "mlc" </> "partial"
      it ("parsed"   </> path) $ showP <$> parsedFile `goldenShouldIO` buildAbsoluteParsedFileName ext path
      it ("reduced"  </> path) $ showP <$> (reduceLambda <$> parsedFile) `goldenShouldIO` buildAbsoluteReducedFileName ext path
      it ("expanded" </> path) $ safeIOToPTextIO (reduceIL <$> parsedFile) `goldenShouldIO` buildAbsoluteExpandedFileName ext path

    describe "translate" $ forM_ parserTypes $ \parseType -> do
      let parseTypeAsString = toLower <$> show parseType
      let f = generateCodeForIL parseType <$> parsedFile
--      let f2 = (controlTToIOWithoutLogs . minimalizeText parseType =<< f) :: IO Text
      it ("translate" </> parseTypeAsString </> path) $ f `goldenShouldIO` buildAbsoluteExtFileName parseTypeAsString ("mlc" </> "translate") path

--    describe "minify" $ forM_ parserTypes $ \parseType -> do
    describe "minify" $ forM_ [defaultParserType] $ \parseType -> do
      let parseTypeAsString = toLower <$> show parseType
      let f = generateCodeForIL parseType <$> parsedFile
      let f2 = (controlTToIOWithoutLogs . minimizeText parseType =<< f) :: IO Text
      it ("minify" </> parseTypeAsString </> path) $ f2 `goldenShouldIO` buildAbsoluteExtFileName parseTypeAsString ("mlc" </> "minify") path

    describe "generator" $ forM_ generatorTypes $ \generatorType -> do
      let generatorTypeAsString = toLower <$> show generatorType
      let f = assembleFile generatorType Meta absolutePath
      it ("generator" </> generatorTypeAsString </> path) $ f `goldenShouldControlT` buildAbsoluteExtFileName generatorTypeAsString ("mlc" </> "generator") path

  describe "assembleText" $ forM_
    [ (": T \\ x \\ y y"              , "I")
    , (": F \\ x \\ y x"              , "I")
    , (": , \\ x \\ y \\ f f x y"     , "I")
    , (": @ \\ n \\ f \\ x f (n f x)" , "I")
    ] $ \(source , code) ->
    it (toString source) $ assembleText MLC Meta (source <> "\n") `shouldSafe` code

  describe "reduce" $ forM_
    [ (": T \\ x \\ y y"              , [Def "T" (App K I)])
    , (": F \\ x \\ y x"              , [Def "F" (App (App S (App K K)) I)])
    , (": . \\ x T"                   , [Def "." (App K (Var "T"))])
    , (": , \\ x \\ y \\ f f x y"     , [Def "," (App (App S (App (App S (App K S)) (App (App S (App K (App S (App K S)))) (App (App S (App K (App S (App K (App S I))))) (App (App S (App K (App S (App K K)))) (App (App S (App K K)) I)))))) (App K (App (App S (App K K)) I)))])
    , (": @ \\ n \\ f \\ x f (n f x)" , [Def "@" (App (App S (App K (App S (App (App S (App K S)) (App (App S (App K K)) I))))) (App (App S (App (App S (App K S)) (App (App S (App K (App S (App K S)))) (App (App S (App (App S (App K S)) (App (App S (App K (App S (App K S)))) (App (App S (App K (App S (App K K)))) (App (App S (App K K)) I))))) (App K (App (App S (App K K)) I)))))) (App K (App K I))))])
    ] $ \(source , code) ->
    it (toString source) $ (reduceLambda <$> parseAssemblyText Meta (source <> "\n")) `shouldSafe` code

  describe "minimizeText" $ forM_
    [ ("'h"              , "104")
    , (": Y \\ f (\\ x f (x x)) (\\ x f (x x))" , ": Y \\ f (\\ x f (x x)) (\\ x f (x x))")
    ] $ \(source , code) -> do
    it (toString source) $ minimizeText Meta (source <> "\n") `shouldControlT` (code <> "\n")

  describe ("internal" </> show Meta) $ forM_
    [ (": id I"                                   , [Def "id" I])
    , ("\\ x \\ y y"                              , [Eval (Abs "x" (Abs "y" (Var "y")))])
    , (": T \\ x \\ y y"                          , [Def "T" (Abs "x" (Abs "y" (Var "y")))])
    , (": F \\ x \\ y x"                          , [Def "F" (Abs "x" (Abs "y" (Var "x")))])
    , (": . \\ x T"                               , [Def "." (Abs "x" (Var "T"))])
    , (": , \\ x \\ y \\ f f x y"                 , [Def "," (Abs "x" (Abs "y" (Abs "f" (App (App (Var "f") (Var "x")) (Var "y")))))])
    , (": @ \\ n \\ f \\ x f (n f x)"             , [Def "@" (Abs "n" (Abs "f" (Abs "x" (App (Var "f") (App (App (Var "n") (Var "f")) (Var "x"))))))])
    , (": Y \\ f ((\\ x f (x x)) (\\ x f (x x)))" , [Def "Y" (Abs "f" (App (Abs "x" (App (Var "f") (App (Var "x") (Var "x")))) (Abs "x" (App (Var "f") (App (Var "x") (Var "x"))))))])
    , ("(, 0 , 1 , 2 , 3 , 4 .)"                  , [Eval (App (App (App (App (App (App (App (App (App (App (Var ",") (Nat 0)) (Var ",")) (Nat 1)) (Var ",")) (Nat 2)) (Var ",")) (Nat 3)) (Var ",")) (Nat 4)) (Var "."))])
    , ("(, -1 , +2 , -3 , +4 .)"                  , [Eval (App (App (App (App (App (App (App (App (Var ",") (Int (SN Minus 1))) (Var ",")) (Int (SN Plus 2))) (Var ",")) (Int (SN Minus 3))) (Var ",")) (Int (SN Plus 4))) (Var "."))])
    ] $ \(source , code) -> do
    it ("parseAssemblyText" </> toString source) $ parseAssemblyText Meta (source <> "\n") `shouldSafe` code
--    it ("minimizeText" </>  toString source) $ (minimizeText Meta (source <> "\n")) `shouldControlT` (source <> "\n")

  describe ("internal" </> show Symbolic) $ forM_
    [ ("(define id I)"                           , [Def "id" I])
    , ("(lambda (x y) y)"                        , [Eval (AbsRe ["x" , "y"] (Var "y"))])
    , ("(define T (lambda (x y) y))"             , [Def "T" (AbsRe ["x" , "y"] (Var "y"))])
    , ("(define F (lambda (x y) x))"             , [Def "F" (AbsRe ["x" , "y"] (Var "x"))])
    , ("(define . (lambda (x) T))"               , [Def "." (AbsRe ["x"] (Var "T"))])
    , ("(define , (lambda (x y f) (f x y)))"     , [Def "," (AbsRe ["x" , "y" , "f"] (App (App (Var "f") (Var "x")) (Var "y")))])
    , ("(define @ (lambda (n f x) (f (n f x))))" , [Def "@" (AbsRe ["n" , "f" , "x"] (App (Var "f") (App (App (Var "n") (Var "f")) (Var "x"))))])
    ] $ \(source , code) ->
    it ("parseAssemblyText" </> toString source) $ parseAssemblyText Symbolic (source <> "\n") `shouldSafe` code