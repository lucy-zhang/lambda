module LambdaSpec where

import Test.Hspec
import Test.QuickCheck
import Lambda

spec :: Spec
spec = do
    describe "parseExpr and format" $ do
        it "returns the same result" $ do
            property (\e -> (parseExpr . format $ e) == Right e)

    describe "eval" $ do
        mapM_ (uncurry evalSpec) [
              ("x", "x")
            , ("xy", "xy")
            , ("xyz", "xyz")
            , ("(\\x.x)y", "y")
            , ("(\\x.xx)y", "yy")
            , ("(\\x.(\\x.x))x", "\\x.x")
            , ("(\\x.\\y.x)y", "\\y1.y")
            , ("(\\x.\\y.xy)y", "\\y1.y y1")
            , ("(\\x.\\y.x y1)y", "\\y2.y y1")
            , ("(\\x.\\y.x y1)(y y2)", "\\y3.(y y2) y1")
            , ("(\\x.\\y.x y1)(y y3)", "\\y2.(y y3) y1")
            , ("(\\y.\\x.\\x.y)x", "\\x1.\\x1.x")
            , ("(\\n.\\f.\\x.f (n f x))(\\f.\\x.x)", "\\f.\\x.f x")
            , ("(\\n.\\f.\\x.f (n f x))(\\f.\\x.f x)", "\\f.\\x.f (f x)")
            , ("(\\p.\\q.p q p)(\\a.\\b.a)(\\a.\\b.b)", "\\a.\\b.b")
            , ("(\\p.\\a.\\b.p b a) (\\a.\\b.b)", "\\a.\\b.a")
            , ("(\\f.\\x.f(fx)) (\\p.\\a.\\b.p b a) (\\a.\\b.b)", "\\a.\\b.b")
            , ("(\\f.\\x.f(f(fx))) (\\p.\\a.\\b.p b a) (\\a.\\b.b)", "\\a.\\b.a")
            , ("(\\x.y)((\\x.xx)(\\x.xx))", "y")
            ]

        mapM_ (uncurry evalErrorSpec) [
              ("(\\x.xx)(\\x.xx)", NoNormalForm)
            , ("(\\x.xxx)(\\x.xxx)", MaxRecursionDepthExceeded)
            ]

unwrap :: Show error => Either error a -> a
unwrap (Right a) = a
unwrap (Left e) = error $ show e

evalSpec :: String -> String -> Spec
evalSpec ex result = it ("reduces " ++ ex ++ " to " ++ result) $ do
    let e = unwrap (eval recursionDepth =<< parseExpr ex)
        expected = unwrap (parseExpr result)
    e `shouldBe` expected

evalErrorSpec :: String -> NonTerminatingEval -> Spec
evalErrorSpec ex err = it ("fails to reduce " ++ ex ++ " with error " ++ show err) $ do
    let e = eval recursionDepth =<< parseExpr ex
    e `shouldBe` Left (LambdaEvalError err)
