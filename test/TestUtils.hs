module TestUtils where

import Test.Hspec

shouldSucceed :: (Show a) =>  Either a b -> Expectation
shouldSucceed (Left res) = fail ("Expected to succeed. Failed with: " ++ show res)
shouldSucceed (Right _) = return ()

shouldSucceedWith :: (Show a, Eq b, Show b) =>  Either a b -> b -> Expectation
shouldSucceedWith (Left res) _ = fail ("Expected to succeed. Failed with: " ++ show res)
shouldSucceedWith (Right actual) expected = actual `shouldBe` expected

shouldFail :: (Show b) =>  Either a b -> Expectation
shouldFail (Right res) = fail ("Expected to fail. Succeeded with: " ++ show res)
shouldFail (Left _) = return ()

shouldBeWithLength :: [a] -> Int -> Expectation
l `shouldBeWithLength` len = length l `shouldBe` len
