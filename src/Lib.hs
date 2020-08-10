module Lib
    ( someFunc
      , foo
    ) where

someFunc :: IO ()
someFunc = putStrLn foo

foo :: String
foo = "foo"
