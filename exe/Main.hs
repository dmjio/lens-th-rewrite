{-# language TemplateHaskell #-}
module Main where

import Control.Lens

data Person
  = Person
  { _name :: String
  } deriving (Show, Eq)

main :: IO ()
main = pure ()

$(makeLenses ''Person)
