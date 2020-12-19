{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -F -pgmF=lens-th-rewrite-pp #-}
module Main where

import Control.Lens

data Person
  = Person
    { _name :: String
    } deriving (Show)

$(makeLenses ''Person)

main = pure ()


