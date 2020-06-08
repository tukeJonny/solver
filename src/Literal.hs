{-# LANGUAGE TemplateHaskell #-}
module Literal (
    Lit (..)
  , invert
  , isOpposite
  , getTrue, getFalse
    ) where

import Control.Lens

data Lit = Lit { _name :: Int, _inv :: Bool } deriving (Show)

makeLenses ''Lit

instance Eq Lit where
  l1 == l2 = let sameName = (l1^.name)  == (l2^.name)
                 sameInv = (l1^.inv) == (l2^.inv)
              in
                sameName && sameInv

invert :: Lit -> Lit
invert l = let inverted = not (l^.inv)
           in
              l&inv.~inverted

isOpposite :: Lit -> Lit -> Bool
isOpposite l1 l2 = let nameMatched = (l1^.name) == (l2^.name)
                       isOpposite = (l1^.inv) /= (l2^.inv)
                    in
                        nameMatched && isOpposite

getTrue :: Lit -> Bool
getTrue l = not (l^.inv)

getFalse :: Lit -> Bool
getFalse l = l^.inv


