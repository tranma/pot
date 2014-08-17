{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveDataTypeable, DeriveGeneric #-}
module Op (Delta, transform) where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Data.Data
import GHC.Generics

prop_compose_lid :: Delta -> Bool
prop_compose_lid d = mempty <> d == d

prop_compose_rid :: Delta -> Bool
prop_compose_rid d = d <> mempty == d

prop_compose_assoc :: Delta -> Delta -> Delta -> Bool
prop_compose_assoc d1 d2 d3 = (d1 <> d2) <> d3 == d1 <> (d2 <> d3)

prop_transform :: Delta -> Delta -> Bool
prop_transform d1 d2 = let (d2', d1') = transform (d1, d2)
                       in  d1 <> d2' == d2 <> d1'


newtype Delta = Delta { undelta :: [Op] } deriving Show

instance Arbitrary Delta where
  arbitrary = normalise <$> (arbitrary :: Gen [Op])
  shrink (Delta xs) = map normalise (shrink xs)

instance Eq Delta where
  Delta a == Delta b = eq a b
    where eq []         [Retain _] = True
          eq [Retain _] []         = True
          eq [Retain _] [Retain _] = True
          eq []         []         = True
          eq (x:xs)     (y:ys)     = x == y && eq xs ys

data Op = Delete Int | Retain Int | Insert String
     deriving (Eq, Show, Data, Typeable, Generic)

instance Arbitrary Op where
  arbitrary = do
    s <- listOf $ elements "abcdefg"
    let i = length s
    elements [Delete i, Retain i, Insert s]
  shrink = genericShrink

instance Monoid Delta where
  mempty = Delta []
  mappend (Delta a) (Delta b) = normalise $ compose a b
    where compose op1s            (Insert s:op2s) = Insert s:(compose op1s op2s)
          compose (Delete x:op1s) op2s            = Delete x:(compose op1s op2s)

          compose (Retain x:op1s) (Delete y:op2s) = case compare x y of
            GT -> Delete y:(compose (Retain (x-y):op1s) op2s)
            LT -> Delete x:(compose op1s (Delete (y-x):op2s))
            EQ -> Delete x:(compose op1s op2s)

          compose (Retain x:op1s) (Retain y:op2s) = case compare x y of
            GT -> Retain y:(compose (Retain (x-y):op1s) op2s)
            LT -> Retain x:(compose op1s (Retain (y-x):op2s))
            EQ -> Retain x:(compose op1s op2s)

          compose (Insert s:op1s) (Delete y:op2s) = case compare (length s) y of
            GT -> compose (Insert (drop y s):op1s) op2s
            LT -> compose op1s (Delete (y - length s):op2s)
            EQ -> compose op1s op2s

          compose (Insert s:op1s) (Retain y:op2s) = case compare (length s) y of
            GT -> Insert (take y s):(compose (Insert (drop y s):op1s) op2s)
            LT -> Insert s:(compose op1s (Retain (y - length s):op2s))
            EQ -> Insert s:(compose op1s op2s)

          compose []              op2s            = op2s
          compose op1s            []              = op1s

transform :: (Delta, Delta) -> (Delta, Delta)
transform (Delta a, Delta b) = let (a', b') = unzip $ t a b
                               in  (normalise a', normalise b')
  where t (Retain x:op1s) (Retain y:op2s) = case compare x y of
          GT -> (Retain y, Retain y):(t (Retain (x-y):op1s) op2s)
          LT -> (Retain x, Retain x):(t op1s (Retain (y-x):op2s))
          EQ -> (Retain x, Retain x):(t op1s op2s)

        t (Insert s:op1s) op2s            = (Retain (length s), Insert s):(t op1s op2s)
        t op1s            (Insert s:op2s) = (Insert s, Retain (length s)):(t op1s op2s)

        t (Delete x:op1s) (Retain y:op2s) = case compare x y of
          GT -> (Retain 0, Delete y):(t (Delete (x-y):op1s) op2s)
          LT -> (Retain 0, Delete x):(t op1s (Retain (y - x):op2s))
          EQ -> (Retain 0, Delete x):(t op1s op2s)

        t (Retain x:op1s) (Delete y:op2s) = case compare x y of
          GT -> (Delete y, Retain 0):(t (Retain (x - y):op1s) op2s)
          LT -> (Delete x, Retain 0):(t op1s (Delete (y-x):op2s))
          EQ -> (Delete x, Retain 0):(t op1s op2s)

        t (Delete x:op1s) (Delete y:op2s) = case compare x y of
          GT -> t (Delete (x - y):op1s) op2s
          LT -> t op1s (Delete (y - x) : op2s)
          EQ -> t op1s op2s

        t [] (Delete y:op2s) = (Delete y, Retain 0):(t [] op2s)
        t [] (Retain y:op2s) = (Retain y, Retain y):(t [] op2s)
        t (Delete x:op1s) [] = (Retain 0, Delete x):(t op1s [])
        t (Retain x:op1s) [] = (Retain x, Retain x):(t op1s [])

        t [] [] = []

normalise :: [Op] -> Delta
normalise d = Delta . n' . foldr (.) id (replicate (length d) n'') . n $ d
  where n  (Delete 0:ops)  = n ops
        n  (Retain 0:ops)  = n ops
        n  (Insert "":ops) = n ops
        n  (x:ops) = x:(n ops)
        n  []      = []
        n' (Delete x:Delete y:ops) = n' (Delete (x+y) :ops)
        n' (Retain x:Retain y:ops) = n' (Retain (x+y) :ops)
        n' (Insert x:Insert y:ops) = n' (Insert (x++y):ops)
        n' (x:ops) = x:(n' ops)
        n' []      = []
        n'' (Delete x:Insert y:ops) = n'' (Insert y:Delete x:ops)
        n'' (x:ops) = x:(n'' ops)
        n'' []      = []

return []
main = $(quickCheckAll)
