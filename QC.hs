{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveDataTypeable, DeriveGeneric, StandaloneDeriving #-}
import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Data.Data

import Op

deriving instance Typeable Op

instance Arbitrary Delta where
  arbitrary = normalise <$> (arbitrary :: Gen [Op])
  shrink (Delta xs) = map normalise (shrink xs)

instance Arbitrary Op where
  arbitrary = do
    s <- listOf $ elements "abcdefg"
    let i = length s
    elements [Delete i, Retain i, Insert s]
  shrink = genericShrink

prop_compose_lid :: Delta -> Bool
prop_compose_lid d = mempty <> d == d

prop_compose_rid :: Delta -> Bool
prop_compose_rid d = d <> mempty == d

prop_compose_assoc :: Delta -> Delta -> Delta -> Bool
prop_compose_assoc d1 d2 d3 = (d1 <> d2) <> d3 == d1 <> (d2 <> d3)

prop_transform :: Delta -> Delta -> Bool
prop_transform d1 d2 = let (d2', d1') = transform (d1, d2)
                       in  d1 <> d2' == d2 <> d1'

return []
main = $(quickCheckAll)
