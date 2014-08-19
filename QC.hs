{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveDataTypeable, DeriveGeneric, StandaloneDeriving #-}
import Control.Applicative
import Control.Monad.State.Strict
import Data.Functor.Identity
import Data.Monoid
import Test.QuickCheck
import Data.Data
import Pipes
import qualified Pipes.Prelude as P

import Op
import Client (From(..), To(..), model)

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

instance Arbitrary From where
  arbitrary = do
    d <- (arbitrary :: Gen Delta)
    i <- (arbitrary :: Gen Int)
    elements [FServer (TrackedDelta i d), FUser d]

-- | Transformed ops from server won't get messed up otw to user
prop_model_delta :: [From] -> Bool
prop_model_delta inputs = runIdentity $ do
  (outputs, _) <- flip runStateT (0, []) $ P.toListM (each inputs >-> model)
  return $ all sameDelta $ zip inputs outputs
  where sameDelta ((FServer (TrackedDelta _ d)), TUser d') = d == d'
        sameDelta (FUser d, TServer (TrackedDelta _ d'))   = d == d'

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
main :: IO Bool
main = $(quickCheckAll)
