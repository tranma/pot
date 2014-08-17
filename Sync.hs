module Sync where

import Data.List
import Op

incoming :: Int -> [Delta] -> Delta -> (Delta, [Delta])
incoming n history op = mapAccumL (curry transform) op $ drop n history
