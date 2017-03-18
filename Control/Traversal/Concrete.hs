-- |
-- Module      : Control.Traversal.Concrete
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemueller $ geo.uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

{-# LANGUAGE TypeFamilies #-}

module Control.Traversal.Concrete where
    
import Data.Traversable

import Control.Applicative

class Traversable t => CTraversable t where
  data Builder t a :: *
  traversalL :: Traversable t => ((Maybe b -> Builder t b -> Builder t b)
                                  -> Maybe a -> mtb -> mtb)
                                -> ((Builder t b -> t b) -> mtb -> m (t b))
                                -> t a -> m (t b)

instance CTraversable [] where
  data Builder [] a = ListBuilder ([a]->[a])
  traversalL q p [] = 
