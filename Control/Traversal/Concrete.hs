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
  traversalr :: Traversable t =>
                 ( (b -> t b -> t b) -> a -> m (t b) -> m (t b) )
                 -> (t b -> m (t b))
                 -> t a -> m (t b)

instance CTraversable [] where
  traversalr cliftA2 cpure = foldr 
  traversalr cliftA2 _ (x:xs) = cliftA2 (:) 
