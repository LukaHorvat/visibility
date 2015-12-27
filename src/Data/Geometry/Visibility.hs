module Data.Geometry.Visibility
    ( Point(..)
    , Polygon(..)
    , SimplePolygon(..)
    , visibilityPolygon ) where

import Data.Geometry.Point as X (Point(..))
import Data.Geometry.Polygon as X (Polygon(..), SimplePolygon(..))
import Data.Geometry.Visibility.Internal as X (visibilityPolygon)
