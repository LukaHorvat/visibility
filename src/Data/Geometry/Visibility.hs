-- | The algorithm is relatively simple. The points are sorted by their polar angle relative to the
-- reference point, keeping track of which line segment they belong to.
-- In this ordering, they are assigned tags representing whether they are a start of a segment, or
-- it's end.
-- A comparison is defined on those oriented segments that determines which segment covers the other
-- if viewed from the reference point. Then the list of points is folded over by putting segments in
-- a set, and removing them from it when a starting point or an end point is encountered,
-- respectively. Every time the closest segment to the reference point changes, we output two points
-- of a visibility polygon by computing relevant ray-segment intersections.
module Data.Geometry.Visibility
    ( Point(..)
    , Polygon(..)
    , SimplePolygon(..)
    , visibilityPolygon ) where

import Data.Geometry.Point as X (Point(..))
import Data.Geometry.Polygon as X (Polygon(..), SimplePolygon(..))
import Data.Geometry.Visibility.Internal as X (visibilityPolygon)
