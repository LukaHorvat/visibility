module Data.Geometry.Visibility.Internal where

-- import Common
import Data.Geometry.Point
import Data.Geometry.Polygon
import Data.Geometry.Angle
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import Data.Function (on)
-- import Debug.Trace (traceShowId, traceShow)

data OrientedSegment = OrientedSegment { origin :: Point
                                       , start  :: Point
                                       , end    :: Point } deriving (Read, Show, Eq)

line :: Point -> Point -> Point -> Double
line (Point x1 y1) (Point x2 y2)
    | x1 == x2  = \(Point x _) -> x - x1
    | otherwise = \(Point x y) -> y - y1 - (y2 - y1) * (x - x1) / (x2 - x1)

sameSide :: (Point -> Double) -> [Point] -> Bool
sameSide lineF pts = let xs = map lineF pts in all (>= 0) xs || all (<= 0) xs

instance Ord OrientedSegment where
    compare (OrientedSegment orig1 s1 e1) (OrientedSegment orig2 s2 e2)
        | orig1 /= orig2 = error "Can't compare oriented segments with different origins"
        | line1Ord == line2Ord = EQ
        | line2Ord == LT       = GT
        | line2Ord == GT       = LT
        | otherwise            = line1Ord
        where (line1, line2) = (line s1 e1, line s2 e2)
              line1Splits = sameSide line1 [s2, e2]
              line2Splits = sameSide line2 [s1, e1]
              line1Ord | not line1Splits                = EQ
                       | sameSide line1 [orig1, s2, e2] = GT
                       | otherwise                      = LT
              line2Ord | not line2Splits                = EQ
                       | sameSide line2 [orig1, s1, e1] = GT
                       | otherwise                      = LT

orient :: Point -> Segment -> Maybe OrientedSegment
orient pt (Segment a b)
    | a1 == a2        = Nothing
    | a1 `ccwFrom` a2 = Just $! OrientedSegment pt b a
    | otherwise       = Just $! OrientedSegment pt a b
    where a1 = polarAngle pt a
          a2 = polarAngle pt b

orientedSegments :: Point -> Polygon -> [OrientedSegment]
orientedSegments cam (Polygon outer holes) =
    mapMaybe (orient cam) $ segments outer ++ concatMap segments holes

intersectsRightRay :: OrientedSegment -> Bool
intersectsRightRay (OrientedSegment (Point x y) (Point x1 y1) (Point x2 y2))
    | not (y1 <= y && y2 >= y || y1 >= y && y2 <= y) = False
    | x1 == x2 = x1 >= x && y1 <= y && y2 >= y
    | y1 == y2 = x1 >= x && x2 >= x && y1 == y
    | otherwise = (x1 * y - x1 * y2 - x2 * y + x2 * y1) / (y1 - y2) >= x

initialSegments :: [OrientedSegment] -> Set OrientedSegment
initialSegments segs = Set.fromList $ filter intersectsRightRay segs

data Event = Start Angle | End Angle deriving (Eq, Ord, Read, Show)

eventAngle :: Event -> Angle
eventAngle (Start a) = a
eventAngle (End a)   = a

isStart :: Event -> Bool
isStart (Start _) = True
isStart (End _)   = False

isEnd :: Event -> Bool
isEnd (Start _) = False
isEnd (End _)   = True

data FoldState = FoldState { activeSegments :: Set OrientedSegment
                           , currentVerts   :: [Point]
                           , currentLowest  :: OrientedSegment } deriving (Eq, Ord, Read, Show)

eventLine :: [OrientedSegment] -> [[(Event, OrientedSegment)]]
eventLine segs = evts
    where evts = groupBy ((==) `on` (eventAngle . fst))
               $ sortBy (comparing (eventAngle . fst))
               $ concatMap enqueue segs
          enqueue seg@(OrientedSegment cam s e) = [ (Start (polarAngle cam s), seg)
                                                  , (End   (polarAngle cam e), seg) ]
intersect :: Point -> Vector -> Segment -> Point
intersect p r (Segment q q') = q + u `scale` s
    where s = q `vecTo` q'
          u = ((p - q) `cross` r) / (s `cross` r)

cast :: Angle -> OrientedSegment -> Point
cast ang (OrientedSegment pt a b) = intersect pt (Point (cos' ang) (sin' ang)) (Segment a b)

processEvent :: FoldState -> [(Event, OrientedSegment)] -> FoldState
processEvent oldState evts = FoldState addNew newVerts newLowest
    where lastLowest = currentLowest oldState
          removeEnds = foldr (Set.delete . snd) (activeSegments oldState) $ filter (isEnd . fst) evts
          addNew     = foldr (Set.insert . snd) removeEnds $ filter (isStart . fst) evts
          newLowest  = Set.findMin addNew
          oldVerts   = currentVerts oldState
          angle      = eventAngle $ fst $ head evts
          newVerts | lastLowest /= newLowest = cast angle newLowest : cast angle lastLowest : oldVerts
                   | otherwise               = oldVerts

-- | Given a Point and a Polygon (possibly with holes), returns a SimplePolygon describing the area
-- visible from the Point inside of the Polygon. O(n log n) where n is the number of points.
visibilityPolygon :: Point -> Polygon -> SimplePolygon
visibilityPolygon cam poly = Simple $! reverse $! currentVerts finalState
    where segs    = orientedSegments cam poly
          initial = initialSegments segs
          evts    = eventLine segs
          finalState = foldl processEvent (FoldState initial [] (Set.findMin initial)) evts
