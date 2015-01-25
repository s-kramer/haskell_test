data Direction = DirRight | DirLeft | DirStraight
               deriving (Show)

data Point = Point (Double, Double)
               deriving (Show)

data Line = Line Point Point
               deriving (Show)

data LinePair = LinePair {
                line1 :: Line,
                line2 :: Line}
                deriving (Show)

-- Get angle of a given line
lineAngle (Line (Point(x1,y1)) (Point(x2,y2))) = atan2 (y2-y1) (x2-x1)

-- Convinience function
lineChangeDirectionPoints pointA pointB pointC = lineChangeDirection (LinePair (Line pointA pointB) (Line pointB pointC))

-- Get two lines relation
lineChangeDirection linePair
    | lineAngle lineA > lineAngle lineB = DirRight
    | lineAngle lineA < lineAngle lineB = DirLeft
    | otherwise                         = DirStraight
    where lineA = line1 linePair
          lineB = line2 linePair

-- Test data
p0 = Point(0,0)
p1 = Point(1,1)
p2 = Point(2,2)
p3 = Point((-1), 1)
p4 = Point(2,1)

l1 = Line p0 p1
l2 = Line p1 p2
l3 = Line p1 p3
l4 = Line p1 p4

lp1 = LinePair l1 l2
lp2 = LinePair l1 l3
lp3 = LinePair l1 l4
