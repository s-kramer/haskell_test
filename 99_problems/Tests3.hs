module Tests3 where

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Enum, Bounded, Show, Eq, Read, Ord)

minDay = minBound :: Day
maxDay = maxBound :: Day
preSun = pred Sunday
postMon = succ Monday

weekend = [Saturday .. Sunday]
workWeek = [Monday .. Friday]

