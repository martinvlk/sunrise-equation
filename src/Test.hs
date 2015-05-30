import qualified Data.Time as DT

aDay = DT.fromGregorian 2000 1 1

toJulianDays :: DT.Day -> Double
toJulianDays day = fromIntegral d + fromIntegral (floor ((153 * mo + 2) / 5))
      + 365 * yr + fromIntegral (floor (dy / 4)) - fromIntegral (floor (dy / 100))
      + fromIntegral (floor (dy / 400)) - 32045
  where (y, m, d) = DT.toGregorian day
        yr = fromIntegral $ y + 4800 - toInteger a
        a = floor $ (14 - fromIntegral m) / 12
        mo = fromIntegral $ m + 12 * a - 3
        dy = fromIntegral y

