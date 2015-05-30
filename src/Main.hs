{-
Source:
http://users.electromagnetic.net/bu/astro/sunrise-set.php

Julian days calculation
https://en.wikipedia.org/wiki/Julian_day#Converting_Julian_or_Gregorian_calendar_date_to_Julian_Day_Number
-}
module Main where

import qualified Data.Time as DT


newtype Latitude = Latitude Double deriving (Eq, Show, Ord)
newtype Longitude = Longitude Double deriving (Eq, Show, Ord)
type Location = (Longitude, Latitude)

polipsy4 = (Longitude (-15.0591222), Latitude 49.7859756)

julianJan1st2000 = 2451545 -- the Julian day of January 1, 2000
sunTransitTime = 0.0009
eccentricityTransitVariation = 0.0053
obliguityTransitVariation = 0.0069
fullCircle = 360
halfCircle = 180
earthMeanAnomaly = 357.5291
earthMeanAnomalyDegPerDay = 0.98560028
eqOfCenterCoeficcient1 = 1.9148
eqOfCenterCoeficcient2 = 0.0200
eqOfCenterCoeficcient3 = 00.0003
earthPerihelion = 102.9372
earthObliquityOfEcliptic = 23.45
solarDiscDiameteronEarth = 0.83

main = do
  now <- DT.getCurrentTime
  let (raise, set, dayLen) = calculateSun polipsy4 now
  putStrLn $ "Sun raises today at " ++ (format raise)
               ++ " and sets at " ++ (format set) ++ ". The day is "
               ++ show dayLen ++ " hours long."
    where format = DT.formatTime DT.defaultTimeLocale "%H:%M"

calculateSun :: Location -> DT.UTCTime -> (DT.UTCTime, DT.UTCTime, Double)
calculateSun loc@(lon, _) now = (raise, fromJulianDays set, dayHours)
  where
    raise           = fromJulianDays $ transit - (set - transit)
    set             = solarNoonApprox
                      + (eccentricityTransitVariation * sin m)
                      - (obliguityTransitVariation * sin (2 * sel))
    solarNoonApprox = julianDateOfSolarNoon'' day loc
    m               = meanSolarAnomaly day lon
    sel             = sunEclipticalLongitude day lon
    transit         = julianDateOfSolarNoon day lon
    dayHours        = undefined
    day             = DT.utctDay now

-- Julian date
toJulianDays :: DT.Day -> Integer
toJulianDays day = toInteger d
                   + floor ((153 * mo + 2) / 5)
                   + 365 * yr
                   + floor (dy / 4)
                   - floor (dy / 100)
                   + floor (dy / 400)
                   - 32045
  where (y, m, d) = DT.toGregorian day
        yr = y + 4800 - toInteger a
        a = floor $ (14 - fromIntegral m) / 12
        mo = fromIntegral $ m + 12 * a - 3
        dy = fromIntegral y

fromJulianDays :: Double -> DT.UTCTime
fromJulianDays jdays = undefined

-- Julian cycle since Jan 1, 2000
julianCycle :: DT.Day -> Longitude -> Integer
julianCycle now (Longitude lw) = round
      ((fromIntegral (toJulianDays now) - julianJan1st2000 - sunTransitTime)
      - (lw / fullCircle))

-- approximation of Julian date of solar noon
julianDateOfSolarNoon' :: DT.Day -> Longitude -> Double
julianDateOfSolarNoon' now lon@(Longitude lw) =
      julianJan1st2000 + sunTransitTime
    + (lw / fullCircle) + fromInteger (julianCycle now lon)

-- This will get a very close value to the actual mean solar anomaly
meanSolarAnomaly :: DT.Day -> Longitude -> Double
meanSolarAnomaly now lon = trim360
                           (earthMeanAnomaly + earthMeanAnomalyDegPerDay
                           * (julianDateOfSolarNoon' now lon
                           - julianJan1st2000))

trim360 :: Double -> Double
trim360 v | v < fullCircle = v
          | otherwise      = trim360 (v - fullCircle)

equationOfCenter :: DT.Day -> Longitude -> Double
equationOfCenter now lon = (eqOfCenterCoeficcient1 * sin m)
                         + (eqOfCenterCoeficcient2 * sin (2 * m))
                         + (eqOfCenterCoeficcient3 * sin (3 * m))
  where m =  meanSolarAnomaly now lon

-- ecliptical longitude of the sun.
sunEclipticalLongitude :: DT.Day -> Longitude -> Double
sunEclipticalLongitude now lon = trim360 (meanSolarAnomaly now lon
                                        + earthPerihelion
                                        + equationOfCenter now lon
                                        + halfCircle)

-- accurate Julian date for solar noon
julianDateOfSolarNoon :: DT.Day -> Longitude -> Double
julianDateOfSolarNoon now lon = julianDateOfSolarNoon' now lon
    + (eccentricityTransitVariation * sin (meanSolarAnomaly now lon))
    - (obliguityTransitVariation * sin(2 * sunEclipticalLongitude now lon)) 

-- 
sunDeclination :: DT.Day -> Longitude -> Double
sunDeclination now lon = asin (sin (sunEclipticalLongitude now lon)
                                    * sin earthObliquityOfEcliptic)

-- 
hourAngle :: DT.Day -> Location -> Double
hourAngle now (lon, (Latitude ln)) = acos
                      ((sin (-solarDiscDiameteronEarth) - sin ln * sin decl)
                       / (cos ln * cos decl))
  where decl = sunDeclination now lon


-- go back through the approximation again, this time we use H in the calculation
julianDateOfSolarNoon'' :: DT.Day -> Location -> Double
julianDateOfSolarNoon'' now loc@(lon@(Longitude lw), _) =
    julianJan1st2000 + sunTransitTime
    + (((hourAngle now loc) + lw) / fullCircle)
    + fromInteger (julianCycle now lon)

