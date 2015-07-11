{-
Source:
http://users.electromagnetic.net/bu/astro/sunrise-set.php

Julian days calculation
https://en.wikipedia.org/wiki/Julian_day#Converting_Julian_or_Gregorian_calendar_date_to_Julian_Day_Number
-}
module Main where

import qualified Data.Time as DT
import Text.Printf


-- places
newtype Latitude = Latitude Double deriving (Eq, Show, Ord)
newtype Longitude = Longitude Double deriving (Eq, Show, Ord)
type Location = (Longitude, Latitude)

polipsy4 = (Longitude 15.0591222, Latitude 49.7859756)
aš = (Longitude 12.1949917, Latitude 50.2238694)
praha = (Longitude 14.4341414, Latitude 50.0835494)
place = polipsy4

-- constants
julianJan1st2000 = 2451545::Double
sunTransitTime = 0.0009
eccentricityTransitVariation = 0.0053
obliguityTransitVariation = 0.0069
fullCircle = 360
halfCircle = 180
earthMeanAnomaly = 357.5291
earthMeanAnomalyDegPerDay = 0.98560028
eqOfCenterCoeficcient1 = 1.9148
eqOfCenterCoeficcient2 = 0.0200
eqOfCenterCoeficcient3 = 0.0003
earthPerihelion = 102.9372
earthObliquityOfEcliptic = 23.45
solarDiscDiameterOnEarth = 0.83

main = do
  now <- DT.getCurrentTime
  let (raise, set, dayLen) = calculateSun place now
  putStrLn $ "Sun raises today at " ++ format raise
               ++ " and sets at " ++ format set ++ ". The day is "
               ++ printf "%.2f" dayLen ++ " hours long."
    where format = DT.formatTime DT.defaultTimeLocale "%H:%M"

calculateSun :: Location -> DT.UTCTime -> (DT.UTCTime, DT.UTCTime, Double)
calculateSun loc@(lon, _) now = (raiseT, setT, dayHours)
  where
    raise           = transit - (set - transit)
    raiseT          = fromJulianDays (raise - 0.5)
    set             = solarNoonApprox
                      + (eccentricityTransitVariation * (sin.rad) m)
                      - (obliguityTransitVariation * (sin.rad) (2 * sel))
    setT            = fromJulianDays (set - 0.5)
    solarNoonApprox = julianDateOfSolarNoon'' day loc
    m               = meanSolarAnomaly day lon
    sel             = sunEclipticalLongitude day lon
    transit         = julianDateOfSolarNoon day lon
    dayHours        = realToFrac (DT.diffUTCTime setT raiseT) / 60 / 60
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
        dy = fromIntegral yr

fromJulianDays :: Double -> DT.UTCTime
fromJulianDays jdn = DT.UTCTime (DT.fromGregorian year
                                      (fromInteger month) (fromInteger day))
                                (DT.secondsToDiffTime daySeconds)
  where
    y = 4716
    j = 1401
    m = 2
    n = 12
    r = 4
    p = 1461
    v = 3
    u = 5
    s = 153
    w = 2
    b = 274277
    c = -38
    f = jdnIntegral + j + (((4 * jdnIntegral + b) `div` 146097) * 3) `div` 4 + c
    jdnIntegral = floor jdn
    e = r * f + v
    g = (e `mod` p) `div` r
    h = u * g + w
    day = (h `mod` s) `div` u + 1
    month = ((h `div` s + m) `mod` n) + 1
    year = (e `div` p) - y + (n + m - month) `div` n
    daySeconds = floor $ (jdn - fromIntegral jdnIntegral) * (24*60*60)

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
meanSolarAnomaly now lon = mod360
                           (earthMeanAnomaly + earthMeanAnomalyDegPerDay
                           * (julianDateOfSolarNoon' now lon
                           - julianJan1st2000))

mod360 :: Double -> Double
mod360 v | v < fullCircle = v
          | otherwise      = mod360 (v - fullCircle)

rad :: Double -> Double
rad x = x * pi / halfCircle

unrad :: Double -> Double
unrad x = x / pi * halfCircle

equationOfCenter :: DT.Day -> Longitude -> Double
equationOfCenter now lon = (eqOfCenterCoeficcient1 * (sin.rad) m)
                         + (eqOfCenterCoeficcient2 * (sin.rad) (2 * m))
                         + (eqOfCenterCoeficcient3 * (sin.rad) (3 * m))
  where m =  meanSolarAnomaly now lon

-- ecliptical longitude of the sun.
sunEclipticalLongitude :: DT.Day -> Longitude -> Double
sunEclipticalLongitude now lon = mod360 (meanSolarAnomaly now lon
                                        + earthPerihelion
                                        + equationOfCenter now lon
                                        + halfCircle)

-- accurate Julian date for solar noon
julianDateOfSolarNoon :: DT.Day -> Longitude -> Double
julianDateOfSolarNoon now lon = julianDateOfSolarNoon' now lon
    + (eccentricityTransitVariation * (sin.rad) (meanSolarAnomaly now lon))
    - (obliguityTransitVariation * (sin.rad) (2 * sunEclipticalLongitude now lon)) 

-- 
sunDeclination :: DT.Day -> Longitude -> Double
sunDeclination now lon = (unrad.asin) ((sin.rad) (sunEclipticalLongitude now lon)
                                    * (sin.rad) earthObliquityOfEcliptic)

-- 
hourAngle :: DT.Day -> Location -> Double
hourAngle now (lon, Latitude ln) = (unrad.acos.rad)
                      (((sin.rad) (-solarDiscDiameterOnEarth) - (sin.rad) ln * (sin.rad) decl)
                       / rad ((cos.rad) ln * (cos.rad) decl))
  where decl = sunDeclination now lon

-- go back through the approximation again, this time we use H in the calculation
julianDateOfSolarNoon'' :: DT.Day -> Location -> Double
julianDateOfSolarNoon'' now loc@(lon@(Longitude lw), _) =
    julianJan1st2000 + sunTransitTime
    + ((hourAngle now loc + lw) / fullCircle)
    + fromInteger (julianCycle now lon)

