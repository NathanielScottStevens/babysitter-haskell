module Lib
    ( calculateBabySitter
    ) where

import Data.List.Split (splitOn)

minTime = 0
midnight = 7
maxTime = 11

normalPrice = 12
postBedtimePrice = 8
postMidnightPrice = 16

-- To simplify calculations,
-- standard time is converted to an hour in the range of 0..11
-- With 0 = 5:00 PM and 11 = 4:00 AM
calculateBabySitter :: String -> String -> String -> Int
calculateBabySitter start end bed = sum $ map (getCostForHour bedTime) [startTime..(endTime - 1)]
  where startTime = parseTime start
        bedTime = parseTime bed
        endTime = parseTime end

getCostForHour :: Int -> Int -> Int
getCostForHour bedTime hour
  | hour >= midnight = postMidnightPrice
  | hour >= bedTime = postBedtimePrice
  | otherwise = normalPrice

parseTime :: String -> Int
parseTime = clamp . convertToBabySitterTime . roundUpHour . parseMinutesAndHour . words

parseMinutesAndHour :: [String] -> (Int, Int, String)
parseMinutesAndHour (time:period:_) = (hour, minute, period)
  where (hour:minute:_) = map (read :: String -> Int) (splitOn ":" time)

roundUpHour :: (Int, Int, String) -> (Int, String)
roundUpHour (hour, 0, period) = (hour, period)
roundUpHour (hour, _, period) = (hour + 1, period)

convertToBabySitterTime :: (Int, String) -> Int
convertToBabySitterTime (12, "AM") = midnight
convertToBabySitterTime (hour, "AM") = hour + 7
convertToBabySitterTime (hour, "PM") = hour - 5

clamp :: Int -> Int
clamp = (max minTime) . (min maxTime)

