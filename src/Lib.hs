module Lib
    ( calculateBabySitter,
      someFunc,
      parseTime
    ) where

import Debug.Trace
import Data.List.Split (splitOn)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

minTime = 0
midnight = 7
maxTime = 11

calculateBabySitter :: String -> String -> Int
calculateBabySitter start end = sum $ map getCostForHour [startTime..(endTime - 1)]
  where startTime = max minTime $ parseTime start
        endTime = min maxTime $ parseTime end

getCostForHour :: Int -> Int
getCostForHour hour
  | hour < midnight = 12
  | hour >= midnight = 16


-- To simplify calculations, standard time is converted to an hour
-- in the range of 0..11
-- With 0 = 5:00 PM and 11 = 4:00 AM
parseTime :: String -> Int
parseTime = convertToBabySitterTime . roundUpHour . parseMinutesAndHour . words

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

