module Lib
    ( calculateBabySitter,
      someFunc,
      parseTime
    ) where

import Debug.Trace
import Data.List.Split (splitOn)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

calculateBabySitter :: String -> String -> Int
calculateBabySitter start end
  | startTime < 0 = endTime * 12
  | endTime > 11 = (11 - startTime) * 12
  | otherwise = (endTime - startTime) * 12
  where startTime = parseTime start
        endTime = parseTime end

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
convertToBabySitterTime (hour, "AM") = hour + 7
convertToBabySitterTime (hour, "PM") = hour - 5

