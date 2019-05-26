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
parseTime time = case period of "AM" -> (parseHour hour) + 7
                                "PM" -> (parseHour hour) - 5
  where (hour:period:_) = words time

parseHour :: String -> Int
parseHour time = case parsedTime of [hour, 0] -> hour
                                    [hour, _] -> hour + 1
  where parsedTime = map (read :: String -> Int) (splitOn ":" time)



