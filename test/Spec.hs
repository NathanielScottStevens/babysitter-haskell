import HUnitExample (double, half)
import Lib (calculateBabySitter, parseTime)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    [returnsCorrectPriceForNormalHours
    ,returnsCorrectPriceForAfterMidnightHours
    ,returnsCorrectPriceForAfterBedtimeHours
    ,handlesMidnightCorrectly
    ,hasStartTimeFloor
    ,hasEndTimeCeiling
    ,returnsPMTime
    ,returnsAMTime
    ,roundsUpHour]

normalPrice = 12
postBedtimePrice = 8
postMidnightPrice = 16

returnsCorrectPriceForNormalHours =
  testCase "Returns correct price for normal hours"
  $ assertEqual [] (3 * normalPrice) (calculateBabySitter "5:00 PM" "8:00 PM" "9:00 PM")

returnsCorrectPriceForAfterMidnightHours =
  testCase "Returns correct price for after midnight hours"
  $ assertEqual [] (3 * postMidnightPrice) (calculateBabySitter "1:00 AM" "4:00 AM" "8:00 PM")

returnsCorrectPriceForAfterBedtimeHours =
  testCase "Returns correct price for after bedtime hours"
  $ assertEqual [] ((2 * normalPrice) + (2 * postBedtimePrice)) (calculateBabySitter "6:00 PM" "10:00 PM" "8:00 PM")

handlesMidnightCorrectly =
  testCase "Handles midnight correctly"
  $ assertEqual [] postMidnightPrice (calculateBabySitter "12:00 AM" "1:00 AM" "8:00 PM")

hasStartTimeFloor =
  testCase "Baby sitter cannot start before 5:00PM"
  $ assertEqual [] normalPrice (calculateBabySitter "4:00 PM" "6:00 PM" "8:00 PM")

hasEndTimeCeiling =
  testCase "Baby sitter cannot work past 4:00AM"
  $ assertEqual [] postMidnightPrice (calculateBabySitter "3:00 AM" "5:00 AM" "8:00 PM")

returnsPMTime =
  testCase "5:00 PM is converted to 0" $ assertEqual [] 0 (parseTime "5:00 PM")

returnsAMTime =
  testCase "4:00 AM is converted to 11" $ assertEqual [] 11 (parseTime "4:00 AM")

roundsUpHour =
  testCase "5:01 PM is rounded up to 1" $ assertEqual [] 1 (parseTime "5:01 PM")
