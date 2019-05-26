import HUnitExample (double, half)
import Lib (calculateBabySitter, parseTime)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    [returnsCorrectPriceForNormalHours
    ,hasStartTimeFloor
    ,hasEndTimeCeiling
    ,returnsPMTime
    ,returnsAMTime
    ,roundsUpHour]

returnsCorrectPriceForNormalHours =
  testCase "Returns correct price for normal hours"
  $ assertEqual [] 36 (calculateBabySitter "5:00 PM" "8:00 PM")

hasStartTimeFloor =
  testCase "Baby sitter cannot start before 5:00PM (17:00)"
  $ assertEqual [] 12 (calculateBabySitter "4:00 PM" "6:00 PM")

hasEndTimeCeiling =
  testCase "Baby sitter cannot work past 4:00AM (04:00)"
  $ assertEqual [] 12 (calculateBabySitter "3:00 AM" "5:00 AM")

returnsPMTime =
  testCase "5:00 PM is converted to 0" $ assertEqual [] 0 (parseTime "5:00 PM" )

returnsAMTime =
  testCase "4:00 AM is converted to 11" $ assertEqual [] 11 (parseTime "4:00 AM" )

roundsUpHour =
  testCase "5:01 PM is rounded up to 1" $ assertEqual [] 1 (parseTime "5:01 PM")
