import Data.Array.Unboxed

--L42-1
aLargeList :: [Int]
aLargeList = [1..10000000]

--L42-1 UArray: unboxed array
aLargeArray :: UArray Int Int
aLargeArray = array (0,9999999) []

--L42-3
aLargeListDoubled :: [Int]
aLargeListDoubled = map (*2) aLargeList
