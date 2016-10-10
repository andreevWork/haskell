import Test.HUnit
import ListSearch (quick_sort)

test1 = TestCase $
    assertEqual "quick_sort," [1,5,6,7,9,32,50] $ quick_sort [1,5,9,7,6,32,50]
tests = TestList
    [TestLabel "quick_sort" test1]