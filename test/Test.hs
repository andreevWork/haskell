import Test.HUnit
import qualified Algoritms.List.ListSearch as ListSearch
import qualified Algoritms.List.ListSort as ListSort


{---------------

    ListSearch Test START

----------------}

list_search_array = takeWhile  (<10000) [1,3..]

list_search_tests =
    [
    TestCase $
        assertEqual "ListSearch simple" (Just 2) $ ListSearch.binarySearch list_search_array 5
    ,
    TestCase $
        assertEqual "ListSearch head" (Just 0) $ ListSearch.binarySearch list_search_array 1
    ,
    TestCase $
        assertEqual "ListSearch last" (Just $ length list_search_array - 1) $ ListSearch.binarySearch list_search_array 9999
    ,
    TestCase $
        assertEqual "ListSearch without" Nothing $ ListSearch.binarySearch list_search_array (-7)
    ,
    TestCase $
        assertEqual "ListSearch without, inside" Nothing $ ListSearch.binarySearch list_search_array 4
    ,
    TestCase $
            assertEqual "ListSearch empty" Nothing $ ListSearch.binarySearch [] 4
    ]

{---------------

    ListSearch Test END

----------------}



{---------------

    ListSort Test START

----------------}

list_sort_tests =
    [
    TestCase $
        assertEqual "ListSort simple" [1, 5, 32, 50, 81, 99] $ ListSort.quickSort [1, 5, 99, 81, 32, 50]
    ,
    TestCase $
        assertEqual "ListSort with same elements" [1, 5, 5, 32, 50, 81, 81, 99] $ ListSort.quickSort [1, 5, 99, 81, 32, 50, 5, 81]
    ]

{---------------

    ListSort Test END

----------------}

tests = TestList $
    list_search_tests ++ list_sort_tests