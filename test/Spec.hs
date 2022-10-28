import Test.Tasty
import Test.Tasty.HUnit

import Lib1 (State(..), emptyState)
import Lib2 (renderDocument, gameStart, hint)
import Types (Document(..), Coord(..))

main :: IO ()
main = defaultMain (testGroup "Tests" [
  toYamlTests,
  gameStartTests,
  hintTests])

toYamlTests :: TestTree
toYamlTests = testGroup "Document to yaml"
  [   testCase "null" $
        renderDocument DNull @?= "null\n"
    , testCase "int" $
        renderDocument (DInteger 5) @?= "5\n"
    , testCase "int" $
        renderDocument (DInteger 12345689) @?= "12345689\n"
    , testCase "string" $
        renderDocument (DString "Hello") @?= "'Hello'\n"
    , testCase "string with '\"' " $
        renderDocument (DString "\"Hello\"") @?= "'\"Hello\"'\n"
    , testCase "list of ints" $
        renderDocument (DList [DInteger 5, DInteger 6]) @?= unlines ["- 5", "- 6"]
    , testCase "mixed list" $
        renderDocument (DList [DInteger 5, DString "Hello", DNull])
          @?= unlines ["- 5", "- 'Hello'", "- null"]
    , testCase "2-level nested list" $
        renderDocument (DList [DInteger 5, DString "Hello", DNull, DList [DString "Nested", DInteger 10]])
          @?= unlines ["- 5", "- 'Hello'", "- null", "-", "  - 'Nested'", "  - 10"]
    , testCase "3-level nested list" $
        renderDocument (DList [DInteger 5, DString "Hello", DNull, DList [DString "Nested", DInteger 10, DList [DInteger 10, DString "L"]]])
          @?= unlines ["- 5", "- 'Hello'", "- null", "-", "  - 'Nested'", "  - 10", "  -", "    - 10", "    - 'L'"]
    , testCase "map of ints" $
        renderDocument (DMap [("Five", DInteger 5), ("Six", DInteger 6)])
          @?= unlines ["Five: 5", "Six: 6"]
    , testCase "mixed map" $
        renderDocument (DMap [("Five", DInteger 5), ("Six", DString "Six"), ("Null", DNull)])
          @?= unlines ["Five: 5", "Six: 'Six'", "Null: null"]
    , testCase "nested map of ints" $
        renderDocument (
          DMap [("Five", DInteger 5), ("Six", DInteger 6),
            ("Nested", DMap [("Nested Seven", DInteger 7), ("Nested Eight", DInteger 8)])]
          )
          @?= unlines ["Five: 5", "Six: 6", "Nested:", "  Nested Seven: 7", "  Nested Eight: 8"]
    , testCase "map of lists of ints" $
        renderDocument (DMap [("List", DList [DInteger 5, DInteger 6])])
          @?= unlines ["List:", "  - 5", "  - 6"]
    , testCase "map of lists of ints and maps" $
        renderDocument (DMap [("List", DList [DInteger 5, DInteger 6, DMap [("Lol", DString "lol")]])])
          @?= unlines [
            "List:",
            "  - 5",
            "  - 6", "  -",
            "    Lol: 'lol'"
          ]
    -- https://yaml-online-parser.appspot.com/?yaml=List%3A%0A++-+5%0A++-+6%0A++-%0A++++Lol%3A+%27lol%27%0A++++List%3A%0A++++++-+6%0A++++++-+9%0A++++++-+null&type=json
    , testCase "map of lists of ints and maps and lists" $
        renderDocument (DMap [("List", DList [DInteger 5, DInteger 6, DMap [("Lol", DString "lol"), ("List", DList [DInteger 6, DInteger 9, DNull])]])])
          @?= unlines [
            "List:",
            "  - 5", "  - 6",
            "  -",
            "    Lol: 'lol'",
            "    List:",
            "      - 6",
            "      - 9",
            "      - null"
          ]
  ]

gameStartTests :: TestTree
gameStartTests = testGroup "Test start document" [
  testCase "DNull" $
    gameStart emptyState DNull @?= Left "Document is not a DMap",
  testCase "DString" $
    gameStart emptyState (DString "labas") @?= Left "Document is not a DMap",
  testCase "DInteger" $
    gameStart emptyState (DInteger 7) @?= Left "Document is not a DMap",
  testCase "DMap-Only-Hints" $
    gameStart emptyState (DMap [("number_of_hints", DInteger 9)]) @?= Left "Unable to find value with specified key",
  testCase "DMap-Only-Cols" $
    gameStart emptyState (DMap [("occupied_columns", DInteger 3)]) @?= Left "Unable to find value with specified key",
  testCase "DMap-Only-Rows" $
    gameStart emptyState (DMap [("occupied_rows", DInteger 3)]) @?= Left "Unable to find value with specified key",
  testCase "DMap-No-List" $
    gameStart emptyState (DMap [("occupied_cols", DInteger 7), ("occupied_rows", DInteger 8), ("number_of_hints", DInteger 8)]) @?= Left "Document is not a DList",
  testCase "DMap-One-Cols-Rows" $
    gameStart emptyState (DMap [("occupied_cols", DList [DInteger 7]), ("occupied_rows", DList [DInteger 10]), ("number_of_hints", DInteger 7)]) @?= Left "Number of rows or cols != 10",
  testCase "DMap-One-Col-Many-Rows" $
    gameStart emptyState (DMap [("occupied_cols", DList[DInteger 8]), ("occupied_rows", DList [DInteger 9, DInteger 7]), ("number_of_hints", DInteger 10)]) @?= Left "Number of rows or cols != 10",
  testCase "DMap-Many-Cols-One-Row" $
    gameStart emptyState (DMap [("occupied_cols", DList[DInteger 8, DInteger 1]), ("occupied_rows", DList [DInteger 9]), ("number_of_hints", DInteger 10)]) @?= Left "Number of rows or cols != 10",
  testCase "Game-Start-Cols-1-Row" $
    gameStart emptyState (DMap [("occupied_cols", DList[DInteger 8, DInteger 1, DInteger 8, DInteger 1, DInteger 8, DInteger 1, DInteger 8, DInteger 1, DInteger 8, DInteger 1]), ("occupied_rows", DList [DInteger 9]), ("number_of_hints", DInteger 10)]) @?= Left "Number of rows or cols != 10",
  testCase "Game-Start-Cols-Rows" $
    gameStart emptyState (DMap [("occupied_cols", DList[DInteger 8, DInteger 1, DInteger 8, DInteger 1, DInteger 8, DInteger 1, DInteger 8, DInteger 1, DInteger 8, DInteger 1]), ("occupied_rows", DList [DInteger 8, DInteger 1, DInteger 8, DInteger 1, DInteger 8, DInteger 1, DInteger 8, DInteger 1, DInteger 8, DInteger 1]), ("number_of_hints", DInteger 10)]) @?= Right (State [] [] [8,1,8,1,8,1,8,1,8,1] [8,1,8,1,8,1,8,1,8,1] 10),
  testCase "Start-State-Not-Empty" $
    gameStart (State [Coord 1 7] [Coord 8 9] [1, 2, 3] [1, 2, 3] 10) (DMap [("occupied_cols", DList[DInteger 8, DInteger 1, DInteger 8, DInteger 1, DInteger 8, DInteger 1, DInteger 8, DInteger 1, DInteger 8, DInteger 1]), ("occupied_rows", DList [DInteger 8, DInteger 1, DInteger 8, DInteger 1, DInteger 8, DInteger 1, DInteger 8, DInteger 1, DInteger 8, DInteger 1]), ("number_of_hints", DInteger 10)]) @?= Right (State [] [] [8,1,8,1,8,1,8,1,8,1] [8,1,8,1,8,1,8,1,8,1] 10),
  testCase "Too-Many-Cols-Rows" $
    gameStart (State [Coord 1 7] [Coord 8 9] [1, 2, 3] [1, 2, 3] 10) (DMap [("occupied_cols", DList[DInteger 8, DInteger 1, DInteger 8, DInteger 1, DInteger 8, DInteger 1, DInteger 8, DInteger 1, DInteger 8, DInteger 1, DInteger 8, DInteger 1]), ("occupied_rows", DList [DInteger 8, DInteger 1, DInteger 8, DInteger 1, DInteger 8, DInteger 1, DInteger 8, DInteger 1, DInteger 8, DInteger 1, DInteger 8, DInteger 1]), ("number_of_hints", DInteger 10)]) @?= Left "Number of rows or cols != 10"
    
  ]

hintTests :: TestTree
hintTests = testGroup "Test hint" [
    testCase "1-Hint" $
      hint emptyState (DMap [("coords",DMap [("head",DMap [("col",DInteger 8),("row",DInteger 9)]),("tail",DNull)])])
        @?= Right (State [] [Coord 8 9] [] [] 0),
    testCase "10-Hint" $
      hint emptyState (DMap
    [("coords",
        DMap [("head",
            DMap [("col", DInteger 8), ("row", DInteger 9)]), ("tail",
                DMap [("head",
                    DMap [("col", DInteger 7), ("row", DInteger 9)]), ("tail",
                        DMap [("head",
                            DMap [("col", DInteger 6), ("row", DInteger 9)]), ("tail",
                                DMap [("head",
                                    DMap [("col", DInteger 5), ("row", DInteger 9)]), ("tail",
                                        DMap [("head",
                                            DMap [("col", DInteger 5), ("row", DInteger 7)]), ("tail",
                                                DMap [("head",
                                                    DMap [("col", DInteger 5), ("row", DInteger 6)]), ("tail",
                                                        DMap [("head",
                                                            DMap [("col",DInteger 5), ("row",DInteger 5)]), ("tail",
                                                                DMap [("head",
                                                                    DMap [("col", DInteger 3), ("row", DInteger 3)]), ("tail",
                                                                        DMap [("head",
                                                                            DMap [("col",DInteger 3), ("row",DInteger 2)]), ("tail",
                                                                                DMap [("head",
                                                                                    DMap [("col",DInteger 3),("row",DInteger 1)]),("tail",DNull)])])])])])])])])])])])
    @?= Right (State [] [
      Coord 3 1, 
      Coord 3 2,
      Coord 3 3,
      Coord 5 5,
      Coord 5 6,
      Coord 5 7,
      Coord 5 9,
      Coord 6 9,
      Coord 7 9,
      Coord 8 9
      ] [] [] 0),

      testCase "5-Hint" $
      hint emptyState (DMap
      [("coords",
        DMap [("head",
          DMap [("col", DInteger 5), ("row", DInteger 5)]), ("tail",
            DMap [("head",DMap [("col", DInteger 4), ("row", DInteger 4)]), ("tail",
              DMap [("head",DMap [("col", DInteger 3), ("row", DInteger 3)]), ("tail",
                DMap [("head",DMap [("col", DInteger 2), ("row", DInteger 2)]), ("tail",
                  DMap [("head",DMap [("col", DInteger 1), ("row", DInteger 1)]), ("tail", DNull)])])])])])])
      @?= Right (State [] [
        Coord 1 1,
        Coord 2 2,
        Coord 3 3,
        Coord 4 4,
        Coord 5 5
        ] [] [] 0),
      
      testCase "No-DMap" $
      hint emptyState DNull @?= Left "Document is not a DMap",

      testCase "No-Coords" $
      hint emptyState (DMap [("number_of_hints", DInteger 9)]) @?= Left "Unable to find value with specified key",

      testCase "No-Col" $
      hint emptyState (DMap [("coords",DMap [("head",DMap [("row",DInteger 9)]),("tail",DNull)])]) @?= Left "Unable to find value with specified key",

      testCase "No-Tail" $
      hint emptyState (DMap [("coords",DMap [("head",DMap [("col",DInteger 8),("row",DInteger 9)])])]) @?= Left "Unable to find value with specified key",

      testCase "No-Head" $
      hint emptyState (DMap [("coords",DMap [("tail",DNull)])]) @?= Left "Unable to find value with specified key",

      testCase "Cols-Rows-Are-DStrings" $
      hint emptyState (DMap [("coords",DMap [("head",DMap [("col",DString "lol"),("row",DString "pabandyk")]),("tail",DNull)])]) @?= Left "Document is not a DInteger",

      testCase "Hint-1-to-Hint-1" $
      hint (State [] [Coord 5 6] [] [] 0) (DMap [("coords",DMap [("head",DMap [("col",DInteger 5),("row",DInteger 6)]),("tail",DNull)])]) @?= Right (State [] [Coord 5 6] [] [] 0),

      testCase "Hint-1-to-Hint-2" $
      hint (State [] [Coord 5 6] [] [] 0) (DMap [("coords",DMap [("head",DMap [("col",DInteger 5),("row",DInteger 6)]),("tail", DMap [("head",DMap [("col", DInteger 7), ("row", DInteger 8)]), ("tail", DNull)])])]) @?= Right (State [] [Coord 7 8, Coord 5 6] [] [] 0),

      testCase "Hint-2-to-Hint-1" $
      hint (State [] [Coord 7 8, Coord 5 6] [] [] 0) (DMap [("coords",DMap [("head",DMap [("col",DInteger 5),("row",DInteger 6)]),("tail",DNull)])]) @?=Right (State [] [Coord 5 6] [] [] 0)
  ]
