import Test.Tasty
import Test.Tasty.HUnit

import Lib1 (State(..))
import Lib2 (renderDocument, gameStart, hint)
import Types (Document(..))

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
    , testCase "map of lists of ints and maps and lists" $  -- TODO
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
gameStartTests = testGroup "Test start document" []

hintTests :: TestTree
hintTests = testGroup "Test start document" [
  testCase "DNull" $
    gameStart (State [] [] [] [] 0) DNull @?= Left "This Document is not a DMap"
  ]
