{-# LANGUAGE FlexibleInstances #-}
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.String.Conversions
import Data.Yaml as Y ( encode )

import Lib1 (State(..), emptyState)
import Lib2 (renderDocument, gameStart, hint)
import Lib3 (parseDocument, tokenizeYaml, Token(..))
import Types (Document(..), Coord(..))
-- import Test.Tasty.Runners (TestTree(TestGroup))

main :: IO ()
main = defaultMain (testGroup "Tests" [
  toYamlTests,
  fromYamlTests,
  gameStartTests,
  hintTests,
  tokenizeYamlTests,
  properties])

properties :: TestTree
properties = testGroup "Properties" [golden, dogfood]

golden :: TestTree
golden = testGroup "Handles foreign rendering"
  [
    testProperty "parseDocument (Data.Yaml.encode doc) == doc" $
      \doc -> parseDocument (cs (Y.encode doc)) == Right doc
  ]

dogfood :: TestTree
dogfood = testGroup "Eating your own dogfood"
  [
    testProperty "parseDocument (renderDocument doc) == doc" $
      \doc -> parseDocument (renderDocument doc) == Right doc
  ]


tokenizeYamlTests :: TestTree
tokenizeYamlTests = testGroup "Test `tokenizeYaml`" [
    testCase "joinUnknown" $
    tokenizeYaml (init (unlines [
        "- \"test\"",
        "- asd"
      ])) @?= [
        TokenDashListItem,
        TokenScalarString "test",
        TokenNewLine,
        TokenDashListItem,
        TokenScalarString "asd"
      ]
  , testCase "joinBetweenScalar" $
    tokenizeYaml "foo    bar: f o o o b a a r" @?= [
      TokenScalarString "foo    bar",
      TokenKeyColon,
      TokenScalarString "f o o o b a a r"
    ]
  , testCase "TODO: Test case name" $
    -- init is needed because unlines produces an additional newline
      tokenizeYaml (init $ unlines [
          "- \"test",
          " test \\\"",
          "",
          "- \"",
          "- ",
          "  - \"test\""
        ]) @?= [
          TokenDashListItem,
          TokenScalarString "test\n test \\\"\n\n- ",
          TokenDashListItem,
          TokenNewLine,
          TokenSpace 2,
          TokenDashListItem,
          TokenScalarString "test"
        ]
    , testCase "Nested list" $ tokenizeYaml (unlines [
        "List:",
        "- 5",
        "- 6",
        "-",
        "  Lol  asd: 'lol'",
        "  List:",
        "    - 6",
        "    - 9",
        "    - null"
      ]) @?= [TokenScalarString "List", TokenKeyColon, TokenNewLine, 
        TokenDashListItem, TokenScalarInt 5, TokenNewLine,
        TokenDashListItem, TokenScalarInt 6, TokenNewLine,
        TokenDashListItem, TokenNewLine,
        TokenSpace 2, TokenScalarString "Lol  asd", TokenKeyColon, TokenScalarString "lol", TokenNewLine,
        TokenSpace 2, TokenScalarString "List", TokenKeyColon, TokenNewLine,
        TokenSpace 4, TokenDashListItem, TokenScalarInt 6, TokenNewLine,
        TokenSpace 4, TokenDashListItem, TokenScalarInt 9, TokenNewLine,
        TokenSpace 4, TokenDashListItem, TokenScalarNull, TokenNewLine
      ]
  ]

fromYamlTests :: TestTree
fromYamlTests = testGroup "Document from yaml"
  [     testCase "null (null)" $
          parseDocument "null" @?= Right DNull
      , testCase "null (~)" $
          parseDocument "~" @?= Right DNull
      , testCase "null ()" $
          parseDocument "" @?= Right DNull
      , testCase "integer" $
          parseDocument "56" @?= Right (DInteger 56)
      , testCase "integer" $
          parseDocument "420" @?= Right (DInteger 420)
      , testCase "string with \"" $
          parseDocument "\"foobar\"" @?= Right (DString "foobar")
      , testCase "string with '" $
          parseDocument "'foobar'" @?= Right (DString "foobar")
      , testCase "List of one" $ parseDocument "- 5" @?= Right (DList [DInteger 5])
      , testCase "Simple simple list" $ (parseDocument (unlines [
          "- asd",
          "- 1",
          "- nice",
          "- 2"
        ])) @?= Right (DList [DString "asd", (DInteger 1), DString "nice", (DInteger 2)])
      , testCase "Simple list" $ parseDocument (unlines [
          "- asd",
          "-",
          "  - lol",
          "- nice",
          "- "
        ])
          @?=
            Right (DList [DString "asd", DInteger 5, DList [DString "lol"], DString "nice", DNull])
        , testCase "Simple mapping" $ parseDocument (unlines [
          "key1: value1",
          "key2: value2",
          "key3: value3"
        ])
          @?=
            Right (DMap [("key1", DString "value1"), ("key2", DString "value2"), ("key3", DString "value3")])
      -- , testCase "Simple map" $ parseDocument (unlines [
      --     "key0: value0"
      --   ]) @?= Right (DMap [("key0", DString "value0")])
      -- , testCase "Keyless map" $ parseDocument (unlines [ ": value" ]) @?= (Left _)
  ]

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
    , testCase "TrickySpooky Halloween-Themed" $
        renderDocument (DMap [("key1", DMap [("key2", DList [DInteger 1,DMap [("key3", DList [DInteger 1,DInteger 3,DNull,DMap [("", DNull)],DMap []]),("key4", DString "")],DNull])]),("key5", DList [])])
        @?= unlines [
          "key1:",
          "  key2:",
          "    - 1",
          "    -",
          "      key3:",
          "        - 1",
          "        - 3",
          "        - null",
          "        -",
          "          '': null",
          "        -",
          "          {}",
          "      key4: ''",
          "    - null",
          "key5:",
          "  []"
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
