import Test.Tasty
import Test.Tasty.HUnit

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
        renderDocument DNull @?= "null"
    , testCase "int" $
        renderDocument (DInteger 5) @?= "5"
    , testCase "int" $
        renderDocument (DInteger 12345689) @?= "12345689"
    , testCase "string" $
        renderDocument (DString "Hello") @?= "'Hello'"
    , testCase "string with '\"' " $
        renderDocument (DString "\"Hello\"") @?= "'\"Hello\"'"
    , testCase "list of ints" $
        renderDocument (DList [DInteger 5, DInteger 6]) @?= listOfInts
    -- IMPLEMENT more test cases:
    -- * nested types
  ]

listOfInts :: String
listOfInts = unlines [
      "---"
    , "- 5"
    , "- 6"
  ]

gameStartTests :: TestTree
gameStartTests = testGroup "Test start document" []

hintTests :: TestTree
hintTests = testGroup "Test hint document" []
