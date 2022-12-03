module Testing(test0, test1, test2, test3) where

test0 :: String
test0 =
    unlines [
          "jy:"
        , "- ' 1'"
    ]

test1 :: String
test1 = unlines [
          "- - []"
        , "  - ' 7o5'"
        -- , "- -2"
    ]

test2 :: String
test2 = unlines [
          "-",
          "- 0"
        , "  - ' 7o5'"
        , "- -2"
    ]

test3 :: String
test3 = unlines [
      "- 6",
      "-",
      "  - 5",
      "  - 4",
      "- 3"
    ]