module Testing(test0, test1, test2, test3, test4, friendlyEncode) where

import Data.Yaml as Y ( encodeWith, defaultEncodeOptions, defaultFormatOptions, setWidth, setFormat)
import Types
import Data.String.Conversions (cs)

-- test0 :: String
-- test0 =
--     unlines [
--         "bejjKf: -3"
--     ]

test0 :: String
test0 =
    unlines [
        "1:",
        "    'str':",
        "      - fml",
        "      - end mi"
    ]
-- test0 :: String
-- test0 =
--     unlines [
--         "- bejjKf: -3",
--         "  FYVfRg: ' '",
--         "  I: []",
--         "  Omc: 2 o",
--         "- ''"
--     ]

test1 :: String
test1 = unlines [
        "- key: stuff",
        "  key1: studd",
        "- 666"
    ]

test2 :: String
test2 = unlines [
        "- key: stuff",
        "  key1: studd",
        "  key2: stuss",
        "- 666"
    ]

test3 :: String
test3 = unlines [
        "key:",
        "  key:",
        "    - fml",
        "    - end me",
        "key1: studd"
    ]

test4 = unlines [
        "key1: value1",
        "key2:",
        "  - foo",
        "  - bar",
        "key3: value3"
    ]

-- test3 :: String
-- test3 = unlines [
--       "- 6",
--       "-",
--       "  - 5",
--       "  - 4",
--       "- 3"
--     ]

-- test4 :: Document
-- test4 = DMap [("g",DString "S MBeh9xMAIJ"),("E",DMap [("N",DString "8zd i"),("u",DList [DList [DMap [],DList [DString "mCw",DList [DMap [("nYUqEBm",DString "7 7i 0kc"),("u",DList [DMap [("tQqM",DMap [("Ij",DMap []),("YpBCGZLJE",DString "u Cz8nvD"),("MCXZEF",DList [DList [DList [DMap [("dVg",DMap [("Xuwsu",DMap []),("KewL",DInteger (-12)),("yAcFsRNpEH",DInteger (-2)),("DDH",DString "XAYo4ACS")]),("G",DString ""),("BI",DMap [("SlxKxn",DString " X S3O  C3")]),("zAvWMPz",DMap [("ZjmUGYm",DMap [("LeZMX",DList [DList [DMap [("LfCTUCCS",DMap []),("ybf",DList [DString " NsE4",DList [DMap [("uQizAiM",DString "F1EV6NmB"),("czo",DInteger 10)],DMap [("ortMjGbv",DString "xj07E 8hiO"),("xIrKs",DInteger (-10))],DInteger (-3),DMap [("zvUi",DList [DList [DString "2 Pp753sa 9"],DInteger 6,DList [DInteger 12,DList [],DMap [],DString "uVl2 2Jjw 5"]]),("MqGNY",DString "If5vOO2Ywew "),("FFQOPrw",DInteger 8),("TkhUcdSIpE",DList [DList [],DList [DString "xk JejA11E",DString "ovXv Xui"],DList [DInteger 6,DList [DList [DList [DMap [("iECiXOrRYx",DMap [("nO",DString " tY ")])],DList [],DList [DList [DString "7b6h D9"],DInteger (-12)],DString "17k "],DList [],DInteger 0]],DList [DList [DList [DString "8f te61"],DList [DMap [("hgbQoqIeY",DList [DInteger 10]),("ljUiuuXE",DString "RQ280IpSs")],DString "9 O5d6 8 ",DList [],DMap [("hvtWM",DList [DString "1P2r",DString "0lglx 86 "]),("Gwvyp",DList [DString "JP7U 8",DMap [("SYghZNhQM",DString "9F"),("JU",DMap [("fwOwOnnKW",DString "f  3T Zl 6h "),("QQMCwmY",DString "4 "),("gZqk",DMap [])]),("awyxQ",DInteger (-4))],DInteger 11])]],DString "  kui C",DMap [("ZBnffnaw",DMap [("bo",DString " s No2Xy"),("BvWUX",DInteger 9),("zUfzFMzkC",DMap [("zxDnHJuJ",DMap []),("LaHKJTYkpW",DString " b O "),("zDygJO",DList [DMap [("B",DMap [("zrhhur",DString "aNM j HN"),("dSIEZijKzY",DInteger 10),("RlOZL",DMap [("udeuqOuN",DInteger (-7))])]),("vHNQcLx",DString "CpsG79o7 0L7")],DString " Psy Va"])])])]]]]])]]])]],DInteger (-1),DMap [("pqrOEzrAOV",DMap [("rUywXiPt",DMap [])]),("BS",DString "g 4  J HJd"),("fqXTXcI",DString " 9 1y6  8  ")],DMap [("WT",DMap [("cOG",DList [DMap [],DString "8BM",DString "A q20hP Sc",DMap [("omXEOQJoFV",DString "  5M QI Pu"),("mYZQP",DInteger (-4))]]),("wr",DInteger (-10)),("cacTJeNKI",DList []),("ol",DInteger (-4))]),("elGydp",DString "UY2l")]]),("DPEXD",DMap [("tFPxffC",DInteger 4),("gkDudGSYN",DMap []),("yzbE",DInteger (-3)),("eJZPKnTzH",DList [DMap [("PrqYc",DMap [("xIoFNqpVM",DString "RD3 "),("DKcMdSi",DInteger 11),("mUafHzBQAC",DMap [("LyxLFqed",DInteger 12)]),("bJGlZee",DMap [("BwrJCKh",DList []),("S",DInteger 6)])]),("sbWNXijKGd",DInteger (-2)),("X",DMap [("jRde",DString " TK B2Q1 MYe"),("hklJSjRyCv",DString "8")]),("kjJbOgAW",DList [])],DInteger 6,DList [DList [DMap [],DList [],DString " e",DString "LQQ4Z"],DInteger (-7),DString " "],DString "d0FW "])]),("hjb",DMap [("NacouqXALR",DList [DMap [("K",DMap [("jJDKBQys",DList [DString "StU sNOeC18"]),("H",DMap [("fra",DMap [("SBF",DList [DInteger (-1),DList [DString "l ",DList [DString "m1I"],DInteger 1,DString " "],DList [DMap [("l",DList [DInteger 4,DList [DList [DInteger 6,DString " 1  5 0DL1",DMap [("dTD",DList [DInteger 0,DMap [],DMap [("OBTibTUyDe",DInteger (-6)),("sO",DList [DString " 7  xI53b",DInteger (-1),DString "rMk  tT69 "]),("mxvCdV",DString " ")],DList [DMap [("NffxWjBR",DMap [("GjCS",DInteger 0)]),("ZVidbO",DInteger (-2)),("eewTb",DString "vW2")]]])]],DString " MHd Hh"]]),("cFhObBkIz",DInteger (-10)),("UAOnRbAd",DMap [("PopXW",DMap [("xAwGpEGq",DString "PQ v66Z   "),("sVrQv",DInteger (-2)),("wgKTP",DMap [("kIXetPsP",DInteger (-3)),("Saue",DInteger 2)]),("FGHcIm",DList [])]),("fBNwd",DMap [("AQiVTrBV",DList [DMap [("TNqdaz",DString "d GnB2d Zl5"),("eRFAVpRe",DMap [("wnDOZOTGDc",DString " l8v ")]),("B",DString "4qTG Q0 3"),("ePLIn",DList [])]]),("sxgAKp",DList [DMap [("dQQRkIwZ",DString "s   k  1KF ")],DString "7i 09 "]),("PAglwEFs",DInteger (-5))]),("EzjOze",DMap [("uEiYGyyca",DInteger 9),("rI",DString "Un1N"),("Jno",DMap [("dmWRs",DMap []),("gJ",DMap [("iDXrquYe",DMap [("OUTLrXn",DList [])]),("BrMbS",DMap [("GVAYpoofG",DList [DList [DInteger 4,DMap [("tomxJHPz",DMap [("zDHoZac",DInteger (-2)),("Vwzb",DString " XKOFc3 x3")])],DList [DList [DList [DMap [("DBYvjgs",DString "qO6c023")],DList [DMap [("FOlTFPJQl",DList [DList [DMap [("l",DInteger 3),("cojKIOR",DString "oxZ "),("hftjGbZjr",DString " q y0"),("g",DMap [])]],DList [],DString "0 eC5W ",DInteger (-1)]),("TIT",DList [DMap [("XBzaIDxqAj",DInteger 10)],DInteger 4,DList [DMap [("Ecas",DString "2z pN eDh N"),("ywZxrs",DInteger (-5)),("mbhemrQo",DMap [("NAqiiDAIe",DString "04 747 7RJJ")]),("eUABKvV",DInteger 11)]]])],DString "vW3 052L1  I",DInteger 0],DMap []],DList [],DList [DString "8 7JhVs  f  ",DList [DString " w  d4 v ",DInteger (-2),DString "N  2"]],DInteger (-7)],DMap [("FdhdvsGWU",DMap [("hksBLXdaNt",DString "pF4"),("pSEUW",DString "l NGx6R ")])],DList [DString "dE0"]]],DInteger (-6),DInteger (-5)]),("VkxCRuM",DMap [])]),("Th",DInteger 1)])]),("aXWIiZP",DInteger 10)]),("ZjvkiskRvs",DMap [("rygH",DInteger (-4))])])]],DInteger (-10)]),("pV",DList [DString "WV D8Y",DInteger (-4),DList [DInteger 3,DMap [("PwpeUgyWDm",DList [DString "O",DInteger (-1)])]]]),("DrfDfgcymn",DInteger 10)]),("DNPjub",DString "7Z7MJI2XlE J"),("d",DList [DList [DInteger 4,DMap [("IMGwvd",DInteger 6),("rdoeQkcwcs",DInteger 8)]],DInteger (-10),DMap [("OIi",DList [DString "9s dfl"]),("zMOGQoH",DInteger (-1)),("ARmH",DMap [("YpF",DList [DList [],DList [DList [DMap [("iwWqqlJerI",DString "   1")],DMap [("TqSuEMcgPK",DInteger 3)]],DInteger 4,DMap [("J",DList []),("SUjMnwFsew",DString "Jz 535l o5AI")]]]),("mMWYX",DInteger 5)]),("PQMgfs",DList [DInteger (-10),DInteger (-12)])]]),("dP",DString "n 5 zz u")]),("j",DList []),("BOARGJb",DString "Z reJ5W1H 4 ")]),("iyGLm",DList [DInteger 10,DString "qeM YdkXs 3r"]),("ABxvH",DInteger 12),("gQG",DList [DInteger 2])],DString "1Lvw1pc3a"]),("oPzOrIgwEX",DList []),("yOGRTRVf",DInteger 9),("KuHYSPFp",DMap [("af",DString "Am L"),("nupmVTxVvf",DList [DList [DString "O"],DInteger (-11)]),("sf",DMap [])])])])])],DInteger 4,DList [DInteger (-1),DList [DMap [("mbraHY",DInteger 12),("YCKMBW",DString "c  "),("O",DList [])],DMap [("oOrnqnhp",DList [DString "   BPA1 P3",DString "hi8oa0"])],DMap [("vXgpUF",DInteger 8)],DString "6mYG"],DString "o75L  "],DList [DInteger (-1),DString "1C p1"]],DList [DString "XJ v",DList [DInteger (-11),DList [DInteger 8,DList [DInteger (-5),DInteger (-12),DMap [("GL",DMap [("Evqqb",DInteger 6)]),("mX",DString "F5EHQiCqW"),("Gc",DInteger 1)]]],DMap [("gM",DString "")],DInteger (-11)]]],DInteger (-6)]),("lSvoUMTqC",DList [DMap [("ueMHQpRL",DInteger 10),("aRTBfp",DMap [("kiyqhcjYv",DList []),("yAZx",DList [])])],DList [DMap [("St",DList [DString "Y"]),("PXt",DMap [("YQfnYtBevy",DMap [("YexmGYAEF",DMap [("FMO",DString "K")]),("PoTxjohg",DInteger 2)]),("jAYe",DList [DString "Ut "]),("gWDRYxtLtM",DInteger (-3)),("neruB",DMap [])]),("m",DInteger 6),("AKiMoCVTY",DList [])],DString " 0",DString "7ZUWT J"],DInteger 8,DString "ARqfGV6yk"])]),("ZivuMsdV",DInteger (-7)),("GJixjWSu",DString " L")],DMap [],DList [],DList [DMap [("kZHCadZaHw",DList []),("KginoIC",DList []),("GZyggsc",DList [DInteger 0]),("nUEfL",DList [])],DList [],DList [DInteger 2,DList [],DMap [],DString "  "]]]),("jM",DInteger 3)],DMap [("nxrupSQTvK",DMap []),("CzMXgwZ",DString "sM3"),("OyBbg",DMap [("okvdEFJs",DInteger (-11)),("YTlmgML",DString ""),("XHZwh",DInteger 8),("dO",DList [])])],DString "m 0Jk 9v  sP"]]],DString "z3 a n",DList [DList [DMap [("m",DList [])]]]])]),("yFcwnQh",DString " x4  "),("FZoeBbus",DMap [("ieoRcA",DMap [("VkPcAv",DString "6O 9"),("gGsRdt",DString " AhM Q71R y1")]),("RimbXuKO",DMap [("ozM",DInteger 7),("q",DList [DList [DList [DString "0   GT y",DInteger (-10),DInteger 6]]])]),("yxhdM",DList []),("PyqQfpK",DString "1XKIJs6qi")])]

friendlyEncode :: Document -> String
friendlyEncode doc = cs (Y.encodeWith (setFormat (setWidth Nothing defaultFormatOptions) defaultEncodeOptions) doc)
