module ArrowTest () where

import Arrow
import Test.HUnit

tests = test [ "add" ~: "(+ 1 1)" ~: "2" ~=? (evalString "(+ 1 1)")
             , "add" ~: "(- 7 3)" ~: "4" ~=? (evalString "(- 7 3)")
             , "add" ~: "(* 2 4)" ~: "8" ~=? (evalString "(* 2 4)")
             , "add" ~: "(/ 9 3)" ~: "3" ~=? (evalString "(/ 9 3)")
             ]
               
