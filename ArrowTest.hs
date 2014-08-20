module ArrowTest () where

import Arrow
import Test.HUnit

tests = test [ "add"       ~: "(+ 1 1)" ~: "2" ~=? (evalString "(+ 1 1)")
             , "subtract"  ~: "(- 7 3)" ~: "4" ~=? (evalString "(- 7 3)")
             , "multiply"  ~: "(* 2 4)" ~: "8" ~=? (evalString "(* 2 4)")
             , "divide"    ~: "(/ 9 3)" ~: "3" ~=? (evalString "(/ 9 3)")
             , "if true"   ~: "(if (== 2 (+ 1 1)) 3 4)" ~: "3" ~=?  evalString "(if (== 2 (+ 1 1)) 3 4)"
             , "if false"  ~: "(if (== 2 (+ 0 1)) 3 4)" ~: "4" ~=?  evalString "(if (== 2 (+ 0 1)) 3 4)"
             , "first 1"   ~: "(first '(1 2 3))" ~: "1" ~=? evalString "(first '(1 2 3))"
             , "first 2"   ~: "(first '(1))" ~: "1" ~=? evalString "(first '(1))"
             , "rest 1"   ~: "(rest '(1 2 3))" ~: "(2 3)" ~=? evalString "(rest '(1 2 3))"
             , "rest 2"   ~: "(rest '(1))" ~: "()" ~=? evalString "(rest '(1))"
             , "cons 1"   ~: "(cons 1 '(2 3))" ~: "(1 2 3)" ~=? evalString "(cons 1 '(2 3))"
             , "cons 2"   ~: "(cons 1 '())" ~: "(1)" ~=? evalString "(cons 1 '())"  
             ]
