module Database.SDM.Query.AST where

import Database.SDM.Algebra

-- The AST is simple enough as it evaluates to only one type
-- if we get more complcated than this then we'd need a GADT

data Expr = Term
          | Or Expr Expr
          | And Expr Expr
          | Not Expr

eval :: Expr -> Vec
eval = undefined

