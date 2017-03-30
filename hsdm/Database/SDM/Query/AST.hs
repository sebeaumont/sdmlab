module Database.SDM.Query.AST where


-- The AST is simple enough as it evaluates to only one type
-- if we get more complcated than this then we'd need a GADT

type Space = String
type Name = String


data Expr = Symbol Space Name
          | Or Expr Expr
          | And Expr Expr
          | Not Expr


