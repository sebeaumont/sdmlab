{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Module for DSL syntax  

module Database.SDM.Query.AST where

import Database.SDM
import Database.SDM.Algebra


type Space = String
type Name = String

-- | Query DSL expression (GADT) type allow us to contruct polymorphic
-- expressions for query DSL

data Expr a where
  -- | `Vec` valued constructors
  Elem :: Space -> Name -> Expr Vec                   -- Elements which correspond to basis vectors
  State :: Space -> Name -> Expr Vec                  -- State vectors 
  Or :: Expr Vec -> Expr Vec -> Expr Vec              -- Boolean operators
  And :: Expr Vec -> Expr Vec -> Expr Vec
  Not :: Expr Vec -> Expr Vec
  -- | Topo with metric lower bound, density upper bound and cardinality constraints with target
  -- vector expression computes LevelSet
  Topo :: Space -> Double -> Double -> Int -> Expr Vec -> Expr LevelSet
  -- | Terms matching with cardinality 
  Terms :: Space -> String -> Int -> Expr TermMatch
  -- TODO design full language: including imperative/declarative/binding forms
  -- + training, data processing etc. 

deriving instance Show (Expr a)

data Stmt


