{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Module for Query language...

module Database.SDM.Query.AST where

import Database.SDM.Algebra
import Database.SDM.Query.IO (SDMCard, SDMPoint)

type Space = String
type Name = String


type LevelSet = ([SDMPoint], SDMCard)

data Expr a where
  -- | Vector valued expressions
  Elem :: Space -> Name -> Expr Vec
  State :: Space -> Name -> Expr Vec
  Or :: Expr Vec -> Expr Vec -> Expr Vec
  And :: Expr Vec -> Expr Vec -> Expr Vec
  Not :: Expr Vec -> Expr Vec
  -- | other stuff
  Topo :: Space -> Double -> Double -> Int -> Expr Vec -> Expr LevelSet

deriving instance Show (Expr a)
  

  
