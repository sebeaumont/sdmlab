{-# LANGUAGE GADTs #-}

-- | Query AST evaluation

module Database.SDM.Query.Eval (eval) where

import Control.Applicative

import Math.Algebra

import Database.SDM
import Database.SDM.Query.IO
import Database.SDM.Query.AST (Expr(..), Stmt(..))


-- | Evaluate the polymorphic query expression via. database IO

eval :: SDMDatabase -> Expr a -> IO (Either SDMStatus a)
--
eval db (Elem s n) = do
  sv <- ensureSpace db s
  case sv of
    Left err -> return $ Left err
    Right sp -> getElementalVector sp n
--
eval db (State s n) = do
  sv <- ensureSpace db s
  case sv of
    Left err -> return $ Left err
    Right sp -> getSemanticVector sp n
--
eval db (Or e1 e2) = liftA2 add <$> (eval db e1) <*> (eval db e2)
--
eval db (And e1 e2) = liftA2 mul <$> (eval db e1) <*> (eval db e2)
--
eval db (Not e1) = undefined -- XXXXXX TODO XXXXX
--
eval db (Terms sn px n) = do
  sv <- ensureSpace db sn
  case sv of
    Left err -> return $ Left err
    Right sp -> do
      t <- getTerms sp px n 
      case t of
        Nothing -> return $ Left (-101) -- XXX refactor getTerms
        Just t' -> return $ Right (termMatch t')
--
eval db (Topo sn p d n e1) = do
  sv <- ensureSpace db sn
  case sv of
    Left err -> return $ Left err
    Right sp -> do
      v <- eval db e1
      case v of
        Left e -> return $ Left e
        Right v' -> do
          t <- getTopology sp p d n v'
          return $ Right t

--
--evalStmt :: SDMDatabase -> Stmt a -> IO (Either SDMStatus a)

