module Database.SDM.Query.Eval where

import Database.SDM.Algebra
import Database.SDM.Query.IO
import Database.SDM.Query.AST (Expr(..))

eval :: SDMDatabase -> Expr -> IO (Either SDMStatus Vec)
eval db (Symbol s n) = do
  sv <- ensureSpace db s
  case sv of
    Left err -> return $ Left err
    Right sp -> getElementalVector sp n
eval db (Or e1 e2) = undefined
eval db (And e1 e2) = undefined
eval db (Not e1) = undefined
    
    


