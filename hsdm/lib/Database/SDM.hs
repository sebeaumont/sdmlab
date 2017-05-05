-- export all the SDM types
module Database.SDM (SDMDatabase,
                     SDMDataWord,
                     SDMVectorIdx,
                     sdmDataEls,
                     SDMPoint,
                     symbol,
                     metric,
                     density,
                     SDMCard,
                     LevelSet,
                     TermMatch,
                     termMatch, -- TermMatchReply -> TermMatch
                     prefix,
                     matches,
                     terms,
                     Term,
                     name,
                     rho
                    ) where

import Database.SDM.Internal.SDMLIB
import Database.SDM.Internal.Decode

type LevelSet = ([SDMPoint], SDMCard)
