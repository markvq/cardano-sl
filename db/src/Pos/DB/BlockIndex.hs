-- | Operations with block index db.

module Pos.DB.BlockIndex
       ( getHeader
       , getTipHeader
       , putHeadersIndex
       , deleteHeaderIndex
       ) where

import           Universum

import qualified Database.RocksDB as Rocks

import           Pos.Binary.Class (serialize')
import           Pos.Chain.Block (BlockHeader (..), HeaderHash,
                     genHeaderPrevBlock, headerHash)
import           Pos.Chain.Update (ConsensusEra (..))
import           Pos.DB.Class (DBTag (BlockIndexDB), MonadBlockDBRead,
                     MonadDB (..))
import           Pos.DB.Error (DBError (DBMalformed))
import           Pos.DB.Functions (dbGetBi)
import           Pos.DB.GState.Tip (getTipSomething)
import           Pos.DB.Update.GState.BlockVersion (blockIndexKey,
                     getConsensusEra)
import           Pos.Util.Util (maybeThrow)

-- | Returns header of block that was requested from Block DB.
getHeader
    :: (MonadBlockDBRead m)
    => HeaderHash -> m (Maybe BlockHeader)
getHeader = dbGetBi BlockIndexDB . blockIndexKey

-- | Get 'BlockHeader' corresponding to tip.
getTipHeader :: MonadBlockDBRead m => m BlockHeader
getTipHeader = do
    era <- getConsensusEra
    tipHeader <- getTipHeaderFromDb
    case era of
        Original -> pure tipHeader
        OBFT _   -> case tipHeader of
            BlockHeaderMain _      -> pure tipHeader
            BlockHeaderGenesis gbh -> do
                let prevBlockHash :: HeaderHash
                    prevBlockHash = gbh ^. genHeaderPrevBlock
                maybeThrow (DBMalformed $ "sup") =<< getHeader prevBlockHash
  where
    getTipHeaderFromDb :: MonadBlockDBRead m => m BlockHeader
    getTipHeaderFromDb = getTipSomething "header" getHeader

-- | Writes batch of headers into the block index db.
putHeadersIndex :: (MonadDB m) => [BlockHeader] -> m ()
putHeadersIndex =
    dbWriteBatch BlockIndexDB .
    map (\h -> Rocks.Put (blockIndexKey $ headerHash h) (serialize' h))

-- | Deletes header from the index db.
deleteHeaderIndex :: MonadDB m => HeaderHash -> m ()
deleteHeaderIndex = dbDelete BlockIndexDB . blockIndexKey
