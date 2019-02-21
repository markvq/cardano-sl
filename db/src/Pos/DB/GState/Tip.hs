module Pos.DB.GState.Tip
       (
         -- * Getters
         getTip
       , getTipSomething

         -- * Putters
       , putTip

         -- * Keys
       , tipKey
       ) where

import           Universum

import           Formatting (sformat, stext, (%))

import           Pos.Chain.Block (BlockHeader (..), HeaderHash,
                     genHeaderPrevBlock)
import           Pos.Chain.Update (ConsensusEra (..))
import           Pos.DB.Class (DBTag (..), MonadBlockDBRead, MonadDB,
                     MonadDBRead (..))
import           Pos.DB.Error (DBError (DBMalformed))
import           Pos.DB.Functions (dbGetBi)
import           Pos.DB.GState.Common (gsGetBi, gsPutBi)
import           Pos.DB.Update.GState.BlockVersion (blockIndexKey,
                     getConsensusEra)
import           Pos.Util.Util (maybeThrow)


----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

-- | Get current tip from GState DB.
getTip :: MonadDBRead m => m HeaderHash
getTip = do
    era <- getConsensusEra
    tipHash <- getTipFromDb
    tipHeader <- maybe (error "ok") pure =<< getHeader tipHash
    case era of
        Original -> pure tipHash
        OBFT _   -> case tipHeader of
            BlockHeaderMain _      -> pure tipHash
            BlockHeaderGenesis gbh -> do
                let prevBlockHash :: HeaderHash
                    prevBlockHash = gbh ^. genHeaderPrevBlock
                prevBlockHeader <- getHeader prevBlockHash
                case prevBlockHeader of
                    Nothing -> throwM (DBMalformed $ "sup")
                    Just _  -> pure prevBlockHash
  where
    getTipFromDb :: MonadDBRead m => m HeaderHash
    getTipFromDb = maybeThrow (DBMalformed "no tip in GState DB") =<< getTipMaybe
    --
    getHeader
        :: (MonadBlockDBRead m)
        => HeaderHash -> m (Maybe BlockHeader)
    getHeader = dbGetBi BlockIndexDB . blockIndexKey

getTipSomething
    :: forall m smth.
       MonadDBRead m
    => Text -> (HeaderHash -> m (Maybe smth)) -> m smth
getTipSomething smthDescription smthGetter =
    maybe onFailure pure =<< smthGetter =<< getTip
  where
    fmt = "there is no "%stext%" corresponding to tip"
    onFailure = throwM $ DBMalformed $ sformat fmt smthDescription

getTipMaybe :: MonadDBRead m => m (Maybe HeaderHash)
getTipMaybe = gsGetBi tipKey

----------------------------------------------------------------------------
-- Putters
----------------------------------------------------------------------------

putTip :: MonadDB m => HeaderHash -> m ()
putTip = gsPutBi tipKey

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

tipKey :: ByteString
tipKey = "c/tip"
