{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cyrano where

import Ngram
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.Trans.Maybe

type Corpus = String

data CyranoConfig = CyranoConfig

data CyranoInfo = CyranoInfo
                    { corpora :: [Corpus]
                    , models :: M.Map Corpus Markov
                    , order :: Int
                    , len :: Int
                    }

newtype Cyrano a = Cyrano {
  runC :: ReaderT CyranoInfo (MaybeT IO) a
} deriving (Monad, MonadIO, MonadReader CyranoInfo, MonadPlus)

runCyrano :: Cyrano a -> CyranoInfo -> MaybeT IO a
runCyrano c = runReaderT (runC c)

complete :: Corpus -> Sequence -> Cyrano String
complete c s = do
  m <- asks models
  l <- asks len
  maybe mzero (liftIO . generate s l) (M.lookup c m)
