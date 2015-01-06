{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cyrano ( CyranoInfo (..)
              , CyranoConfig (..)
              , cyrano
) where

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

-- runCyrano (complete "kjv" "i am thy") info
runCyrano :: Cyrano a -> CyranoInfo -> MaybeT IO a
runCyrano c = runReaderT (runC c)

-- e.g., `complete "kjv" "i am thy"
complete :: Corpus -> String -> Cyrano String
complete c t = do
  m <- asks models
  l <- asks len
  maybe mzero (liftIO . generate t l) (M.lookup c m)

cyrano :: CyranoInfo -> Corpus -> String -> MaybeT IO String
cyrano i c t = runCyrano (complete c t) i
