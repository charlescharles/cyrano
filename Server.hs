module Server where

import Cyrano
import Ngram (Markov, markov, generate)
import qualified Data.Map as M
import Control.Monad.Trans.Maybe

corp = [("kjv", "kjv1.txt")]

printCyrano :: MaybeT IO String -> IO ()
printCyrano k = runMaybeT k >>= maybe (return ()) putStrLn

main :: Int -> Int -> String -> IO ()
main o n start = do
  kjv <- readFile "kjv1.txt"
  let m = markov o kjv
  let corpMap = M.fromList [("kjv", m)]
  let info = CyranoInfo ["kjv"] corpMap o n
  let kjv = cyrano info "kjv"
  printCyrano (kjv start)
