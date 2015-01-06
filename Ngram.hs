{-# LANGUAGE RecordWildCards #-}

module Ngram (Markov, Sequence, markov, generate) where

import           Data.Char     (isAlpha, isSpace, toLower)
import           Data.Functor  ((<$>))
import           Data.List     (group, sort)
import qualified Data.Map      as M
import qualified System.Random as Rand
import Data.Maybe (isNothing, fromJust)

type Text = String
type Sequence = [String]
type Word = String
type Order = Int

allowedChar :: Char -> Bool
allowedChar c = any ($ c) [isAlpha, isSpace]

clean :: Text -> Text
clean = map toLower . filter allowedChar

ngrams' :: Sequence -> Sequence -> Int -> [Sequence]
ngrams' x@(p:ps) y@(w:ws) n = (x ++ [w]) : ngrams' (ps ++ [w]) ws n
ngrams' _ _ _ = []

ngrams :: Int -> Sequence -> [Sequence]
ngrams n ws
    | hasN ws n = ngrams' (take (n - 1) ws) (drop (n - 1) ws) n
    | otherwise = []
    where hasN xs m = m == length (take m xs)

count :: [Sequence] -> [(Sequence, Int)]
count = map f . group . sort where
    f gr = (head gr, length gr)

type Distribution = [(Word, Int)]
type Predictor = M.Map Sequence Distribution

buildPredictor :: [(Sequence, Int)] -> Predictor
buildPredictor = foldr go M.empty where
    go (sq, ct) m = let key = init sq
                        char = last sq in
                            case M.lookup key m of
                                Nothing -> M.insert key [(char, ct)] m
                                Just _ -> M.adjust ((char, ct):) key m

nth :: Int -> [(a, Int)] -> a
nth n ((x, ct):xs)
    | n <= ct = x
    | otherwise = nth (n - ct) xs

pick :: Distribution -> IO Word
pick dist = do
        let range =  (sum . map snd) dist
        g <- Rand.getStdGen
        n <- Rand.getStdRandom $ Rand.randomR (1, range)
        return $ nth n dist

predict :: Predictor -> Sequence -> IO (Maybe Word)
predict p sq = case M.lookup sq p of
    Nothing -> return Nothing
    Just dist -> return <$> pick dist

nextN :: Predictor -> Sequence -> Int -> IO [Word]
nextN _ _ 0 = return []
nextN p sq n = do
        nextM <- predict p sq
        if isNothing nextM
            then return []
            else do
                let next = fromJust nextM
                rest <- nextN p (tail sq ++ [next]) (n - 1)
                return (next : rest)

predictor :: Order -> Text -> Predictor
predictor n = buildPredictor . count . ngrams n . words . clean

lastN :: Int -> [a] -> [a]
lastN n xs = drop ((length xs) - n) xs

data Markov = Markov { order :: Int, pred :: !Predictor }
    deriving (Eq, Show)

markov :: Order -> Text -> Markov
markov n c = Markov n (predictor n c)

generate :: Sequence -> Int -> Markov -> IO String
generate s n Markov{..} = (unwords . (s ++)) <$> nextN pred initial n where
    initial = lastN (order - 1) s
