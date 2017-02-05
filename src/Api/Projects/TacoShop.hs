{-# LANGUAGE OverloadedStrings #-}

module Api.Projects.TacoShop
  ( NameType(..)
  , makeBertosName
  ) where

import           Control.Monad.State.Lazy
import           Data.Monoid
import qualified Data.Text                as T
import           System.Random


data NameType = RandomCharacter
              | RNN
              | HMM
              deriving (Show, Eq)

vowels :: [T.Text]
vowels = ["a", "e", "i", "o", "u"]

consonants :: [T.Text]
consonants = ["b", "c", "d", "f", "g", "h", "j", "l", "ll", "m", "n",
              "ñ", "p", "q", "r", "s", "t", "v", "w", "x", "y", "z"]

getRandChar' :: [T.Text] -> IO T.Text
getRandChar' xs = do
  x <- getStdRandom $ randomR (0, length xs - 1)
  return (xs !! x)

buildVals :: Int -> [Int]
buildVals 0 = 1 : buildVals 1
buildVals 1 = 0 : buildVals 0

letterGetter :: Int -> IO T.Text
letterGetter 0 = getRandChar' vowels
letterGetter 1 = getRandChar' consonants

makeRandWord :: [Int] -> IO T.Text
makeRandWord ns = T.concat <$> mapM letterGetter ns

makeBertosName :: Int -> IO T.Text
makeBertosName len = do
  g <- getStdGen
  let total = len - 6
      first = fst $ randomR (0, 1) g
      randomizer = take total $ buildVals first
      newWord = makeRandWord randomizer
  newWord <> pure "bertos"
