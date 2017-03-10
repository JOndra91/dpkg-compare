{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Data.Char
import Data.Monoid
import System.Environment
import Text.ParserCombinators.ReadP
import Debug.Trace
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Set as Set

data Status
    = None
    | Installed
    | Configured
    | Unpacked
    | HalfConfigured
    | HalfInstalled
    | TriggerAwaiting
    | TriggerPending
  deriving (Show, Eq)

data Architecture
    = ArchAll
    | Arch64
    | Arch32
  deriving (Show, Eq)

data Package = Package
    { pkgName :: String
    , pkgVersion :: String
    , pkgArch :: Architecture
    , pkgStatus :: Status
    }
  deriving (Show, Eq)

data Difference
    = Extra Package
    | Missing Package
    | Mismatch Package Package
    | Same Package
  deriving (Show)

main :: IO ()
main = getArgs >>= \case
    [f1, f2] -> do
        referenceMap <- mkPkgMap . parseFile <$> readFile f1
        otherMap <- mkPkgMap . parseFile <$> readFile f2
        let diff = Map.merge
              (Map.mapMissing $ (Extra .) . (flip const))
              (Map.mapMissing $ (Missing .) . (flip const))
              (Map.zipWithMatched matchVersion)
              referenceMap otherMap
        putStrLn . unlines $ show <$> Map.elems diff
  where
    matchVersion _ left right
        | left == right = Same left
        | otherwise = Mismatch left right

parseFile :: String -> [Package]
parseFile = fmap parsePkg . drop 5 . lines

parsePkg :: String -> Package
parsePkg = fst . head . readP_to_S pkgParser

pkgParser :: ReadP Package
pkgParser = do
    void $ get -- desired
    pkgStatus <- mkStatus <$> get
    void $ get -- err
    skipSpaces
    pkgName <- munch1 (not . isSpace)
    skipSpaces
    pkgVersion <- munch1 (not . isSpace)
    skipSpaces
    pkgArch <- mkArch <$> munch1 (not . isSpace)
    pure Package{..}
  where
    mkStatus = \case
        'n' -> None
        'i' -> Installed
        'c' -> Configured
        'u' -> Unpacked
        'f' -> HalfConfigured
        'h' -> HalfInstalled
        'w' -> TriggerAwaiting
        't' -> TriggerPending
        s -> error $ "Unexpected status: " <> [s]

    mkArch = \case
        "amd64" -> Arch64
        "i386" -> Arch32
        "all" -> ArchAll
        a -> error $ "Unexpected architecture: " <> a

mkPkgMap :: [Package] -> Map String Package
mkPkgMap = foldl (\m pkg@Package{..} -> Map.insert pkgName pkg m) Map.empty
