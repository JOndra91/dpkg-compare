{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels #-}

module Main where

import Control.Monad
import Data.Char
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import Data.Monoid
import qualified Data.Set as Set
import System.Console.GetOpt
import System.Environment
import System.Exit
import Text.ParserCombinators.ReadP

data ProgOpts = ProgOpts
    { optReferenceFile :: String
    , optOtherFile :: String
    , optFilter :: OutputFilter
    , optStatus :: OutputStatus
    , optHelp :: Bool
    }
  deriving (Show)

data OutputFilter
    = FilterAll
    | FilterOnlyExtra
    | FilterOnlyMissing
    | FilterOnlyMismatch
  deriving (Show)

data OutputStatus
    = StatusAll
    | StatusOnlyInstalled
  deriving (Show)

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
main = getArgs >>= \args -> case getOpt Permute argsDef args of
    (opts, fs, []) -> doStuff . applyNonOpts fs $ (foldl (.) id opts) defOpts
    (_, _, err:_) -> die $ err <> usage
  where
    defOpts = ProgOpts "" "" FilterAll StatusAll False

    applyNonOpts fs opts@ProgOpts{..} = case fs of
        [f1,f2] -> opts
            { optReferenceFile = f1
            , optOtherFile = f2
            }
        _ -> error "Invalid arguments."

usage :: String
usage = usageInfo str argsDef
  where
    str = unlines
        [ "Utility for comparing output of 'dpkg --list' command."
        , ""
        , "Usage:"
        , "dpkg-compare {[-r|--reference] REFERENCE_FILE} {[-o|--other] OTHER_FILE}"
        , "             [--only-extra|--only-missing|--only-mismatch] [--only-installed]"
        , "dpkg-compare {-h|--help}"
        ]

argsDef :: [OptDescr (ProgOpts -> ProgOpts)]
argsDef =
    [ Option ['h'] ["help"]
        (NoArg (\opts -> opts { optHelp = True }))
        "Print this help."
    , Option ['r'] ["reference"]
        (ReqArg (\s opts -> opts { optReferenceFile = s }) "REFERENCE_FILE")
        "Path to file that's result of 'dpkg --list' command."
    , Option ['o'] ["other"]
        (ReqArg (\s opts -> opts { optOtherFile = s }) "OTHER_FILE")
        "Path to file that's result of 'dpkg --list' command."
    , Option [] ["only-extra"]
        (NoArg (\opts -> opts { optFilter = FilterOnlyExtra }))
        "Print only packages found in REFERENCE_FILE but not in OTHER_FILE."
    , Option [] ["only-missing"]
        (NoArg (\opts -> opts { optFilter = FilterOnlyMissing }))
        "Print only packages found in OTHER_FILE but not in REFERENCE_FILE."
    , Option [] ["only-mismatch"]
        (NoArg (\opts -> opts { optFilter = FilterOnlyMismatch }))
        "Print only packages found in both files but not with same version/architecture/status."
    , Option [] ["only-installed"]
        (NoArg (\opts -> opts { optStatus = StatusOnlyInstalled }))
        "Print only packages with 'installed' status."
    ]

doStuff :: ProgOpts -> IO ()
doStuff ProgOpts{..}
  | optHelp = putStrLn usage
  | otherwise = do
    when (null optReferenceFile) $ die "REFERENCE_FILE not specified."
    when (null optOtherFile) $ die "OTHER_FILE not specified."
    referenceMap <- loadFile optReferenceFile
    otherMap <- loadFile optOtherFile
    let diff = Map.merge
          (Map.mapMissing $ (Extra .) . (flip const))
          (Map.mapMissing $ (Missing .) . (flip const))
          (Map.zipWithMatched matchVersion)
          referenceMap otherMap
    putStrLn . unlines . fmap show . pkgFilter $ Map.elems diff
  where
    loadFile = fmap (mkPkgMap . pkgStatus . parseFile) . readFile

    matchVersion _ left right
        | left == right = Same left
        | otherwise = Mismatch left right

    pkgStatus :: [Package] -> [Package]
    pkgStatus = case optStatus of
        StatusAll -> id
        StatusOnlyInstalled -> filter isInstalled

    isInstalled = \case
        Package _ _ _ Installed -> True
        _ -> False

    pkgFilter :: [Difference] -> [Difference]
    pkgFilter = case optFilter of
        FilterAll -> id
        FilterOnlyExtra -> filter isExtra
        FilterOnlyMissing -> filter isMissing
        FilterOnlyMismatch -> filter isMismatch

    isMissing = \case
        Missing _ -> True
        _ -> False

    isExtra = \case
        Extra _ -> True
        _ -> False

    isMismatch = \case
        Mismatch _ _ -> True
        _ -> False

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
