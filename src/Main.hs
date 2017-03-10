{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels #-}

module Main where

import Control.Monad
import Data.Char
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import Data.Tuple
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
usage = usageInfo pre argsDef <> post
  where
    pre = unlines
        [ "Utility for comparing output of 'dpkg --list' command."
        , ""
        , "Usage:"
        , "dpkg-compare {[-r|--reference] REFERENCE_FILE} {[-o|--other] OTHER_FILE}"
        , "             [--only-extra|--only-missing|--only-mismatch] [--only-installed]"
        , "dpkg-compare {-h|--help}"
        ]

    post = unlines
        [ "Output format:"
        , "<diff> <name> <version> <architecture> <status>"
        , ""
        , "  diff:"
        , "    o : package is same in both files"
        , "    + : package is only in REFERENCE_FILE"
        , "    - : package is only in OTHER_FILE"
        , ""
        , "For alignment of output into columns, pipe output into 'column -t' command."
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
  | optHelp = putStr usage
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
    putStrLn . unlines . concat . fmap showDiff . pkgFilter $ Map.elems diff
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
    mkStatus s = fromMaybe err (lookup s statusMap)
      where
        err = error $ "Unknown status: " <> [s]

    mkArch a = fromMaybe err (lookup a archMap)
      where
        err = error $ "Unknown architecture: " <> a

archMap :: [(String, Architecture)]
archMap =
    [ ("amd64",Arch64)
    , ("i386",Arch32)
    , ("all",ArchAll)
    ]

statusMap :: [(Char, Status)]
statusMap =
    [ ('n', None)
    , ('i', Installed)
    , ('c', Configured)
    , ('u', Unpacked)
    , ('f', HalfConfigured)
    , ('h', HalfInstalled)
    , ('w', TriggerAwaiting)
    , ('t', TriggerPending)
    ]

mkPkgMap :: [Package] -> Map String Package
mkPkgMap = foldl (\m pkg@Package{..} -> Map.insert pkgName pkg m) Map.empty

showPkg :: Package -> String
showPkg Package{..} = intercalate " " [pkgName, pkgVersion, arch, [status]]
  where
    arch = fromJust $ rLookup pkgArch archMap
    status = fromJust $ rLookup pkgStatus statusMap

    rLookup needle haystack = lookup needle $ fmap swap haystack

showDiff :: Difference -> [String]
showDiff = \case
    Extra p -> ["+ " <> showPkg p]
    Missing p -> ["- " <> showPkg p]
    Mismatch p q -> ["- " <> showPkg p, "+ " <> showPkg q]
    Same p -> ["o " <> showPkg p]
