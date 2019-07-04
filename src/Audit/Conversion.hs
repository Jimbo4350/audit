{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Audit.Conversion
  ( audDepToPackage
  , audDepToText
  , createPackage
  , diffDepToPackage
  , diffDepToText
  , fst3
  , packageToParsedDep
  , parsedDepToAudQExpr
  , parsedDepToDiffQExpr
  , pkgThruples
  , pkgToAuditor
  , pkgToDiff
  )
where

import Audit.Database (Auditor, AuditorT(..), Diff, DiffT(..))
import Audit.Types (AnalysisStatus(..), ConversionError(..), Package(..), ParsedDependency(..))
import Data.Text (Text, pack, unpack)
import Data.Time.Clock (UTCTime)
import Data.Int (Int32)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Database.SQLite.Simple.Time.Implementation (parseUTCTime)
import Database.Beam
import Database.Beam.Sqlite.Connection (Sqlite)


audDepToPackage :: Auditor -> Either ConversionError Package
audDepToPackage diff = case audDepToText diff of
  [pId, pName, pVer, dateFirSeen, dirDep, stillUsed, aStat] ->
    case parseUTCTime dateFirSeen of
      Left  err  -> Left $ UTCTimeParseError err
      Right time -> Right $ createPackage
        (read $ unpack pId :: Int32)
        pName
        pVer
        time
        (read $ unpack dirDep)
        (read $ unpack stillUsed)
        ((read $ unpack aStat) :: [AnalysisStatus])
  _ ->
    error
      "diffDepToPackage: An additional column was added to the Diff table, please account for it here"

audDepToText :: Auditor -> [Text]
audDepToText audDep = map
  (\f -> f audDep)
  [ pack . show .  auditorDependencyId
  , auditorPackageName
  , auditorPackageVersion
  , auditorDateFirstSeen
  , auditorDirectDep
  , auditorStillUsed
  , auditorAnalysisStatus
  ]

createPackage
  :: Int32 -> Text -> Text -> UTCTime -> Bool -> Bool -> [AnalysisStatus] -> Package
createPackage pId pName pVer dateFirSeen dirDep stillUsed aStat =
  Package pId pName pVer dateFirSeen dirDep stillUsed aStat

diffDepToPackage :: Diff -> Either ConversionError Package
diffDepToPackage diff = case diffDepToText diff of
  [pId, pName, pVer, dateFirSeen, dirDep, stillUsed, aStat] ->
    case parseUTCTime dateFirSeen of
      Left  err  -> Left $ UTCTimeParseError err
      Right time -> Right $ createPackage
        (read $ unpack pId :: Int32)
        pName
        pVer
        time
        (read $ unpack dirDep)
        (read $ unpack stillUsed)
        ((read $ unpack aStat) :: [AnalysisStatus])
  _ ->
    error
      "diffDepToPackage: An additional column was added to the Diff table, please account for it here"

diffDepToText :: Diff -> [Text]
diffDepToText diffDep = map
  (\f -> f diffDep)
  [ pack . show .  diffDependencyId
  , diffPackageName
  , diffPackageVersion
  , diffDateFirstSeen
  , diffDirectDep
  , diffStillUsed
  , diffAnalysisStatus
  ]

parsedDepToAudQExpr :: ParsedDependency -> AuditorT (QExpr Sqlite s)
parsedDepToAudQExpr (ParsedDependency pName pVersion dateFS dDep sUsed aStatus) = do
  let fTime = formatTime defaultTimeLocale "%F %X%Q" dateFS
  Auditor
    default_
    (val_ pName)
    (val_ pVersion)
    (val_ (pack fTime))
    (val_ (pack $ show dDep))
    (val_ (pack $ show sUsed))
    (val_ (pack $ show aStatus))

parsedDepToDiffQExpr :: ParsedDependency -> DiffT (QExpr Sqlite s)
parsedDepToDiffQExpr (ParsedDependency pName pVersion dateFS dDep sUsed aStatus) = do
  let fTime = formatTime defaultTimeLocale "%F %X%Q" dateFS
  Diff
    default_
    (val_ pName)
    (val_ pVersion)
    (val_ (pack fTime))
    (val_ (pack $ show dDep))
    (val_ (pack $ show sUsed))
    (val_ (pack $ show aStatus))

pkgToAuditor :: Package -> Auditor
pkgToAuditor (Package pId pName pVersion dateFS dDep sUsed aStatus) = do
  let fTime = formatTime defaultTimeLocale "%F %X%Q" dateFS
  Auditor
    pId
    pName
    pVersion
    (pack fTime)
    (pack $ show dDep)
    (pack $ show sUsed)
    (pack $ show aStatus)

pkgToDiff :: Package -> Diff
pkgToDiff (Package pId pName pVersion dateFS dDep sUsed aStatus) = do
  let fTime = formatTime defaultTimeLocale "%F %X%Q" dateFS
  Diff
    pId
    pName
    pVersion
    (pack fTime)
    (pack $ show dDep)
    (pack $ show sUsed)
    (pack $ show aStatus)

-- Makes it easy to filter new packages
pkgThruples :: [Package] -> [(Text, Text, Bool)]
pkgThruples pkgs =
  map (\pkg -> (packageName pkg, packageVersion pkg, directDep pkg)) pkgs

-- Only makes sense within the context of comparing a query with a parsed result. The database has an additional
-- dependency_id column which gets set upon new dependency insertion but we still need to compare newly parsed
-- dependency tree changes with existing entries in the database.
packageToParsedDep :: Package -> ParsedDependency
packageToParsedDep pkg = ParsedDependency (packageName pkg) (packageVersion pkg) (dateFirstSeen pkg) (directDep pkg) (stillUsed pkg) (analysisStatus pkg)


fst3 :: (Text, Text, Bool) -> Text
fst3 (x, _, _) = x
