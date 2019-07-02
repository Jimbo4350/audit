module Audit.Conversion
  ( audDepToPackage
  , audDepToText
  , createPackage
  , diffDepToPackage
  , diffDepToText
  , pkgToAuditor
  , pkgToDiff
  )
where

import Audit.Database (Auditor, AuditorT(..), Diff, DiffT(..))
import Audit.Types (AnalysisStatus(..), ConversionError(..), Package(..))
import Data.Text (Text, pack, unpack)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Database.SQLite.Simple.Time.Implementation (parseUTCTime)

audDepToPackage :: Auditor -> Either ConversionError Package
audDepToPackage diff = case audDepToText diff of
  [pName, pVer, dateFirSeen, dirDep, stillUsed, aStat] -> do
    case (parseUTCTime dateFirSeen) of
      Left  err  -> Left $ UTCTimeParseError err
      Right time -> Right $ createPackage
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
  [ auditorPackageName
  , auditorPackageVersion
  , auditorDateFirstSeen
  , auditorDirectDep
  , auditorStillUsed
  , auditorAnalysisStatus
  ]

createPackage
  :: Text -> Text -> UTCTime -> Bool -> Bool -> [AnalysisStatus] -> Package
createPackage pName pVer dateFirSeen dirDep stillUsed aStat =
  Package pName pVer dateFirSeen dirDep stillUsed aStat

diffDepToPackage :: Diff -> Either ConversionError Package
diffDepToPackage diff = case diffDepToText diff of
  [pName, pVer, dateFirSeen, dirDep, stillUsed, aStat] -> do
    case (parseUTCTime dateFirSeen) of
      Left  err  -> Left $ UTCTimeParseError err
      Right time -> Right $ createPackage
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
  [ diffPackageName
  , diffPackageVersion
  , diffDateFirstSeen
  , diffDirectDep
  , diffStillUsed
  , diffAnalysisStatus
  ]

pkgToAuditor :: Package -> Auditor
pkgToAuditor (Package pName pVersion dateFS dDep sUsed aStatus) = do
  let fTime = formatTime defaultTimeLocale "%F %X%Q" dateFS
  Auditor
    pName
    pVersion
    (pack fTime)
    (pack $ show dDep)
    (pack $ show sUsed)
    (pack $ show aStatus)

pkgToDiff :: Package -> Diff
pkgToDiff (Package pName pVersion dateFS dDep sUsed aStatus) = do
  let fTime = formatTime defaultTimeLocale "%F %X%Q" dateFS
  Diff
    pName
    pVersion
    (pack fTime)
    (pack $ show dDep)
    (pack $ show sUsed)
    (pack $ show aStatus)
