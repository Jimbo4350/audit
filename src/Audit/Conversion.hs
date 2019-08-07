{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Audit.Conversion
  ( auditorEntryToParsedDep
  , auditorEntryToNotUsed
  , compareParsedWithAuditor
  , newParsedDeps
  , parsedDepToAuditor
  , parsedDepToAudQExpr
  , returnUpdatedAuditorEntry
  , updatedAuditorValues
  )
where

import           Audit.Database                 ( Auditor
                                                , AuditorT(..)
                                                )
import           Audit.Types                    ( DirectDependency
                                                , IndirectDependency
                                                , Package(..)
                                                , ParsedDependency(..)
                                                , ConversionError(..)
                                                , DependencyName
                                                , RepoName
                                                , Version
                                                )

import           Data.Either                    ( rights )
import           Data.Int                       ( Int32 )
import           Data.Maybe                     ( fromMaybe )
import           Data.Time.Clock                ( UTCTime )
import           Data.Text                      ( pack
                                                , unpack
                                                )
import           Database.Beam
import           Database.Beam.Sqlite.Connection
                                                ( Sqlite )
import           Text.Read                      ( readEither )

auditorEntryToParsedDep :: Auditor -> Either String ParsedDependency
auditorEntryToParsedDep aEnt =
  case readEither (unpack $ auditorAnalysisStatus aEnt) of
    Left  err   -> Left err
    Right aStat -> pure $ ParsedDependency (auditorRepoName aEnt)
                                           (auditorPackageName aEnt)
                                           (auditorPackageVersion aEnt)
                                           (auditorDateFirstSeen aEnt)
                                           (auditorDirectDep aEnt)
                                           (auditorStillUsed aEnt)
                                           aStat

auditorEntryToNotUsed :: Auditor -> Auditor
auditorEntryToNotUsed aEnt = aEnt { auditorStillUsed = False }

-- | Check to see if a ParsedDependency has already been added to the Auditor table.
compareParsedWithAuditor :: ParsedDependency -> Auditor -> Bool
compareParsedWithAuditor prsDep audEnt = all (== True) $ zipWith
  (==)
  [depName prsDep, depVersion prsDep, pack . show $ isDirect prsDep]
  [ auditorPackageName audEnt
  , auditorPackageVersion audEnt
  , pack . show $ auditorDirectDep audEnt
  ]

-- | Build package/dependency list for the initialization of
-- the database.
newParsedDeps
  :: RepoName
  -> [(DependencyName, Version)]
  -> [DirectDependency]
  -> [IndirectDependency]
  -> UTCTime
  -> [ParsedDependency]
newParsedDeps rName pVersions dDeps indirDeps currTime = do

  let directPackages   = map (directPkg currTime) dDeps
  let indirectPackages = map (indirectPkg currTime) indirDeps

  directPackages ++ indirectPackages
 where
  lookupVersion x = pack $ fromMaybe "No version Found" (lookup x pVersions)
  directPkg time x =
    ParsedDependency (pack rName) (pack x) (lookupVersion x) time True True []
  indirectPkg time x =
    ParsedDependency (pack rName) (pack x) (lookupVersion x) time False True []

-- | Used for testing purposes.
parsedDepToAuditor :: ParsedDependency -> Auditor
parsedDepToAuditor (ParsedDependency repoName depName depVersion firstSeen isDirect inUse aStatus)
  = Auditor 0
            repoName
            depName
            depVersion
            firstSeen
            isDirect
            inUse
            (pack $ show aStatus)

parsedDepToAudQExpr :: ParsedDependency -> AuditorT (QExpr Sqlite s)
parsedDepToAudQExpr (ParsedDependency pRepoName pName pVersion dateFS dDep sUsed aStatus)
  = Auditor default_
            (val_ pRepoName)
            (val_ pName)
            (val_ pVersion)
            (val_ dateFS)
            (val_ dDep)
            (val_ sUsed)
            (val_ (pack $ show aStatus))

parsedDepToAud :: Int32 -> ParsedDependency -> Auditor
parsedDepToAud pId (ParsedDependency pRepoName pName pVersion dateFS dDep sUsed aStatus)
  = Auditor pId pRepoName pName pVersion dateFS dDep sUsed (pack $ show aStatus)

returnUpdatedAuditorEntry
  :: ParsedDependency -> Auditor -> Either ConversionError Auditor
returnUpdatedAuditorEntry pDep aud = if compareParsedWithAuditor pDep aud
  then Right $ parsedDepToAud (auditorDependencyId aud) pDep
  else Left $ NewlyParsedDepDoesNotExistInDb pDep

-- | This only works for new dependencies. Any dependency that is removed
-- must rely on the Auditor table for its relevant information
updatedAuditorValues :: [Auditor] -> [ParsedDependency] -> [Auditor]
updatedAuditorValues auditorEntries parsedDeps = do
  aEnt <- auditorEntries
  pDep <- parsedDeps
  rights [returnUpdatedAuditorEntry pDep aEnt]
