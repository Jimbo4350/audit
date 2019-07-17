{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Audit.Conversion
  ( auditorEntryToParsedDep
  , auditorEntryToNotUsed
  , compareParsedWithAuditor
  , packageToParsedDep
  , parsedDepToAudQExpr
  , returnUpdatedAuditorEntry
  , updatedAuditorValues
  )
where

import           Audit.Database                 ( Auditor
                                                , AuditorT(..)
                                                )
import           Audit.Types                    ( Package(..)
                                                , ParsedDependency(..)
                                                , ConversionError(..)
                                                )

import           Data.Either                    ( rights )
import           Data.Int                       ( Int32 )
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
    Right aStat -> pure $ ParsedDependency (auditorPackageName aEnt)
                                           (auditorPackageVersion aEnt)
                                           (auditorDateFirstSeen aEnt)
                                           (auditorDirectDep aEnt)
                                           (auditorStillUsed aEnt)
                                           aStat

auditorEntryToNotUsed :: Auditor -> Auditor
auditorEntryToNotUsed aEnt = aEnt { auditorStillUsed = False }

parsedDepToAudQExpr :: ParsedDependency -> AuditorT (QExpr Sqlite s)
parsedDepToAudQExpr (ParsedDependency pName pVersion dateFS dDep sUsed aStatus)
  = Auditor default_
            (val_ pName)
            (val_ pVersion)
            (val_ dateFS)
            (val_ dDep)
            (val_ sUsed)
            (val_ (pack $ show aStatus))

parsedDepToAud :: Int32 -> ParsedDependency -> Auditor
parsedDepToAud pId (ParsedDependency pName pVersion dateFS dDep sUsed aStatus)
  = Auditor pId pName pVersion dateFS dDep sUsed (pack $ show aStatus)

-- Only makes sense within the context of comparing a query with a parsed result.
-- The database has an additional dependency_id column which gets set upon new
-- dependency insertion but we still need to compare newly parsed
-- dependency tree changes with existing entries in the database.
packageToParsedDep :: Package -> ParsedDependency
packageToParsedDep pkg = ParsedDependency (packageName pkg)
                                          (packageVersion pkg)
                                          (dateFirstSeen pkg)
                                          (directDep pkg)
                                          (stillUsed pkg)
                                          (analysisStatus pkg)

-- | Check to see if a ParsedDependency has already been added to the Auditor table.
compareParsedWithAuditor :: ParsedDependency -> Auditor -> Bool
compareParsedWithAuditor prsDep audEnt = all (== True) $ zipWith
  (==)
  [depName prsDep, depVersion prsDep, pack . show $ isDirect prsDep]
  [ auditorPackageName audEnt
  , auditorPackageVersion audEnt
  , pack . show $ auditorDirectDep audEnt
  ]

returnUpdatedAuditorEntry
  :: ParsedDependency -> Auditor -> Either ConversionError Auditor
returnUpdatedAuditorEntry pDep aud = if compareParsedWithAuditor pDep aud
  then Right $ parsedDepToAud (auditorDependencyId aud) pDep
  else Left NewlyParsedDepDoesNotExistInDb

-- | This only works for new dependencies. Any dependency that is removed
-- must rely on the Auditor table for its relevant information
updatedAuditorValues :: [Auditor] -> [ParsedDependency] -> [Auditor]
updatedAuditorValues auditorEntries parsedDeps = do
  aEnt <- auditorEntries
  pDep <- parsedDeps
  rights [returnUpdatedAuditorEntry pDep aEnt]
