{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Audit.Conversion
  ( compareParsedWithAuditor
  , packageToParsedDep
  , parsedDepToAudQExpr
  )
where

import Audit.Database (Auditor, AuditorT(..))
import Audit.Types (Package(..), ParsedDependency(..))
import Data.Text (Text, pack)
import Database.Beam
import Database.Beam.Sqlite.Connection (Sqlite)


parsedDepToAudQExpr :: ParsedDependency -> AuditorT (QExpr Sqlite s)
parsedDepToAudQExpr (ParsedDependency pName pVersion dateFS dDep sUsed aStatus) =
  Auditor
    default_
    (val_ pName)
    (val_ pVersion)
    (val_ dateFS)
    (val_ dDep)
    (val_ sUsed)
    (val_ (pack $ show aStatus))

-- Only makes sense within the context of comparing a query with a parsed result. The database has an additional
-- dependency_id column which gets set upon new dependency insertion but we still need to compare newly parsed
-- dependency tree changes with existing entries in the database.
packageToParsedDep :: Package -> ParsedDependency
packageToParsedDep pkg = ParsedDependency (packageName pkg) (packageVersion pkg) (dateFirstSeen pkg) (directDep pkg) (stillUsed pkg) (analysisStatus pkg)

-- | Check to see if a ParsedDependency has already been added to the Auditor table.
compareParsedWithAuditor :: ParsedDependency -> Auditor -> Bool
compareParsedWithAuditor prsDep audEnt =
  all (== True) $
    zipWith (==)
    [ depName prsDep
    , depVersion prsDep
    , pack . show $ isDirect prsDep
    ]
    [ auditorPackageName audEnt
    , auditorPackageVersion audEnt
    , pack . show $ auditorDirectDep audEnt
    ]
