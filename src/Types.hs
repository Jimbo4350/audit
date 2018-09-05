module Types where


data Package = Package
    { name       :: String
    , directDeps :: [String]
    }
