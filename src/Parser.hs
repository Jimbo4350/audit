module Parser
       ( allDependencies
       , digraphRemoval
       , getCommand
       , mainPackage
       , packageName'
       , rankRemoval
       , shapeRemoval
       , stringParse
       , styleRemoval
       , versions
       ) where

import           Data.List           (concat, intercalate)
import           Data.Semigroup      ((<>))
import           Options.Applicative (Parser, help, long, metavar, strOption)
import           Text.Parsec
import           Types               (Command (..), PackageName, Version)

-- | Returns name of the repo.
packageName' :: Parsec String () String
packageName' = do
    _ <- digraphRemoval
    concat <$> mainPackage <* styleRemoval

-- | Returns all dependencies.
allDependencies :: Parsec String () [(String,String)]
allDependencies = do
    _ <- digraphRemoval
    _ <- many (mainPackage <* styleRemoval)
    _ <- (try shapeRemoval <|> rankRemoval) `sepEndBy` newline
    directDependancies `sepEndBy` newline

-- | Returns all package versions.
versions :: Parsec String () [(PackageName, Version)]
versions = dependencyVersion `sepEndBy` newline

------------------------------Helper Parsers-----------------------------------

-- | Returns (package, dependency)
directDependancies :: Parsec String () (String,String)
directDependancies = do
    package <- stringParse
    _ <- space *> string "->" *> space
    dependency <- stringParse
    _ <- char ';'
    return $ (package, dependency)

-- | Removes "strict digraph deps {"
digraphRemoval :: Parsec String () [String]
digraphRemoval =
    concat <$> manyTill (many alphaNum `sepBy` space) (char '{') <* many space

-- | Parses the package on the left hand side of " ->"
mainPackage :: Parsec String () [String]
mainPackage =
    between
        (char '"')
        (char '"')
        (many alphaNum `sepBy` char '-') <* many space

-- | Removes "[style=dashed]; "
styleRemoval :: Parsec String () ()
styleRemoval = do
    _ <- between
             (char '[')
             (char ']')
             (many alphaNum >> char '=' >> many alphaNum)
    _ <- char ';'
    _ <- many space
    pure ()

-- | Removes "{rank=max; "String"; };"
rankRemoval :: Parsec String () ()
rankRemoval = do
    _ <- between (char '{')
                 (char '}')
                 (string "rank=max;"
                     *> space
                     *> stringParse
                     *> char ';'
                     *> space
                 )
    _ <- char ';'
    pure ()

-- | Removes "{rank=max; "String" [shape=box]; };"
shapeRemoval :: Parsec String () ()
shapeRemoval = do
    _ <- between (char '{')
                 (char '}')
                 (string "rank=max;"
                     *> space
                     *> shapeRemoval'
                     *> char ';'
                     *> space
                 )
    _ <- char ';'
    pure ()

shapeRemoval' :: Parsec String () String
shapeRemoval' = do
    _ <- stringParse
    _ <- space
    between
        (char '[')
        (char ']')
        (many alphaNum *> char '=' *> many alphaNum)

-- | Parses a string
stringParse :: Parsec String () String
stringParse =
    between
        (char '"')
        (char '"')
        (try (intercalate "-" <$> many alphaNum `sepBy` char '-')
            <|> (many alphaNum))

dependencyVersion :: Parsec String () (String,String)
dependencyVersion = do
    pName <- (intercalate "-") <$> many alphaNum `sepBy` char '-'
    _ <- space
    version <- intercalate "." <$> many digit `sepBy` char '.'
    return (pName,version)

------------------------------Optparse Parsers-----------------------------------

getCommand :: Parser Command
getCommand = Command
    <$> strOption
        ( long "audit"
       <> metavar "TARGET"
       <> help "Enter currentstate or updatecurrentstate"
        )
