module Parser
       ( digraphRemoval
       , mainPackage
       , mainPackages
       , rankRemoval
       , shapeRemoval
       , stringParse
       , styleRemoval
       , versions
       ) where

import           Data.List   (concat, intercalate)
import           Text.Parsec

mainPackages :: Parsec String () [(String,String)]
mainPackages = do
    _ <- digraphRemoval
    _ <- many (mainPackage <* styleRemoval)
    _ <- (try shapeRemoval <|> rankRemoval) `sepEndBy` newline
    directDependancies `sepEndBy` newline


versions :: Parsec String () [(String,String)]
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
        (many alphaNum `sepBy` char ('-')) <* many space

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
    pName <- concat <$> many alphaNum `sepBy` char '-'
    _ <- space
    version <- intercalate "." <$> many digit `sepBy` char '.'
    return (pName,version)
