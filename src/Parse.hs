{-# LANGUAGE OverloadedStrings #-}

module Parse where
import           Text.Parsec            (alphaNum, char, digit, many, parse,
                                         skipMany, (<|>))
import           Text.Parsec.Combinator
import           Text.Parsec.String     (Parser)

data NumberOrString =
  NOSS String
  | NOSI Integer
  deriving (Eq, Show)

instance Ord NumberOrString where
  compare (NOSS _ ) (NOSI _) = GT
  compare (NOSI a ) (NOSI b) = compare a b
  compare (NOSS a ) (NOSS b) = compare a b



newtype Fields = Fields [NumberOrString]
  deriving (Eq, Show)

instance Ord Fields where
  compare (Fields []) (Fields [])    =  EQ
  compare (Fields []) (Fields b)     = GT
  compare (Fields (x:xs)) (Fields (y:ys)) = case compare x y of
    EQ -> compare xs ys
    GT -> GT
    LT -> LT

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = Fields
type Metadata = Fields

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Eq, Show)

instance Ord SemVer where
  compare (SemVer maj1 min1 pat1 rel1 met1) (SemVer maj2 min2 pat2 rel2 met2)
    | maj1 > maj2 = GT
    | min1 > min2 = GT
    | pat1 > pat2 = GT
    | rel1 > rel2 = GT
    | met1 > met2 = GT

parseNoS :: Parser NumberOrString
parseNoS = do
  (NOSI . read <$> many1 digit) <|> NOSS <$> many1 alphaNum

parseNoSs :: Parser Fields
parseNoSs = Fields <$> sepBy parseNoS (char '.')

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- read <$> many1 digit
  char '.'
  minor <- read <$> many1 digit
  char '.'
  patch <- read <$> many1 digit

  optional $ char '-'
  release <- parseNoSs

  optional $ char '+'
  meta <- parseNoSs

  return $ SemVer major minor patch release meta

testBasic = "10.1.1"

testRelease =  "10.1.1-alpha.1"
testReleaseAndMeta =  "10.1.1-alpha.1+mac"
testMeta = "10.1.1+linux.1.0.0"

test = do
  print $ parse parseSemVer "" testBasic
  print $ parse parseSemVer "" testRelease
  print $ parse parseSemVer "" testMeta
  print $ parse parseSemVer "" testReleaseAndMeta
