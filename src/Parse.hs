module Parse where
import           Text.Parsec            (alphaNum, char, digit, endOfLine, many,
                                         parse, skipMany, (<|>))
import           Text.Parsec.Combinator
import           Text.Parsec.String     (Parser)

data NumberOrString =
  NOSS String
  | NOSI Integer
  deriving (Eq, Show)

instance Ord NumberOrString where
  compare (NOSS a ) (NOSI b) = GT
  compare (NOSI a ) (NOSS b) = LT
  compare (NOSI a ) (NOSI b) = compare a b
  compare (NOSS a ) (NOSS b) = compare a b



newtype Fields = Fields [NumberOrString]
  deriving (Eq, Show)

instance Ord Fields where
  compare (Fields []) (Fields [])    =  EQ
  compare (Fields []) (Fields _)     = GT
  compare (Fields _) (Fields [])     = LT
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
  compare (SemVer maj1 min1 pat1 rel1 _meta1) (SemVer maj2 min2 pat2 rel2 _meta2)
    | maj1 > maj2 = GT
    | maj1 < maj2 = LT
    | min1 > min2 = GT
    | min1 < min2 = LT
    | pat1 > pat2 = GT
    | pat1 < pat2 = LT
    | rel1 > rel2 = GT
    | rel1 < rel2 = LT
    | otherwise = EQ

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

  eof

  return $ SemVer major minor patch release meta
