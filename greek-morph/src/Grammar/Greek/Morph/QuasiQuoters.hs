{-# LANGUAGE TemplateHaskell #-}

module Grammar.Greek.Morph.QuasiQuoters where

import Prelude hiding (Word)
import Control.Applicative
import Control.Lens (over, _1, _2)
import Data.Data
import Data.Either.Validation
import Data.Generics (extQ)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Grammar.Common
import Grammar.Common.Decompose
import Grammar.Greek.Script.Types
import Grammar.Greek.Script.Word
import Grammar.Greek.Morph.Paradigm.Ending
import Grammar.Greek.Morph.Paradigm.Types
import Grammar.Greek.Morph.Types
import qualified Grammar.Greek.Script.Stage as Stage

handleText :: Text -> Maybe ExpQ
handleText x = Just $ appE (varE 'Text.pack) $ litE $ StringL $ Text.unpack x

trim :: String -> String
trim = Text.unpack . Text.strip . Text.pack

parseWordPairMap :: (String -> String) -> String -> Validation String (String :* Word)
parseWordPairMap f x = case (roundTo Stage.script) [x :^ (trim . f . decompose) x :^ NoWordPunctuation] of
  Failure es -> Failure $ concatMap (\(c, e) -> c ++ " -- " ++ show e ++ "\n") es
  Success [(s, w)] -> Success $ s :^ w
  Success w -> Failure $ "Unexpected word: " ++ show w

parseStar :: String -> Validation String (Maybe a)
parseStar "*" = Success Nothing
parseStar x = Failure $ "not a star: " ++ x

macronChar :: Char
macronChar = '\772'

parseWordPairs :: String -> Validation String [(String :* Word)]
parseWordPairs = traverse (parseWordPairMap ignoreMacron) . words
  where
  ignoreMacron = filter (/= macronChar)

ignoreParadigmChars :: String -> String
ignoreParadigmChars = filter (flip Set.notMember ignoreChars)
  where
  ignoreChars = Set.fromList
    [ macronChar
    , '-'
    , '('
    , ')'
    ]

parseParadigmExemplars :: String -> Validation String [Maybe ParadigmExemplar]
parseParadigmExemplars = traverse (\x -> parseStar x <|> doWord x) . words
  where
  doWord
    = over _Success (pure . uncurry ParadigmExemplar)
    . over (_Success . _1) Text.pack
    . over (_Success . _2) wordToAccentedWord
    . parseWordPairMap ignoreParadigmChars

wordPairsExp :: Data a => (String :* Word -> a) -> String -> Q Exp
wordPairsExp f x = case parseWordPairs x of
  Failure e -> fail e
  Success ws -> dataToExpQ (const Nothing `extQ` handleText) $ fmap f ws

nounParadigmParser :: String -> Q Exp
nounParadigmParser x = case parseParadigmExemplars x of
  Failure e -> fail e
  Success [a,b,c,d,e, f,g, h,i,j,k] -> case getParadigmEndings . nounParadigmForms $ NounParadigm a b c d e f g h i j k of
    Left err -> fail . show $ err
    Right r -> dataToExpQ (const Nothing `extQ` handleText) r
  Success ws -> fail $ "Incorrect number of noun forms: " ++ show (length ws)

verbParadigmParser :: String -> Q Exp
verbParadigmParser x = case parseParadigmExemplars x of
  Failure e -> fail e
  Success [a,b,c, d,e, f,g,h] -> case getParadigmEndings . verbParadigmForms $ VerbParadigm a b c d e f g h of
    Left err -> fail . show $ err
    Right r -> dataToExpQ (const Nothing `extQ` handleText) r
  Success ws -> fail $ "Incorrect number of verb forms: " ++ show (length ws)

coreWords :: QuasiQuoter
coreWords = QuasiQuoter
  { quoteExp = wordPairsExp (wordToCoreWord . snd)
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

coreWordPairs :: QuasiQuoter
coreWordPairs = QuasiQuoter
  { quoteExp = wordPairsExp (over _2 wordToCoreWord)
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

accentedWords :: QuasiQuoter
accentedWords = QuasiQuoter
  { quoteExp = wordPairsExp (wordToAccentedWord . snd)
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

nounParadigm :: QuasiQuoter
nounParadigm = QuasiQuoter
  { quoteExp = nounParadigmParser
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

verbParadigm :: QuasiQuoter
verbParadigm = QuasiQuoter
  { quoteExp = verbParadigmParser
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }
