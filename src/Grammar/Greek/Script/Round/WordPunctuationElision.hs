module Grammar.Greek.Script.Round.WordPunctuationElision where

import Data.Either
import Data.Either.Validation
import Data.Void
import Grammar.Round
import Grammar.CommonTypes
import Grammar.Greek.Script.Types

data InvalidWordPunctuation
  = InvalidWordPunctuation [WordPunctuation]
  | EmptyWord
  deriving (Show)

wordPunctuationElision :: Round InvalidWordPunctuation Void [a :+ WordPunctuation] ([a] :* Elision)
wordPunctuationElision = makeToValidationRound to from
  where
  to xs = case reverse xs of
    [] -> Failure EmptyWord
    (Right P_RightQuote : xs') -> case rights xs' of
      [] -> Success (reverse . lefts $ xs', IsElided)
      ps@(_ : _) -> Failure $ InvalidWordPunctuation (reverse ps)
    xs'@(_ : _) -> case rights xs' of
      [] -> Success (reverse . lefts $ xs', NotElided)
      ps@(_ : _) -> Failure $ InvalidWordPunctuation (reverse ps)

  from (xs, IsElided) = fmap Left xs ++ [Right P_RightQuote]
  from (xs, NotElided) = fmap Left xs
