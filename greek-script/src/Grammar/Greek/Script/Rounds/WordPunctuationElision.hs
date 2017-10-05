module Grammar.Greek.Script.Rounds.WordPunctuationElision where

import Data.Either
import Data.Either.Validation
import Grammar.Common.Round
import Grammar.Common.Types
import Grammar.Greek.Script.Types

data InvalidWordPunctuation
  = InvalidWordPunctuation [WordPunctuation]
  | EmptyWord
  deriving (Show)

wordPunctuationElision :: RoundFwd InvalidWordPunctuation [a :+ WordPunctuation] ([a] :* Elision)
wordPunctuationElision = makeRoundFwd to from
  where
  to xs = case xs of
    [] -> Failure EmptyWord
    (Right P_RightQuote : xs') -> case rights xs' of
      [] -> Success (lefts xs', Aphaeresis)
      ps@(_ : _) -> Failure $ InvalidWordPunctuation ps
    (_ : _) -> case reverse xs of
      [] -> Failure EmptyWord
      (Right P_RightQuote : xs') -> case rights xs' of
        [] -> Success (reverse . lefts $ xs', IsElided)
        ps@(_ : _) -> Failure $ InvalidWordPunctuation (reverse ps)
      xs'@(_ : _) -> case rights xs' of
        [] -> Success (reverse . lefts $ xs', NotElided)
        ps@(_ : _) -> Failure $ InvalidWordPunctuation (reverse ps)

  from (xs, IsElided) = fmap Left xs ++ [Right P_RightQuote]
  from (xs, Aphaeresis) = Right P_RightQuote : fmap Left xs
  from (xs, NotElided) = fmap Left xs
