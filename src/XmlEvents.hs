{-# LANGUAGE LambdaCase #-}

module XmlEvents where

import Conduit
import Control.Exception (Exception (..), SomeException)
import Control.Monad (when)
import Control.Monad.Trans.Resource
import qualified Data.Char as Char
import Data.Conduit.Attoparsec (PositionRange)
import qualified Data.Conduit.Internal as CI
import qualified Data.Conduit.List as CL
import qualified Data.Text as Text
import qualified Data.Text as T
import Data.XML.Types
import Text.XML.Stream.Parse (def, ParseSettings)
import qualified Text.XML.Stream.Parse as P
import Text.XML.Unresolved

readEvents :: FilePath -> IO [P.EventPos]
readEvents path = runResourceT $ sourceFile path =$= P.parseBytesPos def $$ sinkList

readDocument :: FilePath -> IO Document
readDocument path = runResourceT $ sourceFile path =$= P.parseBytesPos def $$ ourFromEvents

manyTries :: Monad m => m (Maybe a) -> m [a]
manyTries f = go id
  where
    go g = f >>= \case
      Nothing -> return $ g []
      Just y -> go $ g . (y :)

dropReturn :: Monad m => a -> ConduitM i o m a
dropReturn x = CL.drop 1 >> return x

miscellaneous :: Monad m => ConduitM (t, Event) o m (Maybe Miscellaneous)
miscellaneous = CL.peek >>= \case
  Just (_, EventInstruction i) -> dropReturn $ Just $ MiscInstruction i
  Just (_, EventComment t) -> dropReturn $ Just $ MiscComment t
  Just (_, EventContent (ContentText t)) | T.all Char.isSpace t -> CL.drop 1 >> miscellaneous
  _ -> return Nothing

doctype :: MonadThrow m => ConduitM (Maybe PositionRange, Event) o m (Maybe Doctype)
doctype = CL.peek >>= \case
  Just (_, EventBeginDoctype name meid) -> do
    CL.drop 1
    dropTillDoctype
    return (Just $ Doctype name meid)
  _ -> return Nothing
  where
    dropTillDoctype = do
      x <- CL.head
      case x of
        -- Leaving the following line commented so that the intention of
        -- this function stays clear. I figure in the future xml-types will
        -- be expanded again to support some form of EventDeclaration
        --
        -- Just (EventDeclaration _) -> dropTillDoctype
        Just (_, EventEndDoctype) -> return ()
        Just epos -> lift $ monadThrow $ InvalidInlineDoctype epos
        Nothing -> lift $ monadThrow UnterminatedInlineDoctype

miscellaneousList :: MonadThrow m => ConduitM (t, Event) o m [Miscellaneous]
miscellaneousList = manyTries miscellaneous

prologue :: MonadThrow m => ConduitM (Maybe PositionRange, Event) o m Prologue
prologue = Prologue <$> miscellaneousList <*> doctype <*> miscellaneousList

skip :: Monad m => Event -> ConduitM (a, Event) o m ()
skip e = do
  x <- CL.peek
  when (fmap snd x == Just e) (CL.drop 1)

require
  :: MonadThrow m
  => ConduitM (Maybe PositionRange, Event) o m (Maybe b)
  -> ConduitM (Maybe PositionRange, Event) o m b
require f = f >>= \case
  Just y -> return y
  Nothing -> CL.head >>= \case
    Nothing -> error "Text.XML.Unresolved:impossible"
    Just (_, EventEndDocument) -> lift $ monadThrow MissingRootElement
    Just y -> lift $ monadThrow $ ContentAfterRoot y

-- | Parse a document from a stream of events.
ourFromEvents :: MonadThrow m => Consumer P.EventPos m Document
ourFromEvents = do
  skip EventBeginDocument
  d <- Document <$> prologue <*> require elementFromEvents <*> miscellaneousList
  skip EventEndDocument
  y <- CL.head
  case y of
    Nothing -> return d
    Just (_, EventEndDocument) -> lift $ monadThrow MissingRootElement
    Just z -> lift $ monadThrow $ ContentAfterRoot z
