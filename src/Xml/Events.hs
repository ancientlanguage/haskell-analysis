{-# LANGUAGE FlexibleContexts #-}

module Xml.Events where

import Conduit
import Control.Exception (Exception (..), try, SomeException)
import Control.Monad (when)
import Control.Monad.Trans.Resource
import qualified Data.Char as Char
import Data.Conduit.Attoparsec (PositionRange)
import qualified Data.Conduit.Internal as CI
import qualified Data.Conduit.List as CL
import qualified Data.Text as Text
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.XML.Types
import Text.XML.Stream.Parse (def, ParseSettings)
import qualified Text.XML.Stream.Parse as P

data InvalidXml
  = ContentAfterRoot P.EventPos
  | MissingRootElement
  | InvalidInlineDoctype P.EventPos
  | MissingEndElement Name (Maybe P.EventPos)
  | UnterminatedInlineDoctype
  | InternalErrorRequire
  deriving (Show, Typeable)
instance Exception InvalidXml

readEvents :: FilePath -> IO [P.EventPos]
readEvents path = runResourceT $ sourceFile path =$= P.parseBytesPos def $$ sinkList

readDocument :: FilePath -> IO (Either InvalidXml Document)
readDocument path = try $ runResourceT $ sourceFile path =$= P.parseBytesPos def $$ document

many :: Monad m => m (Maybe a) -> m [a]
many f = go id
  where
  go g = f >>= \case
    Nothing -> return $ g []
    Just y -> go $ g . (y :)

dropReturn :: Monad m => a -> ConduitM i o m a
dropReturn x = CL.drop 1 >> return x

miscellaneous :: Monad m => ConduitM (t, Event) o m (Maybe Miscellaneous)
miscellaneous = CL.peek >>= \case
  Just (_, EventInstruction i) -> dropReturn . Just . MiscInstruction $ i
  Just (_, EventComment t) -> dropReturn . Just . MiscComment $ t
  Just (_, EventContent (ContentText t)) | T.all Char.isSpace t -> CL.drop 1 >> miscellaneous
  _ -> return Nothing

doctype :: MonadThrow m => ConduitM (Maybe PositionRange, Event) o m (Maybe Doctype)
doctype = CL.peek >>= \case
  Just (_, EventBeginDoctype name meid) -> do
    CL.drop 1
    finishDoctype
    return (Just $ Doctype name meid)
  _ -> return Nothing

finishDoctype :: MonadThrow m => ConduitM (Maybe PositionRange, Event) o m ()
finishDoctype = CL.head >>= \case
  Just (_, EventEndDoctype) -> return ()
  Just x -> throwM $ InvalidInlineDoctype x
  Nothing -> throwM UnterminatedInlineDoctype

miscellaneousList :: MonadThrow m => ConduitM (t, Event) o m [Miscellaneous]
miscellaneousList = many miscellaneous

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
  Just x -> return x
  Nothing -> CL.head >>= \case
    Nothing -> throwM InternalErrorRequire
    Just (_, EventEndDocument) -> throwM MissingRootElement
    Just x -> throwM $ ContentAfterRoot x

compressNodes :: [Node] -> [Node]
compressNodes [] = []
compressNodes [x] = [x]
compressNodes (NodeContent (ContentText x) : NodeContent (ContentText y) : z) =
  compressNodes $ NodeContent (ContentText $ x `T.append` y) : z
compressNodes (x : xs) = x : compressNodes xs

node :: MonadThrow m => ConduitM (Maybe PositionRange, Event) o m (Maybe Node)
node = CL.peek >>= \case
  Just (_, EventBeginElement n as) -> (Just . NodeElement) <$> finishElement n as
  Just (_, EventInstruction i) -> dropReturn . Just $ NodeInstruction i
  Just (_, EventContent c) -> dropReturn . Just $ NodeContent c
  Just (_, EventComment t) -> dropReturn . Just $ NodeComment t
  Just (_, EventCDATA t) -> dropReturn . Just $ NodeContent (ContentText t)
  _ -> return Nothing

finishElement
  :: MonadThrow m
  => Name
  -> [(Name, [Content])]
  -> ConduitM (Maybe PositionRange, Event) o m Element
finishElement n as = do
  CL.drop 1
  ns <- many node
  x <- CL.head
  if fmap snd x == Just (EventEndElement n)
    then return . Element n as $ compressNodes ns
    else throwM $ MissingEndElement n x

element :: MonadThrow m => Consumer P.EventPos m (Maybe Element)
element = CL.peek >>= \case
  Just (_, EventBeginElement n as) -> Just <$> finishElement n as
  _ -> return Nothing

document :: MonadThrow m => Consumer P.EventPos m Document
document = do
  skip EventBeginDocument
  d <- Document <$> prologue <*> require element <*> miscellaneousList
  skip EventEndDocument
  CL.head >>= \case
    Nothing -> return d
    Just (_, EventEndDocument) -> throwM MissingRootElement
    Just x -> throwM $ ContentAfterRoot x
