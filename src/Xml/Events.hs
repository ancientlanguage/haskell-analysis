{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Xml.Events where

import Prelude hiding (log)
import Conduit
import Control.Exception (Exception (..), try)
import Control.Monad (when)
import qualified Data.Char as Char
import qualified Data.Conduit.List as CL
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import Data.XML.Types hiding (Node(..), Element(..), Content)
import qualified Data.XML.Types as XML
import Text.XML.Stream.Parse (def,  PositionRange)
import qualified Text.XML.Stream.Parse as P
import Xml.PositionTypes

data InvalidXml
  = ContentAfterRoot P.EventPos
  | MissingRootElement
  | InvalidInlineDoctype P.EventPos
  | MissingEndElement Name (Maybe P.EventPos)
  | UnterminatedInlineDoctype
  | InternalErrorRequire
  | InvalidEntity Text PositionRange
  deriving (Show, Typeable)
instance Exception InvalidXml

readEvents :: FilePath -> IO [P.EventPos]
readEvents path = runResourceT $ sourceFile path =$= P.parseBytesPos def $$ sinkList

readRootElement :: (Element -> IO ()) -> FilePath -> IO (Either InvalidXml Element)
readRootElement log path = try $ runResourceT $ sourceFile path =$= P.parseBytesPos def $$ rootElement log

many :: Monad m => m (Maybe a) -> m [a]
many f = go id
  where
  go g = f >>= \case
    Nothing -> return $ g []
    Just x -> go $ g . (x :)

dropReturn :: Monad m => a -> ConduitM i o m a
dropReturn x = CL.drop 1 >> return x

miscellaneous :: Monad m => ConduitM (t, Event) o m (Maybe Miscellaneous)
miscellaneous = CL.peek >>= \case
  Just (_, EventInstruction i) -> dropReturn . Just . MiscInstruction $ i
  Just (_, EventComment t) -> dropReturn . Just . MiscComment $ t
  Just (_, EventContent (ContentText t)) | Text.all Char.isSpace t -> CL.drop 1 >> miscellaneous
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
compressNodes (NodeContent x : NodeContent y : z) =
  compressNodes $ NodeContent (mappend x y) : z
compressNodes (x : xs) = x : compressNodes xs

convertEntity :: MonadThrow m => Text -> PositionRange -> m Content
convertEntity t p = Content <$> tryConvert t <*> pure [p]
  where
  tryConvert = \case
    -- Standard XML entities
    "quot" -> pure "\x0022"
    "amp" -> pure "\x0026"
    "apos" -> pure "\x0027"
    "lt" -> pure "\x003c"
    "gt" -> pure "\x003e"

    -- ISO 8879 entities 
    "lpar" -> pure "\x0028"
    "rpar" -> pure "\x0029"
    "ast" -> pure "\x002a"
    "dash" -> pure "\x2010"
    t' -> throwM $ InvalidEntity t' p

convertAttributes :: MonadThrow m => PositionRange -> [(Name, [XML.Content])] -> m [(Name, Text)]
convertAttributes p as = mapM convert as
  where
  convert (n, cs) = value cs >>= pure . (,) n
  value cs = mconcat <$> mapM valuePart cs
  valuePart (XML.ContentText t) = pure t
  valuePart (XML.ContentEntity e) = contentText <$> convertEntity e p

node :: (MonadThrow m, MonadIO m) => (Element -> IO ()) -> ConduitM (Maybe PositionRange, Event) o m (Maybe Node)
node log = CL.peek >>= \case
  Just ((Just p), EventBeginElement n as) -> (Just . NodeElement) <$> finishElement log p n as
  Just ((Just p), EventContent c) -> case c of
    ContentEntity e -> do
      e' <- convertEntity e p
      dropReturn . Just . NodeContent $ e'
    ContentText t -> dropReturn . Just . NodeContent $ Content t [p] 
  Just (_, EventInstruction _) -> CL.drop 1 >> node log
  Just (_, EventComment _) -> CL.drop 1 >> node log
  Just ((Just p), EventCDATA t) -> dropReturn . Just $ NodeContent (Content t [p])
  _ -> return Nothing

finishElement
  :: (MonadThrow m, MonadIO m)
  => (Element -> IO ())
  -> PositionRange
  -> Name
  -> [(Name, [XML.Content])]
  -> ConduitM (Maybe PositionRange, Event) o m Element
finishElement log p n as = do
  CL.drop 1
  as' <- convertAttributes p as
  ns <- many (node log)
  CL.head >>= \case
    Just (Just p', EventEndElement n') | n == n' -> do
      let e = Element n as' (compressNodes ns) (p, p')
      liftIO $ log e
      return e 
    x -> throwM $ MissingEndElement n x 

element :: (MonadThrow m, MonadIO m) => (Element -> IO ()) -> Consumer P.EventPos m (Maybe Element)
element log = CL.peek >>= \case
  Just ((Just p), EventBeginElement n as) -> Just <$> finishElement log p n as
  _ -> return Nothing

document :: (MonadThrow m, MonadIO m) => (Element -> IO ()) -> Consumer P.EventPos m (Prologue, Element, [Miscellaneous])
document log = do
  skip EventBeginDocument
  d <- (,,) <$> prologue <*> require (element log) <*> miscellaneousList
  skip EventEndDocument
  CL.head >>= \case
    Nothing -> return d
    Just (_, EventEndDocument) -> throwM MissingRootElement
    Just x -> throwM $ ContentAfterRoot x

rootElement :: (MonadThrow m, MonadIO m) => (Element -> IO ()) -> Consumer P.EventPos m Element
rootElement log = do
  (_, r, _) <- document log
  return r
