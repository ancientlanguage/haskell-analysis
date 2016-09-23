{-# LANGUAGE FlexibleContexts #-}

module Xml.Events where

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
import Text.XML.Stream.Parse (def, ParseSettings, PositionRange)
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

readRootElement :: FilePath -> IO (Either InvalidXml Element)
readRootElement path = try $ runResourceT $ sourceFile path =$= P.parseBytesPos def $$ rootElement

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
    "&quot;" -> pure "\x0022"
    "&amp;" -> pure "\x0026"
    "&apos;" -> pure "\x0027"
    "&lt;" -> pure "\x003c"
    "&gt;" -> pure "\x003e"
    t -> throwM $ InvalidEntity t p

convertAttributes :: MonadThrow m => PositionRange -> [(Name, [XML.Content])] -> m [(Name, Text)]
convertAttributes p as = mapM convert as
  where
  convert (n, cs) = value cs >>= pure . (,) n
  value cs = mconcat <$> mapM valuePart cs
  valuePart (XML.ContentText t) = pure t
  valuePart (XML.ContentEntity e) = contentText <$> convertEntity e p

node :: MonadThrow m => ConduitM (Maybe PositionRange, Event) o m (Maybe Node)
node = CL.peek >>= \case
  Just ((Just p), EventBeginElement n as) -> (Just . NodeElement) <$> finishElement p n as
  Just ((Just p), EventContent c) -> case c of
    ContentEntity e -> do
      e' <- convertEntity e p
      dropReturn . Just . NodeContent $ e'
    ContentText t -> dropReturn . Just . NodeContent $ Content t [p] 
  Just (_, EventInstruction i) -> CL.drop 1 >> node
  Just (_, EventComment t) -> CL.drop 1 >> node
  Just ((Just p), EventCDATA t) -> dropReturn . Just $ NodeContent (Content t [p])
  _ -> return Nothing

finishElement
  :: MonadThrow m
  => PositionRange
  -> Name
  -> [(Name, [XML.Content])]
  -> ConduitM (Maybe PositionRange, Event) o m Element
finishElement p n as = do
  CL.drop 1
  as' <- convertAttributes p as
  ns <- many node
  CL.head >>= \case
    Just (Just p', EventEndElement n) -> return $ Element n as' (compressNodes ns) (p, p')
    x -> throwM $ MissingEndElement n x 

element :: MonadThrow m => Consumer P.EventPos m (Maybe Element)
element = CL.peek >>= \case
  Just ((Just p), EventBeginElement n as) -> Just <$> finishElement p n as
  _ -> return Nothing

document :: MonadThrow m => Consumer P.EventPos m (Prologue, Element, [Miscellaneous])
document = do
  skip EventBeginDocument
  d <- (,,) <$> prologue <*> require element <*> miscellaneousList
  skip EventEndDocument
  CL.head >>= \case
    Nothing -> return d
    Just (_, EventEndDocument) -> throwM MissingRootElement
    Just x -> throwM $ ContentAfterRoot x

rootElement :: MonadThrow m => Consumer P.EventPos m Element
rootElement = (\(_, r, _) -> r) <$> document
