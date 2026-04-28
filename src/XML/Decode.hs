-- | DOM parser — builds an XML tree from SAX events.
--
-- Uses 'XML.SAX.parseSAX' internally and folds the event stream into
-- a 'Document' via an element stack.
module XML.Decode
  ( decode
  , decodeText
  , decodeWith
  , decodeTextWith
  , ParseConfig(..)
  , defaultParseConfig
  , Document
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V

import XML.Value
import XML.SAX (SAXEvent(..), parseSAX, parseSAXWith, SAXConfig(..))

data ParseConfig = ParseConfig
  { pcMaxDepth :: !Int
  , pcMaxEntityExpansion :: !Int
  , pcAllowDTD :: !Bool
  , pcAllowExternalEntities :: !Bool
  }

defaultParseConfig :: ParseConfig
defaultParseConfig = ParseConfig
  { pcMaxDepth = 1000
  , pcMaxEntityExpansion = 10000
  , pcAllowDTD = True
  , pcAllowExternalEntities = False
  }

toSAXConfig :: ParseConfig -> SAXConfig
toSAXConfig pc = SAXConfig
  { saxMaxEntityExpansion = pcMaxEntityExpansion pc
  , saxAllowDTD = pcAllowDTD pc
  , saxAllowExternalEntities = pcAllowExternalEntities pc
  }

-- | Parse XML bytes into a DOM Document.
decode :: ByteString -> Either String Document
decode = decodeWith defaultParseConfig

-- | Parse XML Text into a DOM Document.
decodeText :: Text -> Either String Document
decodeText = decodeTextWith defaultParseConfig

-- | Parse XML bytes with explicit config.
decodeWith :: ParseConfig -> ByteString -> Either String Document
decodeWith cfg bs = do
  events <- parseSAXWith (toSAXConfig cfg) bs
  buildDocument (pcMaxDepth cfg) events

-- | Parse XML Text with explicit config.
decodeTextWith :: ParseConfig -> Text -> Either String Document
decodeTextWith cfg = decodeWith cfg . TE.encodeUtf8

data BuildState = BuildState
  { bsStack :: ![StackFrame]
  , bsDecl :: !(Maybe XMLDecl)
  , bsRoot :: !(Maybe Node)
  , bsDepth :: !Int
  }

data StackFrame = StackFrame
  { sfName :: !Name
  , sfAttrs :: !(Vector Attribute)
  , sfChildren :: ![Node]
  }

buildDocument :: Int -> Vector SAXEvent -> Either String Document
buildDocument maxDepth events = go (BuildState [] Nothing Nothing 0) 0
  where
    !len = V.length events
    go !st !i
      | i >= len =
          case bsRoot st of
            Nothing -> Left "No root element found"
            Just root -> Right (Document (bsDecl st) root)
      | otherwise =
          case events V.! i of
            StartDocument mDecl ->
              go (st { bsDecl = mDecl }) (i + 1)
            EndDocument ->
              go st (i + 1)
            StartElement name attrs ->
              let !newDepth = bsDepth st + 1
              in if newDepth > maxDepth
                   then Left $ "Maximum nesting depth exceeded (" ++ show maxDepth ++ ")"
                   else let !frame = StackFrame name attrs []
                        in go (st { bsStack = frame : bsStack st, bsDepth = newDepth }) (i + 1)
            EndElement _name ->
              case bsStack st of
                [] -> Left "EndElement without matching StartElement"
                (frame : rest) ->
                  let !node = Element (sfName frame) (sfAttrs frame)
                                (V.fromList (reverse (sfChildren frame)))
                  in case rest of
                    [] -> go (st { bsStack = [], bsRoot = Just node, bsDepth = 0 }) (i + 1)
                    (parent : grandparents) ->
                      let !parent' = parent { sfChildren = node : sfChildren parent }
                      in go (st { bsStack = parent' : grandparents, bsDepth = bsDepth st - 1 }) (i + 1)
            Characters txt ->
              case bsStack st of
                [] -> go st (i + 1)
                (frame : rest) ->
                  let !frame' = frame { sfChildren = Text txt : sfChildren frame }
                  in go (st { bsStack = frame' : rest }) (i + 1)
            CDATASection txt ->
              case bsStack st of
                [] -> go st (i + 1)
                (frame : rest) ->
                  let !frame' = frame { sfChildren = CData txt : sfChildren frame }
                  in go (st { bsStack = frame' : rest }) (i + 1)
            CommentEvent txt ->
              case bsStack st of
                [] -> go st (i + 1)
                (frame : rest) ->
                  let !frame' = frame { sfChildren = Comment txt : sfChildren frame }
                  in go (st { bsStack = frame' : rest }) (i + 1)
            PI target content ->
              case bsStack st of
                [] -> go st (i + 1)
                (frame : rest) ->
                  let !frame' = frame { sfChildren = ProcessingInstruction target content : sfChildren frame }
                  in go (st { bsStack = frame' : rest }) (i + 1)
