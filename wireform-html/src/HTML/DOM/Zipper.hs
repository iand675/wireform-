{-# LANGUAGE BangPatterns #-}
-- | Zipper-style navigation over 'HTMLNode' (internal to @HTML.DOM@).
module HTML.DOM.Zipper
  ( Crumb(..)
  , Node(..)
  , NodeType(..)
  , rootNode
  , childNodes
  , firstChild
  , lastChild
  , nextSibling
  , prevSibling
  , parentNode
  , nodeName
  , nodeType
  , rawNode
  , textContent
  , tagName
  , getAttribute
  , getAttributes
  , hasAttribute
  , classList
  ) where

import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Primitive.SmallArray
  ( SmallArray, sizeofSmallArray, indexSmallArray
  )

import HTML.Value (HTMLNode(..), HTMLAttribute(..))
import qualified HTML.Value as Value

-- | Zipper context: one frame per ancestor on the path from root to current node.
data Crumb = Crumb
  !Text                        -- parent tag
  !(SmallArray HTMLAttribute)  -- parent attributes
  !(SmallArray HTMLNode)       -- parent's children
  {-# UNPACK #-} !Int          -- this node's index among siblings

-- | A node in the document tree with navigation context.
--
-- Two 'Node' values may refer to the same underlying 'HTMLNode'
-- but have different crumbs (i.e. were reached via different paths).
-- The crumb list is intentionally lazy (~): index-backed queries
-- (querySelectorAllDoc) store a thunk for the crumb chain, avoiding
-- allocation entirely when the caller only inspects tag/attrs.
-- Zipper-built crumbs are already WHNF so the ~ has no effect there.
data Node = Node !HTMLNode ~[Crumb]

-- | Wrap a raw 'HTMLNode' as a root 'Node' with no parent context.
rootNode :: HTMLNode -> Node
rootNode raw = Node raw []
{-# INLINE rootNode #-}

instance Show Node where
  show (Node raw _) = "Node (" ++ show raw ++ ")"

instance Eq Node where
  Node a _ == Node b _ = a == b

data NodeType
  = ElementNode
  | TextNode
  | CommentNode
  | DocumentNode
  | DocumentTypeNode
  | DocumentFragmentNode
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | All direct children of an element node, each carrying full
-- navigation context. Non-element nodes return @[]@.
childNodes :: Node -> [Node]
childNodes (Node (HTMLElement tag attrs children) crumbs) =
  let !n = sizeofSmallArray children
      go !i
        | i >= n    = []
        | otherwise =
            Node (indexSmallArray children i)
                 (Crumb tag attrs children i : crumbs)
              : go (i + 1)
  in go 0
childNodes _ = []

-- | The first child, if any.
firstChild :: Node -> Maybe Node
firstChild (Node (HTMLElement tag attrs children) crumbs)
  | sizeofSmallArray children > 0 =
      Just $! Node (indexSmallArray children 0)
                   (Crumb tag attrs children 0 : crumbs)
firstChild _ = Nothing

-- | The last child, if any.
lastChild :: Node -> Maybe Node
lastChild (Node (HTMLElement tag attrs children) crumbs)
  | let !n = sizeofSmallArray children, n > 0 =
      let !i = n - 1
      in Just $! Node (indexSmallArray children i)
                      (Crumb tag attrs children i : crumbs)
lastChild _ = Nothing

-- | The next sibling in the parent's child list.
nextSibling :: Node -> Maybe Node
nextSibling (Node _ []) = Nothing
nextSibling (Node _ (Crumb tag attrs children idx : rest)) =
  let !next = idx + 1
  in if next < sizeofSmallArray children
     then Just $! Node (indexSmallArray children next)
                       (Crumb tag attrs children next : rest)
     else Nothing

-- | The previous sibling in the parent's child list.
prevSibling :: Node -> Maybe Node
prevSibling (Node _ []) = Nothing
prevSibling (Node _ (Crumb _ _ _ idx : _))
  | idx <= 0 = Nothing
prevSibling (Node _ (Crumb tag attrs children idx : rest)) =
  let !prev = idx - 1
  in Just $! Node (indexSmallArray children prev)
                  (Crumb tag attrs children prev : rest)

-- | Navigate to the parent node. Returns 'Nothing' at the root.
parentNode :: Node -> Maybe Node
parentNode (Node _ []) = Nothing
parentNode (Node _ (Crumb tag attrs children _ : rest)) =
  Just $! Node (HTMLElement tag attrs children) rest

-- | The node name: tag name for elements, @\"#text\"@ for text, etc.
nodeName :: Node -> Text
nodeName (Node raw _) = case raw of
  HTMLElement tag _ _ -> tag
  HTMLText _          -> "#text"
  HTMLComment _       -> "#comment"
  HTMLDoctype n _ _   -> n

-- | The DOM node type.
nodeType :: Node -> NodeType
nodeType (Node raw _) = case raw of
  HTMLElement {}  -> ElementNode
  HTMLText {}     -> TextNode
  HTMLComment {}  -> CommentNode
  HTMLDoctype {}  -> DocumentTypeNode

-- | The underlying 'HTMLNode'. Useful for pattern matching on the raw tree.
rawNode :: Node -> HTMLNode
rawNode (Node raw _) = raw

-- | Recursive text content (concatenated text of all descendant text nodes).
textContent :: Node -> Text
textContent (Node raw _) = Value.textContent raw

-- | The tag name for element nodes, 'Nothing' otherwise.
tagName :: Node -> Maybe Text
tagName (Node (HTMLElement tag _ _) _) = Just tag
tagName _ = Nothing

-- | Look up an attribute value by name.
getAttribute :: Node -> Text -> Maybe Text
getAttribute (Node raw _) name = Value.getAttr name raw

-- | All attributes of an element node as a list.
getAttributes :: Node -> [HTMLAttribute]
getAttributes (Node (HTMLElement _ attrs _) _) = toList attrs
getAttributes _ = []

-- | Check whether an attribute exists on this node.
hasAttribute :: Node -> Text -> Bool
hasAttribute node name = case getAttribute node name of
  Just _  -> True
  Nothing -> False

-- | Split the @class@ attribute into individual class names.
classList :: Node -> [Text]
classList node = maybe [] T.words (getAttribute node "class")
