-- | Convenience re-exports for HTML5 parsing and serialization.
--
-- Both @HTML.Parse@ and @HTML.Encode@ define a function called
-- @buildDocument@ — the parser's internal tree-builder finalizer and the
-- encoder's @Document -> Builder@ respectively. Only the encoder one is
-- useful at the umbrella API level, so we hide the parser's name on
-- import to avoid a re-export conflict.
module Wireform.HTML
  ( module HTML.Value
  , module HTML.Parse
  , module HTML.Encode
  , module HTML.Class
  , module HTML.DOM
  ) where

import HTML.Value
import HTML.Parse hiding (buildDocument)
import HTML.Encode
import HTML.Class
import HTML.DOM hiding (textContent)
