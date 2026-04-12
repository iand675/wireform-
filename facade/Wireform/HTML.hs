-- | Convenience re-exports for HTML5 parsing and serialization.
module Wireform.HTML
  ( module HTML.Value
  , module HTML.Parse
  , module HTML.Encode
  , module HTML.Class
  , module HTML.DOM
  ) where

import HTML.Value
import HTML.Parse
import HTML.Encode
import HTML.Class
import HTML.DOM hiding (textContent)
